{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Backend where

import Common.Api
import Common.Route
import Common.Governance
import Network.WebSockets.Client
import Network.WebSockets.Connection
import Obelisk.Route hiding (encode, decode)
import Obelisk.Backend
import System.Which
import System.Process
import Data.Bool
import Data.Maybe
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Aeson qualified as Aeson
import Data.Aeson (object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Traversable
import Data.Foldable
import System.Directory
import GHC.IO.Handle
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Simple
import Network.HTTP.Conduit

import Types

type ProcessContext = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data State = State
  { state_parties :: TVar (Map Text Party)
  , balances :: TVar (Map Text Balance)
  , utxos :: TVar (Map Text Utxos)
  , pendingTransactions :: TVar (Map Text (Maybe Transaction))
  , hydraNetwork :: TMVar (Map Text ProcessContext)
  , sync :: TVar Float
  , state_governance :: TVar (Maybe Governance)
  , wants_vote :: TVar (Set Text)
  , wants_prop :: TVar Bool
  , state_transactions :: TVar [SubmittedTransaction]
  }

sanchonetMagic :: String
sanchonetMagic = "4"

getTip :: IO (Maybe Tip)
getTip = do
  result <- try $ readCreateProcess (proc "cardano-cli" ["query", "tip", "--testnet-magic", sanchonetMagic, "--socket-path", socketPath]) ""
  case result of
    Left (_ :: IOError) -> pure $ Nothing
    Right output ->
      pure $ Aeson.decode $ LBS.fromStrict $ BS.pack output

loadParty :: Text -> IO Party
loadParty name = do
  -- TODO(skylar): This should create the party if it isn't there!
  let
    hydraPrefix = "keys/" <> name <> "-hydra"
    hydraVk = "keys/" <> name <> "-hydra.vk"
    hydraSk = "keys/" <> name <> "-hydra.sk"

    drepVk = "keys/" <> name <> "-drep.vk"
    drepSk = "keys/" <> name <> "-drep.sk"

    stakeVk = "keys/" <> name <> "-stake.vk"
    stakeSk = "keys/" <> name <> "-stake.sk"

  addr <- T.readFile $ T.unpack $ "keys/" <> name <> ".addr"
  hydraVkExists <- doesFileExist $ T.unpack hydraVk
  when (not hydraVkExists) $ do
    _ <- runReadProcess $ proc "hydra-node" ["gen-hydra-key", "--output-file", T.unpack hydraPrefix]
    pure ()
  Just drepKeyHash <- runReadProcess (proc "cardano-cli" ["conway", "address", "key-hash", "--payment-verification-key-file", T.unpack drepVk])
  pure $ Party name (T.strip addr) ("keys/" <> name <> ".sk") ("keys/" <> name <> ".vk") stakeSk stakeVk drepSk drepVk drepKeyHash hydraSk hydraVk Nothing

createVoteFile :: Party -> ActionId -> IO Text
createVoteFile p (ActionId hash ix) = do
  removeFileIfExists voteFile
  _ <- runReadProcess (proc "cardano-cli"
                   [ "conway"
                   , "governance"
                   , "vote"
                   , "create"
                   , "--yes"
                   , "--governance-action-tx-id"
                   , T.unpack hash
                   , "--governance-action-index"
                   , show ix
                   , "--drep-verification-key-file"
                   , T.unpack $ party_drep_verification_key p
                   , "--out-file"
                   , voteFile
                   ])
  pure $ T.pack voteFile
  where
    voteFile = "temp.vote"

getParty :: State -> Text -> IO (Maybe Party)
getParty state name = do
  partyMap <- readTVarIO $ state_parties state
  pure $ Map.lookup name partyMap

getGovernance :: IO (Maybe Governance)
getGovernance = runMaybeT $ do
  output <- MaybeT $ runReadProcess (proc "cardano-cli"
                                      [ "conway", "query", "gov-state"
                                      , "--testnet-magic", sanchonetMagic
                                      , "--socket-path", socketPath
                                      ])
  gov <- MaybeT $ pure $ Aeson.decode $ LBS.fromStrict $ BS.pack $ T.unpack output
  tip <- MaybeT $ getTip
  let
    -- Find a proposal we can vote on
    expired = Map.fromList $ fmap (\x -> (x, ())) $ expiredGovActions $ nextRatifyState $ gov
    props =
      Map.fromList $ fmap (\x -> (proposal_id x, x)) $ fmap buildProposal $ proposals gov
    currentProp = headMay $ drop 1 $ fmap buildProposal $ sortBy (\x y -> compare (proposedIn y) (proposedIn x)) $ proposals gov

  pure $ Governance currentProp (govActionDeposit . currentPParams $ gov)
  where
    buildProposal gp =
      Proposal
      (actionId gp)
      (Set.fromList $ fmap (T.drop (T.length "keyHash-")) $ Map.keys . unObjectMap . dRepVotes $ gp)
      (url . anchor $ proposalProcedure gp)

getState :: IO State
getState = do
  gov <- loadParty "governance"
  a   <- loadParty "votera"
  b   <- loadParty "voterb"
  c   <- loadParty "voterc"
  parties <- newTVarIO $ Map.fromList $ fmap (\p -> (party_name p, p)) [gov, a, b, c]
  balances <- newTVarIO mempty
  utxos <- newTVarIO mempty
  pending <- newTVarIO mempty
  governance <- newTVarIO Nothing
  hydraNetwork <- newEmptyTMVarIO
  wantsVotes <- newTVarIO mempty
  wantsProp <- newTVarIO False
  transactions <- newTVarIO mempty
  sync <- newTVarIO 0
  return $ State parties balances utxos pending hydraNetwork sync governance wantsVotes wantsProp transactions

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists fp = do
  exits <- doesDirectoryExist fp
  when exits $ removeDirectoryRecursive fp

getHydraUtxos :: IO (Maybe Utxos)
getHydraUtxos = do
  result <- try $ httpJSON "http://127.0.0.1:5001/snapshot/utxo"
  pure $ case result of
    Left (_ :: SomeException) -> Nothing
    Right res -> Just $ responseBody res

getHydraNetwork :: State -> IO ()
getHydraNetwork state = do
  isEmpty <- atomically $ isEmptyTMVar (hydraNetwork state)
  case isEmpty of
    True -> do
      putStrLn "Setting up Hydra Nodes"
      ps <- readTVarIO $ state_parties state
      let
        partiesAndIds = zip [1..] $ Map.elems ps
      results <- for partiesAndIds $ \(partyId, party) -> do
        let
          notUs = Map.filter (\x -> party_name x /= party_name party) ps
          persistenceDir = T.unpack (party_name party) <> "-hydra-node"
        removeDirIfExists persistenceDir
        wholeProcess <- createProcess $ (proc "hydra-node" $ [
          "offline"
          , "--node-id"
          , show partyId
          , "--ledger-protocol-parameters"
          , "protocol-parameters.json"
          , "--port"
          , show $ 8080 + partyId
          , "--persistence-dir"
          , persistenceDir
          , "--testnet-magic"
          , "4"
          , "--node-socket"
          , socketPath
          , "--api-host"
          , "0.0.0.0"
          , "--api-port"
          , show $ 5000 + partyId
          , "--cardano-signing-key"
          , T.unpack $ party_signing_key party
          , "--hydra-signing-key"
          , T.unpack $ party_hydra_signing_key party
          ] <> (mconcat $ fmap (\x -> [ "--hydra-verification-key", T.unpack $ party_hydra_verification_key party
                                      , "--cardano-verification-key", T.unpack $ party_verification_key party
                                      ]) $ Map.elems notUs)) { std_in = CreatePipe
                                                             , std_out = CreatePipe
                                                             , std_err = CreatePipe
                                                             }
        pure (party_name party, wholeProcess)
      putStrLn "Hydra Network set up"
      atomically $ putTMVar (hydraNetwork state) $ Map.fromList results
    False -> do
      pure ()

updateSync :: State -> Tip -> IO Bool
updateSync state (Tip percentage _ _) = do
  let
    syncPercent = read $ T.unpack percentage
  atomically $ writeTVar (sync state) syncPercent
  putStrLn $ "Sync percent " <> show syncPercent
  pure $ syncPercent > 99

utxosToBalance :: Utxos -> Balance
utxosToBalance (Utxos utxos) =
  Balance $ Map.foldr (Map.unionWith (+) . unBalance . value) mempty utxos

runReadProcess :: CreateProcess -> IO (Maybe Text)
runReadProcess cp = do
  result <- try $ readCreateProcess cp ""
  case result of
    Left (_ :: IOError) -> pure Nothing
    Right output -> pure $ Just $ T.strip $ T.pack output

getUtxos :: Text -> IO Utxos
getUtxos address = do
  result <- try $ readCreateProcess (proc "cardano-cli" [
                                         "query"
                                        , "utxo"
                                        , "--address"
                                        , T.unpack address
                                        , "--socket-path"
                                        , socketPath
                                        , "--testnet-magic"
                                        , sanchonetMagic
                                        , "--output-json"
                                        ]) ""
  case result of
    Left (_ :: IOError) ->
      pure mempty
    Right output -> do
      pure $ maybe mempty id $ Aeson.decode $ LBS.fromStrict $ BS.pack $ T.unpack $ T.strip $ T.pack output

hydraScriptTxFile :: FilePath
hydraScriptTxFile = "hydra-tx.id"

sanchonetHydraScriptsTxId :: Text
sanchonetHydraScriptsTxId = "af37f4f6bf7459d2ae1d6b2a1a2e4049465b62a8ebc308f3d6d6af68240a4419"

updateProtocolParameters :: IO ()
updateProtocolParameters = do
  _ <- runReadProcess (proc "cardano-cli" [ "query"
                                          , "protocol-parameters"
                                          , "--testnet-magic"
                                          , "4"
                                          , "--socket-path"
                                          , socketPath
                                          , "--out-file"
                                          , "protocol-parameters.json"
                                          ])
  putStrLn "Updated protocol parameters"
  pure ()

getWitnessCount :: Transaction -> Integer
getWitnessCount t =
  1
  + maybe 0 (const 1) (transaction_certificate_file t)
  + maybe 0 (const 1) (transaction_vote_file t)

headMay :: [a] -> Maybe a
headMay (x: _) = Just x
headMay _ = Nothing

tailMay :: [a] -> Maybe a
tailMay [] = Nothing
tailMay (a: []) = Just a
tailMay (a: rest) = tailMay rest

coinSelectInput :: Text -> Integer -> IO (Maybe Text)
coinSelectInput addr amount = do
  Utxos utxos <- getUtxos addr
  let
    filtered = Map.filter (\o -> utxoBalance o >= amount) utxos
    mUtxo = headMay $ Map.keys filtered
  pure mUtxo
  where
    utxoBalance (Utxo _ v) = getBalance "lovelace" v

buildTransactionArgs :: Party -> Text -> Transaction -> [String]
buildTransactionArgs p input t =
  baseInput <> voteInput <> certificateInput <> proposalInput <> witnessInput <> toInput
  where
    baseInput =
      [ "conway"
      , "transaction"
      , "build"
      , "--change-address"
      , T.unpack fromAddr
      , "--testnet-magic"
      , "4"
      , "--socket-path"
      , socketPath
      , "--out-file"
      , "tx.raw"
      , "--tx-in"
      , T.unpack input
      ]

    voteInput = maybe [] (\x -> ["--vote-file", T.unpack x]) $ transaction_vote_file t
    certificateInput = maybe [] (\x -> ["--ceritficate-file", T.unpack x]) $ transaction_certificate_file t
    proposalInput = maybe [] (\x -> ["--proposal-file", T.unpack x]) $ transaction_proposal_file t
    witnessInput = ["--witness-override", show witnessCount]
    toInput = maybe [] (\x ->  ["--tx-out", T.unpack x <> "+" <> show (transaction_lovelace t)]) $ transaction_to t

    fromAddr = party_address p
    witnessCount = getWitnessCount t

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = do
  exists <- doesFileExist fp
  when exists $ removeFile fp

sendIncentivePayment :: Party -> Text -> IO ()
sendIncentivePayment p addr = do
  runMaybeT $ do
    utxos <- MaybeT $ getHydraUtxos
    let
      ours = filter (\(k, x) -> address x == party_address p) $ Map.toList $ unUtxos utxos
    input <- MaybeT $ pure $ headMay ours
    txid <- MaybeT $ buildTx p input addr rewardAmount
    signedFile <- liftIO $ LBS.readFile "signed.json"
    (TransactionJSON t d c) <- MaybeT $ pure $ Aeson.decode signedFile
    let
      x = object ["tag" .= ("NewTx" :: Text), "transaction" .= object ["type" .= t, "description" .= d, "cborHex" .= c]]
      encoded = T.decodeUtf8 $ LBS.toStrict $ Aeson.encode x
    liftIO $ runClient "127.0.0.1" 5001 "/?history=no" $ \conn -> do
      putStrLn $ "This happened"
      sendTextData conn encoded
    liftIO $ putStrLn "This happened"
    pure ()
  pure ()

buildTx :: Party -> (Text, Utxo) -> Text -> Integer -> IO (Maybe Text)
buildTx p (input, utxo) addr amount = runMaybeT $ do
  liftIO $ do
    removeFileIfExists "tx.json"
    removeFileIfExists "signed.json"

  liftIO $ putStrLn $ "Building raw"
  let
    b = getBalance "lovelace" (value utxo)

    fee = 167000
  MaybeT $ runReadProcess (proc "cardano-cli" [ "babbage", "transaction", "build-raw"
                                              , "--tx-in", T.unpack input
                                              , "--tx-out", T.unpack $ addr <> "+" <> tShow amount
                                              , "--tx-out", T.unpack $ (party_address p) <> "+" <> tShow (b - amount - fee)
                                              , "--fee", show fee
                                              , "--out-file", "tx.json"
                                              ])

  liftIO $ putStrLn $ "Signing"
  MaybeT $ runReadProcess (proc "cardano-cli" $
                        [ "transaction", "sign"
                        , "--tx-body-file", "tx.json"
                        , "--out-file", "signed.json"
                        , "--signing-key-file", T.unpack $ party_signing_key p
                        ]
                       )

  liftIO $ putStrLn $ "Getting hash"
  txid <- MaybeT $ runReadProcess (proc "cardano-cli"
                                    [ "babbage", "transaction", "txid"
                                    , "--tx-body-file", "signed.json"
                                    ])
  pure txid

buildAndSubmit :: Party -> Transaction -> IO (Maybe SubmittedTransaction)
buildAndSubmit p t = runMaybeT $ do
  input <- MaybeT $ coinSelectInput fromAddr $ transaction_lovelace t
  liftIO $ do
    removeFileIfExists "tx.signed"
    removeFileIfExists "tx.raw"
  let
    args = buildTransactionArgs p input t
    includeDrep = (isJust $ transaction_certificate_file t) || (isJust $ transaction_vote_file t)

  MaybeT $ runReadProcess (proc "cardano-cli" args)
  MaybeT $ runReadProcess (proc "cardano-cli" $
                        [ "conway", "transaction", "sign"
                        , "--tx-body-file", "tx.raw"
                        , "--testnet-magic", "4"
                        , "--out-file", "tx.signed"
                        , "--signing-key-file", T.unpack $ party_signing_key p
                        ] <> bool [] ["--signing-key-file", T.unpack $ party_drep_signing_key p] includeDrep
                       )

  txid <- MaybeT $ runReadProcess (proc "cardano-cli"
                                    [ "conway", "transaction", "txid"
                                    , "--tx-body-file", "tx.signed"
                                    ])
  MaybeT $ runReadProcess (proc "cardano-cli"
                       [ "conway", "transaction", "submit"
                       , "--tx-file", "tx.signed"
                       , "--socket-path"
                       , socketPath
                       , "--testnet-magic"
                       , "4"
                       ])
  pure $ SubmittedTransaction txid input t
  where
    fromAddr = party_address p
    witnessCount = getWitnessCount t

adjustParty :: State -> Text -> (Party -> Party) -> IO ()
adjustParty state name f = atomically $ do
  parties <- readTVar (state_parties state)
  writeTVar (state_parties state) $ Map.adjust f name parties

submitAs :: State -> Text -> Transaction -> IO Bool
submitAs state name transaction = do
  parties <- readTVarIO (state_parties state)
  let
    Just party = Map.lookup name parties
  case party_pending_transaction party of
    Just (SubmittedTransaction hash _ _) -> do
      T.putStrLn $ "Waiting on transaction: " <> hash
      pure False
    Nothing -> do
      mSubmit <- buildAndSubmit party transaction
      case mSubmit of
        Just submit -> do
          let
            newParty =
              party { party_pending_transaction = Just submit }
          atomically $ do
            writeTVar (state_parties state) $ Map.insert name newParty parties
            ts <- readTVar (state_transactions state)
            writeTVar (state_transactions state) $ submit : ts
        Nothing -> putStrLn "Failed to submit transaction"
      pure True

transactionComplete :: Text -> IO Bool
transactionComplete hash =
  isJust <$> getUtxoFromChain hash 0

getUtxoFromChain :: Text -> Integer -> IO (Maybe Utxo)
getUtxoFromChain hash index = do
  result <- runReadProcess (proc "cardano-cli"
                   [ "query"
                   , "utxo"
                   , "--tx-in", T.unpack outref
                   , "--testnet-magic", "4"
                   , "--output-json"
                   , "--socket-path"
                   , socketPath
                   ])
  pure $ Map.lookup outref =<< Aeson.decode . LBS.fromStrict . BS.pack . T.unpack =<< result
  where
    outref = hash <> "#" <> tShow index

updateState :: State -> IO ()
updateState state = do
  -- Update governance state
  gov <- getGovernance
  atomically $ writeTVar (state_governance state) gov

  ps <- readTVarIO $ state_parties state

  -- Update transactions!
  ps' <- for ps $ \p -> do
    case party_pending_transaction p of
      Just st -> do
        isObserved <- transactionComplete $ submittedTransaction_hash st
        case isObserved of
          True -> pure $ p { party_pending_transaction = Nothing }
          False -> pure p

      Nothing -> pure p
  atomically $ writeTVar (state_parties state) ps'

  -- Update Balances
  balanceList <- for (Map.elems ps) $ \p -> do
    balance <- fmap utxosToBalance . getUtxos $ party_address p
    pure $ (party_name p, balance)
  atomically $ writeTVar (balances state) $ Map.fromList balanceList

  -- Ensure protocol paramters
  updateProtocolParameters

createGovAction :: Governance -> Party -> IO Text
createGovAction gov p = do
  removeFileIfExists actionFile
  runReadProcess (proc "cardano-cli"
                   [ "conway", "governance", "action", "create-info"
                   , "--testnet"
                   , "--governance-action-deposit", show $ governance_actionDeposit gov
                   , "--deposit-return-stake-verification-key-file", T.unpack $ party_stake_verification_key p
                   , "--anchor-url", "https://tinyurl.com/yc74fxx4"
                   , "--anchor-data-hash", "931f1d8cdfdc82050bd2baadfe384df8bf99b00e36cb12bfb8795beab3ac7fe5"
                   , "--out-file", actionFile
                   ])
  pure $ T.pack actionFile
  where
    actionFile = "info.action"

-- Check if they have min lovelace
-- Check if they are all drep
-- Check if they are all in a head
-- Check if there is a proposal
-- Send vote transactions
-- Check if those are sent
-- When we respond to them
-- We can give them the reward
updateThread :: State -> IO ()
updateThread state = forever $ do
  putStrLn "Updating"
  mtip <- getTip
  case mtip of
    Nothing -> do
      putStrLn "No tip"
    Just tip -> do
      inSync <- updateSync state tip
      when inSync $ do
        updateState state

        view <- getStateView state
        actions <- for (stateView_parties view) $ \(PartyView p b) -> do
          case party_name p == "governance" of
            True -> pure Nothing
            False -> do
              let
                lovelace = getBalance "lovelace" b
              case lovelace < minLovelace of
                True -> pure $ Just $ Fund (minLovelace - lovelace)
                False -> pure Nothing

        getHydraNetwork state
        parties <- readTVarIO $ state_parties state
        let
          partyActions = Map.intersectionWith (\p a -> (p, a)) parties $ Map.mapMaybe id actions
          Just governance = Map.lookup "governance" parties
        for partyActions $ \(party, action) -> do
          case action of
            Fund amount -> do
              let
                t = simpleTransaction (party_address party) amount
              putStrLn "Funding"
              submitAs state "governance" t
              pure ()
          pure ()


        -- Process props
        mGov <- readTVarIO $ state_governance state
        case mGov of
          Nothing -> pure ()
          Just gov -> do
            wantsProp <- readTVarIO $ wants_prop state
            case wantsProp of
              True -> do
                atomically $ writeTVar (wants_prop state) False
                govFile <- createGovAction gov governance
                submitAs state "governance" $ govTransaction gov govFile
                pure ()
              False -> do
                pure ()

        -- Process votes
        wants <- readTVarIO $ wants_vote state
        mProp <- readTVarIO $ state_governance state
        case mProp >>= governance_currentProposal of
          Nothing -> pure ()
          Just prop -> do
            putStrLn "Have proposal, checking"
            parties <- readTVarIO $ state_parties state
            for_ wants $ \name -> do
              let
                mP = Map.lookup name parties
              case mP of
                Nothing -> do
                  T.putStrLn $ "Failed to find" <> name <> " in " <> (tShow $ Map.keys parties)
                  pure ()
                Just p -> do
                  T.putStrLn $ "Found thing: " <> name
                  createVoteFile p (proposal_id prop)
                  submitted <- submitAs state (party_name p) $ voteTransaction "temp.vote"
                  when submitted $ do
                    atomically $ do
                      votes' <- readTVar (wants_vote state)
                      writeTVar (wants_vote state) $ Set.delete (party_name p) votes'
                    sendIncentivePayment governance (party_address p)
              pure ()
        pure ()
        -- Register as a drep if we haven't
        -- TODO(skylar): We will assume if a cert isn't there you aren't a drep
        -- because you can't easily check for that, yay!
      pure ()
  threadDelay oneSecond
  pure ()
  where
    oneSecond = 10000000

withUpdateThread :: State -> IO a -> IO a
withUpdateThread state action =
  bracket (forkIO $ updateThread state) (killThread) $ \_ -> action

socketPath :: String
socketPath = "sancho-db/node.socket"

withState :: (State -> IO a) -> IO a
withState = do
  bracket (getState) (teardownHydraNetwork)

teardownHydraNetwork :: State -> IO ()
teardownHydraNetwork state = do
  network <- atomically $ readTMVar $ hydraNetwork state
  for_ (Map.elems network) cleanupProcess
  putStrLn "Hydra network teardown complete"

getStateView :: State -> IO StateView
getStateView state = do
  ps <- readTVarIO (state_parties state)
  balances <- readTVarIO (balances state)
  governance <- readTVarIO (state_governance state)
  ts <- readTVarIO (state_transactions state)
  pure $ StateView (Map.intersectionWith PartyView ps balances) (maybe (Governance Nothing 0) id governance) ts

getStateApi :: MonadSnap m => State -> m ()
getStateApi state = do
  view <- liftIO $ getStateView state
  writeLBS $ Aeson.encode view

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      let
        node_proc = (proc "cardano-node" [
           "run"
          , "--topology"
          , "sanchotest/topology.json"
          , "--database-path"
          , "sancho-db/"
          , "--socket-path"
          , socketPath
          , "--host-addr"
          , "0.0.0.0"
          , "--port"
          , "3001"
          , "--config"
          , "sanchotest/config.json"
          ])
        piped_node_proc = node_proc
          { std_in  = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
      withState $ \state -> do
        withCreateProcess piped_node_proc $ \_ _ _ _ -> do
          withUpdateThread state $ do
            serve $ \case
              BackendRoute_State :/ () -> getStateApi state
              BackendRoute_Prop :/ () -> do
                liftIO $ atomically $ writeTVar (wants_prop state) True
              BackendRoute_Vote :/ () -> do
                name <- fmap (T.decodeUtf8 . LBS.toStrict) $ readRequestBody 2048
                let
                  votes = wants_vote state
                liftIO $ do
                  T.putStrLn $ "Requests vote: " <> name
                  atomically $ do
                    vs <- readTVar votes
                    writeTVar votes $ Set.insert (T.dropAround (== '\"') $ T.strip name) vs
                pure ()
              _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }
