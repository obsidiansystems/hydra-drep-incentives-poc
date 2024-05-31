{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Api where

import Data.Aeson
import GHC.Generics

import Data.Text (Text)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key as K
import Data.Traversable
import Data.Foldable

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Text (Text)
import Data.Text qualified as T

import Common.Governance

rewardAmount :: Integer
rewardAmount = 12345678

minLovelace :: Integer
minLovelace = 550000000

data Tip = Tip
  { syncProgress :: Text
  , epoch :: Integer
  , block :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Tip

data Utxo = Utxo
  { address :: Text
  , value :: Balance
  }
  deriving (Eq, Show, Generic)

instance FromJSON Utxo where
  parseJSON = withObject "Utxo" $ \o -> do
    addr <- o .: "address"
    ObjectMap values <- o .: "value"
    pure $ Utxo addr $ Balance values

newtype Utxos = Utxos { unUtxos :: Map Text Utxo }
  deriving (Eq, Show)

instance Semigroup Utxos where
  (<>) (Utxos x) (Utxos y) = Utxos $ x <> y

instance Monoid Utxos where
  mempty = Utxos mempty

instance FromJSON Utxos where
  parseJSON value = do
    ObjectMap utxo <- parseJSON value
    pure $ Utxos utxo

newtype Balance = Balance
  { unBalance :: Map Text Integer }
  deriving(Eq, Show)

getBalance :: Text -> Balance -> Integer
getBalance k (Balance m) = maybe 0 id $ Map.lookup k m

tShow :: Show a => a -> Text
tShow = T.pack . show

data TransactionJSON = TransactionJSON
  { transactionJson_type :: Text
  , transactionJson_description :: Text
  , transactionJson_cborHex :: Text
  }

instance FromJSON TransactionJSON where
  parseJSON = withObject "Transaction JSON" $ \o -> do
    TransactionJSON <$> o .: "type" <*> o .: "description" <*> o .: "cborHex"

instance FromJSON Balance where
  parseJSON value = do
    ObjectMap utxo <- parseJSON value
    pure $ Balance utxo

instance ToJSON Balance where
  toJSON (Balance map) =
    toJSON map

data Party = Party
  { party_name :: Text
  , party_address :: Text
  , party_signing_key :: Text
  , party_verification_key :: Text
  , party_stake_signing_key :: Text
  , party_stake_verification_key :: Text
  , party_drep_signing_key :: Text
  , party_drep_verification_key :: Text
  , party_drep_keyhash :: Text
  , party_hydra_signing_key :: Text
  , party_hydra_verification_key :: Text
  , party_pending_transaction :: Maybe SubmittedTransaction
  }
  deriving (Eq, Show, Generic)

data Transaction = Transaction
  { transaction_to :: Maybe Text
  , transaction_lovelace :: Integer
  , transaction_certificate_file :: (Maybe Text)
  , transaction_vote_file :: (Maybe Text)
  , transaction_proposal_file :: (Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction

data SubmittedTransaction = SubmittedTransaction
  { submittedTransaction_hash :: Text
  , submittedTransaction_input :: Text
  , submittedTransaction_transaction :: Transaction
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubmittedTransaction
instance ToJSON SubmittedTransaction

minAda :: Integer
minAda = 2000000

simpleTransaction :: Text -> Integer -> Transaction
simpleTransaction toAddr amount =
  Transaction (Just toAddr) (max minAda amount) Nothing Nothing Nothing

voteTransaction :: Text -> Transaction
voteTransaction voteFile =
  Transaction Nothing minAda Nothing (Just voteFile) Nothing

govTransaction :: Governance -> Text -> Transaction
govTransaction gov govFile =
  Transaction Nothing (governance_actionDeposit gov) Nothing Nothing (Just govFile)

data Action = Fund Integer

data PartyView = PartyView
  { partyView_party :: Party
  , partyView_balance :: Balance
  -- , partyView_utxos :: Utxos
  }
  deriving (Eq, Show, Generic)

renderActionId :: ActionId -> Text
renderActionId (ActionId txid ix) = txid <> "#" <> tShow ix

data Proposal = Proposal
  { proposal_id :: ActionId
  , proposal_voters :: Set Text
  , proposal_url :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Proposal
instance ToJSON Proposal

data Governance = Governance
  { governance_currentProposal :: Maybe Proposal
  , governance_actionDeposit :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON Governance
instance ToJSON Governance

instance FromJSON PartyView
instance ToJSON PartyView

instance FromJSON Party
instance ToJSON Party

data StateView = StateView
  { stateView_parties :: Map Text PartyView
  , stateView_governance :: Governance
  , stateView_submitted :: [SubmittedTransaction]
  }
  deriving (Eq, Show, Generic)

instance FromJSON StateView
instance ToJSON StateView
