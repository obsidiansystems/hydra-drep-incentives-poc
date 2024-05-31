{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Frontend where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Data.Map (Map)
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Text (Text)
import Data.Text qualified as T

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Hydra DRep Incentives POC"
      elAttr "script" ( "src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = elClass "div" "w-screen h-screen bg-gray-100 p-4 text-gray-700" $ do
      elClass "h1" "font-bold text-3xl text-center mb-4"$ text "Hydra DRep Incentives POC"
      prerender_ blank $ do
        pb <- getPostBuild
        tick <- tickLossyFromPostBuildTime 5
        result <- getAndDecode $ "/state" <$ leftmost [() <$ tick, pb]
        dMState <- holdDyn (Nothing) $ ffor (fmapMaybe id result) $ \sv ->
          case Map.null $ stateView_parties sv of
            True -> Nothing
            False -> Just sv
        dMState' <- maybeDyn dMState
        dyn_ $ ffor dMState' $ \case
          Nothing -> elClass "div" "text-center text-3xl text-green-500" $ text "Loading..."
          Just dState -> do
            let
              parties = stateView_parties <$> dState
              newParties = updated parties
              newPartyEvent = (fmap . fmap) Just newParties

              governance = Map.lookup "governance" <$> parties

              -- The current proposal if any
              currentProposal = governance_currentProposal . stateView_governance <$> dState

            dyn_ $ ffor governance $ \case
              Nothing -> blank
              Just gov -> do
                elClass "div" "text-gray-800 font-semibold text-xs p-4 bg-gray-300 border-1 mb-2 border-gray-400" $ do
                  elClass "div" "text-green-700" $ text "governance: "
                  elClass "div" "" $ text $ party_address . partyView_party $ gov
                  let
                    lovelace = tShow . getBalance "lovelace" . partyView_balance $ gov
                  elClass "div" "" $ text $ lovelace <> " lovelace"

            dyn_ $ ffor currentProposal $ \mPp -> do
              case mPp of
                Just pp -> do
                  elClass "div" "p-4 bg-white rounded-lg drop-shadow" $ do
                    elClass "div" "text-sm font-gray-600" $ text "Current Proposal"
                    elClass "div" "" $ text $ renderActionId . proposal_id $ pp
                    elAttr "a" ("href" =: proposal_url pp <> "target" =: "_blank" <> "class" =: "text-xs text-green-500") $ text "View Proposal"
                Nothing -> do
                  pure ()
            (e, _) <- elClass "div" "flex flex-col w-full items-center " $ do
              r <- elClass' "button" "bg-green-500 text-white font-bold px-4 py-2 rounded-lg drop-shadow mt-4" $ text "Create New Proposal"
              elClass "div" "text-gray-500 font-semibold text-xs mt-2" $ text "You need at least 50000000000 lovelace"
              pure r
            _ <- performRequestAsync $ postJson "/prop" () <$ domEvent Click e
            elClass "div" "p-4 bg-white rounded-lg drop-shadow mt-4" $ do
              listWithKey parties $ \k v -> when (k /= "governance") $ do
                let
                  drepHash = party_drep_keyhash . partyView_party <$> v
                  hasVoted = ffor2 drepHash currentProposal $ \kh prop ->
                    maybe False (Set.member kh . proposal_voters) prop

                  lovelace = tShow . getBalance "lovelace" . partyView_balance <$> v
                elClass "div" "flex flex-row items-center justify-between mt-2" $ do
                  elClass "div" "font-bold text-lg" $ text k
                  elClass "div" "font-semibold text-green-800 text-lg" $ dynText $ lovelace <> " lovelace"
                  elClass "div" "" $ dyn_ $ ffor hasVoted $ \case
                    True -> elClass "div" "text-green-300 font-bold px-4 py-2" $ text "Voted"
                    False -> do
                      (e, _) <- elClass' "button" "bg-green-500 text-white font-bold px-4 py-2 rounded-lg drop-shadow" $ text "vote"
                      _ <- performRequestAsync $ postJson "/vote" k <$ domEvent Click e
                      pure ()
            elClass "div" "p-4 bg-white rounded-lg drop-shadow mt-4" $ do
              elClass "div" "text-lg font-semibold" $ text "Transactions"
              simpleList (stateView_submitted <$> dState) $ \st -> do
                elClass "div" "flex flex-row items-center justify-between" $ do
                  elClass "div" "font-bold text-lg" $ dynText $ submittedTransaction_hash <$> st
              pure ()
      return ()
  }
