{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Governance where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key as K
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable

-- Newtype for parsing Object key value pairs as Maps
newtype ObjectMap a =
  ObjectMap { unObjectMap :: Map Text a }
  deriving (Eq, Show)

instance FromJSON a => FromJSON (ObjectMap a) where
  parseJSON = withObject "ObjectMap a" $ \km -> do
    ret <- for (KM.toList km) $ \(key, value) -> do
      parsedV <- parseJSON value
      pure (K.toText key, parsedV)
    pure $ ObjectMap $ Map.fromList ret

instance ToJSON a => ToJSON (ObjectMap a) where
  toJSON (ObjectMap m) =
    object $ fmap (\(k,v) -> K.fromText k .= v) $ Map.toList m

data GovernancePParams = GovernancePParams
  { dRepDeposit :: Integer
  , govActionDeposit :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON GovernancePParams
instance ToJSON GovernancePParams

data ActionId = ActionId
  { txId :: Text
  , govActionIx :: Integer
  }
  deriving (Eq, Show, Generic, Ord)

instance FromJSON ActionId
instance ToJSON ActionId

data ProposalProcedure = ProposalProcedure
  {
    anchor :: Anchor
  }
  deriving (Eq, Show, Generic)

instance FromJSON ProposalProcedure
instance ToJSON ProposalProcedure

data Anchor = Anchor
  {
    url :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Anchor
instance ToJSON Anchor

data GovernanceProposal = GovernanceProposal
  { actionId :: ActionId
  , dRepVotes :: ObjectMap Text
  , proposalProcedure :: ProposalProcedure
  , expiresAfter :: Integer
  , proposedIn :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON GovernanceProposal
instance ToJSON GovernanceProposal

data GovernanceState = GovernanceState
  { currentPParams :: GovernancePParams
  , proposals :: [GovernanceProposal]
  , nextRatifyState :: NextRatifyState
  }
  deriving (Eq, Show, Generic)

instance FromJSON GovernanceState
instance ToJSON GovernanceState

data NextRatifyState = NextRatifyState
  { expiredGovActions :: [ActionId]
  }
  deriving (Eq, Show, Generic)

instance FromJSON NextRatifyState
instance ToJSON NextRatifyState
