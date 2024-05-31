{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key as K
import Data.Traversable
import Data.Foldable
