{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Aeson.Picker.Internal.Functions
  ( (|--)
  , (|-?)
  ) where

import           Control.Lens      (Traversal', (^?))
import           Data.Aeson        (FromJSON (..), Result (..), Value, fromJSON)
import           Data.Aeson.Lens   
import           Data.Maybe        (fromJust)
import           Data.Text         (Text)

infix 5 |--
(|--) :: (AsValue t, FromJSON a) => t -> [Text] -> a
json |-- []        = getFromValue . fromJust $ json ^? _Value
json |-- selectors = getFromValue . fromJust $ json ^? genGetter selectors

infix 5 |-?
(|-?) :: (AsValue t, FromJSON a) => t -> [Text] -> Maybe a
json |-? []        = (json ^? _Value)              >>= getFromValueM
json |-? selectors = (json ^? genGetter selectors) >>= getFromValueM

genGetter :: AsValue t => [Text] -> Traversal' t Value
genGetter []     = error "selector should has at least one field"
genGetter [x]    = key x
genGetter (x:xs) = key x . genGetter xs

getFromValue :: FromJSON a => Value -> a
getFromValue value = case fromJSON value of
  Success r -> r
  Error e   -> error e

getFromValueM :: FromJSON a => Value -> Maybe a
getFromValueM value = case fromJSON value of
  Success r -> Just r
  Error e   -> Nothing
