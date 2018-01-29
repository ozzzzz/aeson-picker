{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Aeson.Picker.Internal.Functions
  ( (|--)
  , (|-?)
  ) where

import           Control.Lens    (Traversal', (^?))
import           Data.Aeson      (FromJSON (..), Result (..), Value, fromJSON)
import           Data.Aeson.Lens (AsValue, key, _Value)
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)

{- |
From given JSON and selectors returns typed field. If input JSON is not valid or selected field is not found then error is thrown.
If you need more safe way use '(|-?)' instead. Examples:

-- >>> "5" |-- [] :: Int
-- 5
-- >>> "5" |-- [] :: Float
-- 5.0
-- >>> "5" |-- [] :: String
-- *** Exception: Data.Aeson.Picker: could not pick field with path: []
--
-- >>> :set -XOverloadedStrings
-- >>> "{\"a\": 5}" |-- ["a"] :: Int
-- 5
-- >>> "{\"a\": 5}" |-- ["b"] :: Int
-- *** Exception: Data.Aeson.Picker: could not pick field with path: ["b"]
-- >>> "{\"outer\": {\"inner\": [1,2,3]}}" |-- ["outer", "inner"] :: [Int]
-- [1,2,3]
-- >>> {\"outer\": {\"inner\": [1,2,3]}}" |-- ["outer", "inner"] :: [Double]
-- [1.0,2.0,3.0]
-- >>> "{a: 5}" |-- ["a"] :: Int
-- *** Exception: Data.Aeson.Picker: input json is not valid
-}
infix 5 |--
(|--) :: (AsValue t, FromJSON a) => t -> [Text] -> a
json |-- selectors = fromMaybe (error $ "Data.Aeson.Picker: could not pick field with path: " ++ show selectors) $ json |-? selectors

{- |
From given JSON and selectors returns typed field inside 'Maybe'. If input JSON is not valid then error is thrown.
Examples:

-- >>> "5" |-? [] :: Maybe Int
-- Just 5
-- >>> "5" |-? [] :: Maybe String
-- Nothing
-- >>> "{\"a\": 5}" |-? ["a"] :: Maybe Int
-- Just 5
-- >>> "{a: 5}" |-? ["a"] :: Maybe Int
-- *** Exception: Data.Aeson.Picker: input json is not valid
-}
infix 5 |-?
(|-?) :: (AsValue t, FromJSON a) => t -> [Text] -> Maybe a
json |-? selectors = let validJSON = checkValidity json
                     in pick validJSON selectors >>= convert

-- | Checks validity for JSON format. Throws error if it is not valid.
checkValidity :: AsValue t => t -> t
checkValidity json = fromMaybe (error "Data.Aeson.Picker: input json is not valid") (json ^? _Value) `seq` json

-- | Picks from given JSON selected field
pick :: AsValue t => t -> [Text] -> Maybe Value
pick json []        = json ^? _Value
pick json selectors = json ^? genGetter selectors

-- | Converts from 'Value'
convert :: FromJSON a => Value -> Maybe a
convert value = case fromJSON value of
  Success r -> Just r
  Error _   -> Nothing

-- | Generates getter from given selectors
genGetter :: AsValue t => [Text] -> Traversal' t Value
genGetter []     = error "Data.Aeson.Picker.Internal.Functions.genGetter: this should not be happened"
genGetter [x]    = key x
genGetter (x:xs) = key x . genGetter xs


