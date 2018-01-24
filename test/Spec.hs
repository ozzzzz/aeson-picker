{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson.Picker
import           Data.Text         (Text, unpack)
import           Test.Hspec

exampleJSON :: Text
exampleJSON = "{\"object-like\":{\"string-like\":\"this is string\", \"int-like\": 42}, \"float-like\":1.2}"

main :: IO ()
main = hspec $ do
    describe ("With JSON " ++ unpack exampleJSON) $ do
      it "|-- [\"float-like\"]" $
           (exampleJSON |-- ["float-like"]) `shouldBe` (1.2 :: Float)
      it "|-- [\"object-like\", \"string-like\"]" $
           (exampleJSON |-- ["object-like", "string-like"]) `shouldBe` ("this is string" :: String)
      it "|-- [\"object-like\", \"int-like\"]" $
           (exampleJSON |-- ["object-like", "int-like"]) `shouldBe` (42 :: Int)
      
     
      
     
