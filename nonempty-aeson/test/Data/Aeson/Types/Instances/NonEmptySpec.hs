module Data.Aeson.Types.Instances.NonEmptySpec
  ( main,
    spec,
  )
where

import Data.Aeson
import Data.Aeson.Types.Instances.NonEmpty ()
import qualified Data.Map.Strict as M
import Data.NonEmpty
import Data.Proxy
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decode" $ do
    describe "regular element" $ do
      it "valid case should parse" $
        decode "[42]" `shouldBe` Just (singleton (Proxy @[Int]) 42)
      it "empty case should fail" $
        decode "[]" `shouldBe` Nothing @(NonEmpty String)
    describe "key" $ do
      it "valid case should parse" $
        decode "{\"a\":42}" `shouldBe` Just (M.singleton (singleton (Proxy @String) 'a') (42 :: Int))
      it "empty case should fail" $
        decode "{\"\":42}" `shouldBe` Nothing @(M.Map (NonEmpty String) Int)

  describe "encode" $ do
    it "list of Int" $
      encode (singleton (Proxy @[Int]) 42) `shouldBe` "[42]"
    it "keymap" $
      encode (M.singleton (singleton (Proxy @String) 'a') (42 :: Int)) `shouldBe` "{\"a\":42}"
