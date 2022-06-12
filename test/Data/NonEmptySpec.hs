module Data.NonEmptySpec
  ( main,
    spec,
  )
where

import Data.NonEmpty
import Data.Proxy
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "Singleton creation should be equivalent to a single element list" $
    singleton (Proxy @[Int]) 42 `shouldBe` trustedNonEmpty [42]
  it "nonEmpty creation on filled list should be equivalent to the wrapped list" $
    nonEmpty @[Int] [1, 2, 3] `shouldBe` Just (trustedNonEmpty [1, 2, 3])
  it "nonEmpty creation on empty list should be Nothing" $
    nonEmpty @[Int] [] `shouldBe` Nothing
