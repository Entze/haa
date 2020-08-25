
import Test.Hspec
import Test.QuickCheck
--import Control.Exception (evaluate)
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import AxisAndAlliesLibrary
import Data.Ratio

main :: IO ()
main = hspec $ do
  describe "Mathematical Misc" $ do
    describe "binomial function" $ do
      it "should return 252 on 10 `binomial` 5" $ do
        ((10 :: Int) `binomial` (5 :: Int)) `shouldBe` 252
      it "should return 6 on 4 `binomial` 2" $ do
        ((4 :: Int) `binomial` (2 :: Int)) `shouldBe` 6
      it "should return 17383860 on 27 `binomial` 15" $ do
        ((27 :: Int) `binomial` (15 :: Int)) `shouldBe` 17383860
      it "should return 21 on 21 `binomial` 1" $ do
        ((21 :: Int) `binomial` (1 :: Int)) `shouldBe` 21
      it "should return 0 on 1 `binomial` 21" $ do
        ((1 :: Int) `binomial` (21 :: Int)) `shouldBe` 0
      it "returns values according to the definition" $
        property $ prop_binomial_equals_definition
      it "returns values according to the recursive definition" $
        property $ prop_binomial_equals_recursive_definition
    describe "binomialDistributionOfDiceThrows" $ do
      it "returns probabilities, s.t. the sum is one" $
        property $ prop_binomialDistributionOfDiceThrows_is_one
  describe "Axis and Allies" $ do
      modifyMaxSuccess (const 15) $ it "returns probabilities, s.t. the sum is one" $
        property $ prop_probabilityOfHitsWith_sum_is_one


prop_binomial_equals_definition :: (Positive Integer) -> (Positive Integer) -> Bool
prop_binomial_equals_definition (Positive n) (Positive k) = expected == actual
  where
    expected = (product [1..n]) `div` (product [1..k] * product [1..(n-k)])
    actual = n `binomial` k


prop_binomial_equals_recursive_definition :: (Positive Integer) -> (Positive Integer) -> Bool
prop_binomial_equals_recursive_definition (Positive n) (Positive k) = binomial n k == binomial (n-1) (k-1) + binomial (n-1) k

prop_probabilityOfHitsWith_sum_is_one :: (Positive Int) -> (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
prop_probabilityOfHitsWith_sum_is_one (Positive ones) (Positive twos) (Positive threes) (Positive fours) = sum (probabilityOfHitsWith ones twos threes fours) == ((1 % 1) :: Rational)

prop_binomialDistributionOfDiceThrows_is_one :: (Positive Int) -> (Positive Int) -> Bool
prop_binomialDistributionOfDiceThrows_is_one (Positive a) (Positive f) = (1 % 1) == sum (map ((flip ((flip binomialDistributionOfDiceThrows) amount)) face) [0..amount])
  where
    amount = a
    face = (f `rem` 4) + 1
