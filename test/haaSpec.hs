
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HCube.All

main :: IO ()
main = hspec $ do
  describe "Axis and Allies" $ do
