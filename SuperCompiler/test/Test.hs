import Test.Tasty

import qualified Test.Generalization


main :: IO ()
main = do
  defaultMain (testGroup "All Tests"
                [ testGroup "Generalization" Test.Generalization.props
                ])