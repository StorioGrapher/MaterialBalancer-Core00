import qualified System.IO.Silently as Silently

import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.HUnit
import Test.QuickCheck

import Test.MaterialBalancer.Data.Axis
import Test.MaterialBalancer.Data.Column
import Test.MaterialBalancer.Data.TheTable
import Test.MaterialBalancer.Data.ValueStorage


main :: IO ()
main = do
  putStrLn "Test MB Data modules"
  Test.defaultMain
    [ Test.MaterialBalancer.Data.Axis.tests
    , Test.MaterialBalancer.Data.Column.tests
    , Test.MaterialBalancer.Data.ValueStorage.tests
    , Test.MaterialBalancer.Data.TheTable.tests
    ]
  putStrLn "Test CLI modules"
  Test.defaultMain
    [
    ]
