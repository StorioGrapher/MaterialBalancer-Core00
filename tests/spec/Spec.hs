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

import qualified Test.MaterialBalancer.Data.Axis as Axis
import qualified Test.MaterialBalancer.Data.Column as Column
import qualified Test.MaterialBalancer.Data.TheTable as TheTable
import qualified Test.MaterialBalancer.Data.ValueStorage as ValueStorage
import qualified Test.MaterialBalancer.Data.View as DView
import qualified Test.MaterialBalancer.Data.ViewResult as DViewResult
import qualified Test.MaterialBalancer.Function.View.View2D as FView2D
import qualified Test.MaterialBalancer.Function.ViewResult as FViewResult


main :: IO ()
main = do
  putStrLn "Process Spec Test"
  Test.defaultMain
    [ Test.MaterialBalancer.Data.Axis.tests
    , Test.MaterialBalancer.Data.Column.tests
    , Test.MaterialBalancer.Data.ValueStorage.tests
    , Test.MaterialBalancer.Data.TheTable.tests
    ]
