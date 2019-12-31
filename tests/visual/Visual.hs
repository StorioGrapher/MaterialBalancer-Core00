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
  putStrLn "Process Visual Test"
  -- Axis.visual
  -- Column.visual
  -- TheTable.visual
  -- ValueStorage.visual
  -- DView.visual
  -- DViewResult.visual
  FView2D.visual
  -- FViewResult.visual
