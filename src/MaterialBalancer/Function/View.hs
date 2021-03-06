module MaterialBalancer.Function.View where


import qualified Data.IntMap                   as I
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis
import           MaterialBalancer.Data.Column  as C
import           MaterialBalancer.Data.ValueStorage
import           MaterialBalancer.Data.TheTable
import           MaterialBalancer.Data.View

buildDefaultView2D :: ID -> AxisIndex -> TheTable -> View2D
buildDefaultView2D vID baseAI theTable@(am, csm, vs) = View2D
  vID
  baseAI
  [0 .. baseColumnNum - 1]
  targetAI
  [theViewGroup]
 where
  is2D     = 2 == I.size am
  targetAI = if is2D
    then 1 - baseAI
    else error "[ERROR]<buildDefaultView2D> Given TheTable is not 2D"
  baseColumnNum   = I.size . C.getColumnMap baseAI $ csm
  targetColumnNum = I.size . C.getColumnMap targetAI $ csm
  theViewGroup =
    V2DGroup Nothing . map (\ci -> (Nothing, ci)) $ [0 .. targetColumnNum - 1]
