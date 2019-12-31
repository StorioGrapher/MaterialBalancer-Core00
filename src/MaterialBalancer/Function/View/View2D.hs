module MaterialBalancer.Function.View.View2D where


import qualified Data.IntMap                   as I
import           Data.Text                      ( Text )
import qualified Data.Vector                   as V

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis
import           MaterialBalancer.Data.Column  as C
import           MaterialBalancer.Data.ValueStorage
import           MaterialBalancer.Data.TheTable
import           MaterialBalancer.Data.View

buildDefaultView :: ID -> AxisIndex -> TheTable -> View2D
buildDefaultView vID baseAI theTable@(am, csm, _) = View2D
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

-- Modifying View
-- * Set baseColumns
-- * Modify viewSettings

-- When type of baseColumns2D is simple list, we may not refer `TheTable` for running the `setBaseColumns`
setBaseColumns :: [ColumnIndex] -> TheTable -> View2D -> View2D
setBaseColumns cIndices _ view = view { baseColumns2D = cIndices }

setBaseColumns' :: [ColumnIndex] -> TheTable -> View2D -> View2D
setBaseColumns' cIndices (am, csm, _) view@View2D {..} = if check
  then view { baseColumns2D = cIndices }
  else
    error
    $  "[ERROR]<setBaseColumns'> some columnIndices are out of bound "
    ++ (show . filter (not . checkBound) $ cIndices)
 where
  check        = checkCIBound
  ciBound      = I.size . getColumnMap baseAxis2D $ csm
  checkCIBound = all checkBound cIndices
  checkBound x = x >= 0 && x < ciBound
