module Test.MaterialBalancer.Data.TestSet where


import           Test.Helper
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import           Test.Framework.TH
import           Test.HUnit.Base

import           MaterialBalancer.Data.Primitive
import qualified MaterialBalancer.Data.Axis    as A
import qualified MaterialBalancer.Data.Column  as C
import qualified MaterialBalancer.Data.ValueStorage
                                               as S
import           MaterialBalancer.Data.TheTable


theTable00 = blankTable

theTable3D00 = addAxis "Z-axis" theTable00
addedColumn3D00 = addColumn 0 "Column X-1" theTable3D00
addedColumn3D00' = addColumn 1 "Column Y-1" addedColumn3D00
addedColumn3D00'' = addColumn 1 "Column Y-2" addedColumn3D00'
addedColumn3D00x = addColumn 0 "Column X-2" addedColumn3D00
addedColumn3D00z' = addColumn 2 "Column Z-1" addedColumn3D00
addedColumn3D00z'' = addColumn 2 "Column Z-1" addedColumn3D00z'

addedAxis01 =
  addColumn 1 "Column Y-2"
    . addColumn 1 "Column Y-1"
    . addColumn 0 "Column X-2"
    . addColumn 0 "Column X-1"
    $ theTable00
addedAxis01' =
  addColumn 1 "Column Y-2"
    . addColumn 0 "Column X-2"
    . addColumn 1 "Column Y-1"
    . addColumn 0 "Column X-1"
    $ theTable00

table2D23 =
  addColumn 1 "Column Y-2"
    . addColumn 1 "Column Y-1"
    . addColumn 0 "Column X-1"
    $ theTable00
table2D23' =
  addColumn 1 "Column Y-2"
    . addColumn 1 "Column Y-1"
    . addColumn 0 "Column X-1"
    $ theTable00

table2D23WithData =
  setVariable [(0,0),(1,0)] (Just "ABC")
    . addColumn 1 "Column Y-2"
    . addColumn 1 "Column Y-1"
    . addColumn 0 "Column X-1"
    $ theTable00
--   =TEST=   Test integrated features
--  ==TEST==  Test add a column
