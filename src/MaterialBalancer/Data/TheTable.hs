-- Note: TheTable module provides abstract interface that operate Axis, Columns and ValueStorage with single function

module MaterialBalancer.Data.TheTable where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis     ( AxisIndex
                                                , AxisName
                                                , AxisMap
                                                )
import qualified MaterialBalancer.Data.Axis    as A
import           MaterialBalancer.Data.Column   ( ColumnsMap
                                                , ColumnIndex
                                                , ColumnName
                                                , ColumnMap
                                                )
import qualified MaterialBalancer.Data.Column  as C
import           MaterialBalancer.Data.ValueStorage
                                                ( Variable
                                                , ValueStorage
                                                , RMap(..)
                                                )
import qualified MaterialBalancer.Data.ValueStorage
                                               as S

import qualified Data.IntMap as I


type TheTable = (AxisMap, ColumnsMap, ValueStorage)

initialTheTable = (I.empty, I.empty, IM I.empty)

getAxisMap :: TheTable -> AxisMap
getAxisMap (am, _, _) = am

getColumnsMap :: TheTable -> ColumnsMap
getColumnsMap (_, csm, _) = csm

getValueStorage :: TheTable -> ValueStorage
getValueStorage (_, _, vs) = vs

{-
addColumnAt :: AxisIndex -> ColumnName -> TheTable -> (ColumnIndex, TheTable)
addColumnAt axisIdx columnName (am, csm, vs) = (columnIdx, (am, newCsM, newVS))
 where
  (columnIdx, newCsM) = C.addColumn axisIdx columnName csm
  newVS               = S.addColumn axisIdx vs

deleteColumnAt :: ColumnIndex -> AxisIndex -> TheTable -> TheTable
deleteColumnAt columnIdx axisIdx theTable = theTable
-}

addAxis :: AxisName -> TheTable -> TheTable
addAxis name (am, csm, vs) = (newAM, newCsM, newVS)
 where
  (_,newAM)  = A.addAxis name am
  (_,newCsM) = C.addAxis csm
  newVS  = S.addAxis vs

addColumn :: AxisIndex -> ColumnName -> TheTable -> TheTable
addColumn ai name (am, csm, vs) = (am, newCsM, newVS)
 where
  (ci, newCsM) = C.addColumn ai name csm
  newVS        = S.addColumn ai vs

setValue :: Key -> Variable -> TheTable -> TheTable
setValue key variable (am, csm, vs) = (am, csm, newVS)
  where newVS = S.setValue key variable vs
