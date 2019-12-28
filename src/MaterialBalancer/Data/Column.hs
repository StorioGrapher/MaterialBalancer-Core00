module MaterialBalancer.Data.Column where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe


type ColumnsMap = IntMap ColumnMap
type ColumnIndex = Int
type ColumnName = Name
type ColumnMap = IntMap ColumnName


getColumnMap :: AxisIndex -> ColumnsMap -> ColumnMap
getColumnMap idx columnsMap = fromJust $ I.lookup idx columnsMap

getColumnMap' :: AxisIndex -> ColumnsMap -> ColumnMap
getColumnMap' idx columnsMap = if isJust mColumnMap
  then getColumnMap idx columnsMap
  else error $ "[ERROR]<getColumnMap'> No such ColumnMap like " ++ show idx
  where mColumnMap = I.lookup idx columnsMap

addAxis :: ColumnsMap -> (AxisIndex, ColumnsMap)
addAxis columnsMap = (newIdx, I.insert newIdx I.empty columnsMap)
  where newIdx = (I.size columnsMap)

addColumnIn :: ColumnName -> ColumnMap -> (ColumnIndex, ColumnMap)
addColumnIn name cm = (idx, I.insert idx name cm) where idx = I.size cm

addColumnIn' :: ColumnName -> ColumnMap -> (ColumnIndex, ColumnMap)
addColumnIn' name cm = if isExist
  then
    error
      "[ERROR]<addColumnIn'> Auto-generated Column idx is duplicated with another exist"
  else addColumnIn name cm
 where
  idx     = I.size cm
  isExist = isJust (I.lookup idx cm)

deleteColumnIn :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumnIn idx cm = (deletedName, I.mapKeys fixer cm)
 where
  deletedName = fromJust $ I.lookup idx cm
  fixer prev = if prev > idx then prev - 1 else prev

deleteColumnIn' :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumnIn' idx cm = if isJust (I.lookup idx cm)
  then deleteColumnIn idx cm
  else error $ "[ERROR]<deleteColumn'> No such Column like " ++ show idx

changeColumnNameIn :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnNameIn = I.insert

changeColumnNameIn' :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnNameIn' idx name cm = if isJust (I.lookup idx cm)
  then changeColumnNameIn idx name cm
  else error $ "[ERROR]<changeColumnName'> No such Column like " ++ show idx

-- TODO: reallocateColumn ::

-- TODO: mergeColumn ::
