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
getColumnMap idx am = fromJust $ I.lookup idx am

getColumnMap' :: AxisIndex -> ColumnsMap -> ColumnMap
getColumnMap' idx am =
  if isJust mColumnMap
    then getColumnMap idx am
    else error $ "[ERROR]<getColumnMap'> No such ColumnMap like " ++ show idx
  where
    mColumnMap = I.lookup idx am

addColumn :: ColumnName -> ColumnMap -> (ColumnIndex, ColumnMap)
addColumn name am = (idx, I.insert idx name am)
  where idx = I.size am

addColumn' :: ColumnName -> ColumnMap -> (ColumnIndex, ColumnMap)
addColumn' name am =
  if isExist
    then error
      "[ERROR]<addColumn'> Auto-generated Column idx is duplicated with another exist"
    else addColumn name am
  where
    idx     = I.size am
    isExist = isJust (I.lookup idx am)

deleteColumn :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumn idx am = (deletedName, I.mapKeys fixer am)
  where
    deletedName = fromJust $ I.lookup idx am
    fixer prev = if prev > idx then prev - 1 else prev

deleteColumn' :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumn' idx am =
  if isJust (I.lookup idx am)
    then deleteColumn idx am
    else error $ "[ERROR]<deleteColumn'> No such Column like " ++ show idx

changeColumnName :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnName idx name am = I.insert idx name am

changeColumnName' :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnName' idx name am =
  if isJust (I.lookup idx am)
    then changeColumnName idx name am
    else error $ "[ERROR]<changeColumnName'> No such Column like " ++ show idx

-- TODO: reallocateColumn ::

-- TODO: mergeColumn ::
