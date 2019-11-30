module MB.Data.Column where


import           MB.Data.Primitive
import           MB.Data.Axis

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe


type ColumnsMap = IntMap ColumnMap
type ColumnIndex = Int
type ColumnName = Name
type ColumnMap = IntMap ColumnName


getColumnMap :: ColumnsMap -> AxisIndex -> ColumnMap
getColumnMap am idx = fromJust $ I.lookup idx am

getColumnMap' :: ColumnsMap -> AxisIndex -> Maybe ColumnMap
getColumnMap' am idx = I.lookup idx am

addColumn :: ColumnMap -> ColumnName -> (ColumnIndex, ColumnMap)
addColumn am name = (idx, I.insert idx name am)
  where idx = I.size am

addColumn' :: ColumnMap -> ColumnName -> (ColumnIndex, ColumnMap)
addColumn' am name =
  if isExist
    then error
      "[ERROR]<addColumn'>: auto-Column idx is duplicated with another exist"
    else addColumn am name
 where
  idx     = I.size am
  isExist = isJust (I.lookup idx am)

deleteColumn :: ColumnMap -> ColumnIndex -> (ColumnName, ColumnMap)
deleteColumn am idx = (deletedName, I.mapKeys fixer am)
 where
  deletedName = fromJust $ I.lookup idx am
  fixer prev = if prev > idx then prev - 1 else prev

deleteColumn' :: ColumnMap -> ColumnIndex -> Maybe (ColumnName, ColumnMap)
deleteColumn' am idx =
  if isJust (I.lookup idx am)
    then Just $ deleteColumn am idx
    else Nothing

changeColumnName :: ColumnMap -> ColumnIndex -> ColumnName -> ColumnMap
changeColumnName am idx name = I.insert idx name am

changeColumnName' :: ColumnMap -> ColumnIndex -> ColumnName -> Maybe ColumnMap
changeColumnName' am idx name =
  if isJust (I.lookup idx am)
    then Just $ changeColumnName am idx name
    else Nothing

-- TODO: reallocateColumn ::

-- TODO: mergeColumn ::
