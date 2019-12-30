module MaterialBalancer.Data.Column where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe


type ColumnsMap = IntMap ColumnMap
type ColumnIndex = Index
type ColumnName = Name
type ColumnMap = IntMap ColumnName


getColumnMap :: AxisIndex -> ColumnsMap -> ColumnMap
getColumnMap idx columnsMap = fromJust $ I.lookup idx columnsMap

getColumnMap' :: AxisIndex -> ColumnsMap -> ColumnMap
getColumnMap' idx columnsMap = if isJust mColumnMap
  then getColumnMap idx columnsMap
  else error $ "[ERROR]<getColumnMap'> No such ColumnMap like " ++ show idx
  where mColumnMap = I.lookup idx columnsMap

getColumnName :: AxisIndex -> ColumnIndex -> ColumnsMap -> ColumnName
getColumnName ai ci columnsMap = getColumnNameIn ci targetCM
  where targetCM = columnsMap I.! ai

getColumnName' :: AxisIndex -> ColumnIndex -> ColumnsMap -> ColumnName
getColumnName' ai ci columnsMap = getColumnNameIn' ci targetCM
 where
  mTargetCM = I.lookup ai columnsMap
  targetCM  = fromMaybe
    (  error ("[ERROR]<getColumnName'> No such ColumnMap in ColumnsMap["
    ++ show ai
    ++ "]")
    )
    mTargetCM

getColumnNameIn :: ColumnIndex -> ColumnMap -> ColumnName
getColumnNameIn ci cm = cm I.! ci

getColumnNameIn' :: ColumnIndex -> ColumnMap -> ColumnName
getColumnNameIn' ci cm = fromMaybe
  (  error ("[ERROR]<getColumnNameIn'> No such ColumnName in Column["
  ++ show ci
  ++ "]")
  )
  mName
  where mName = I.lookup ci cm

addAxis :: ColumnsMap -> (AxisIndex, ColumnsMap)
addAxis columnsMap = (newIdx, I.insert newIdx I.empty columnsMap)
  where newIdx = I.size columnsMap


addColumn :: AxisIndex -> ColumnName -> ColumnsMap -> (ColumnIndex, ColumnsMap)
addColumn ai name columnsMap = (ci, I.insert ai newCM columnsMap)
 where
  targetCM    = columnsMap I.! ai
  (ci, newCM) = addColumnIn name targetCM

addColumn' :: AxisIndex -> ColumnName -> ColumnsMap -> (ColumnIndex, ColumnsMap)
addColumn' ai name columnsMap = (ci, I.insert ai newCM columnsMap)
 where
  target   = I.lookup ai columnsMap
  targetCM = fromMaybe
    (error "[ERROR]<addColumn'>: No such Axis idx in the ColumnsMap")
    target
  (ci, newCM) = addColumnIn' name targetCM

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


deleteColumn
  :: AxisIndex -> ColumnIndex -> ColumnsMap -> (ColumnName, ColumnsMap)
deleteColumn ai ci columnsMap = (deletedName, I.insert ai newCM columnsMap)
 where
  targetCM             = columnsMap I.! ai
  (deletedName, newCM) = deleteColumnIn ci targetCM

deleteColumn'
  :: AxisIndex -> ColumnIndex -> ColumnsMap -> (ColumnName, ColumnsMap)
deleteColumn' ai ci columnsMap = (deletedName, I.insert ai newCM columnsMap)
 where
  target   = I.lookup ai columnsMap
  targetCM = fromMaybe
    (error "[ERROR]<deleteColumn'>: No such Axis idx in the ColumnsMap")
    target
  (deletedName, newCM) = deleteColumnIn' ci targetCM

deleteColumnIn :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumnIn idx cm = (deletedName, I.mapKeys fixer cm)
 where
  deletedName = fromJust $ I.lookup idx cm
  fixer prev = if prev > idx then prev - 1 else prev

deleteColumnIn' :: ColumnIndex -> ColumnMap -> (ColumnName, ColumnMap)
deleteColumnIn' idx cm = if isJust (I.lookup idx cm)
  then deleteColumnIn idx cm
  else error $ "[ERROR]<deleteColumn'> No such Column like " ++ show idx

changeColumnName
  :: AxisIndex -> ColumnIndex -> ColumnName -> ColumnsMap -> ColumnsMap
changeColumnName ai ci name columnsMap = I.insert ai newCM columnsMap
 where
  target = columnsMap I.! ai
  newCM  = changeColumnNameIn ci name target

changeColumnName'
  :: AxisIndex -> ColumnIndex -> ColumnName -> ColumnsMap -> ColumnsMap
changeColumnName' ai ci name columnsMap = I.insert ai newCM columnsMap
 where
  target   = I.lookup ai columnsMap
  targetCM = fromMaybe
    (error "[ERROR]<changeColumnName'>: No such Axis idx in the ColumnsMap")
    target
  newCM = changeColumnNameIn ci name targetCM

changeColumnNameIn :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnNameIn = I.insert

changeColumnNameIn' :: ColumnIndex -> ColumnName -> ColumnMap -> ColumnMap
changeColumnNameIn' idx name cm = if isJust (I.lookup idx cm)
  then changeColumnNameIn idx name cm
  else error $ "[ERROR]<changeColumnName'> No such Column like " ++ show idx

-- TODO: reallocateColumn ::

-- TODO: mergeColumn ::
