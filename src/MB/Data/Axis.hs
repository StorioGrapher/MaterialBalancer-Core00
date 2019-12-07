module MB.Data.Axis where


import           MB.Data.Primitive

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe


type AxisIndex = Int
type AxisName = Name
type AxisMap = IntMap AxisName


getAxisName :: AxisMap -> AxisIndex -> AxisName
getAxisName am idx = fromJust $ I.lookup idx am

getAxisName' :: AxisMap -> AxisIndex -> AxisName
getAxisName' am idx =
  if isJust mAxisName
    then getAxisName am idx
    else error $ "[ERROR]<getAxisName'> No such Axis like " ++ show idx
  where
    mAxisName = I.lookup idx am

addAxis :: AxisMap -> AxisName -> (AxisIndex, AxisMap)
addAxis am name = (idx, I.insert idx name am)
  where idx = I.size am

addAxis' :: AxisMap -> AxisName -> (AxisIndex, AxisMap)
addAxis' am name =
  if isExist
    then error
      "[ERROR]<addAxis'> Auto-generated Axis idx is duplicated with another exist"
    else addAxis am name
  where
    idx     = I.size am
    isExist = isJust (I.lookup idx am)

deleteAxis :: AxisMap -> AxisIndex -> (AxisName, AxisMap)
deleteAxis am idx = (deletedName, I.mapKeys fixer am)
  where
    deletedName = fromJust $ I.lookup idx am
    fixer prev = if prev > idx then prev - 1 else prev

deleteAxis' :: AxisMap -> AxisIndex -> (AxisName, AxisMap)
deleteAxis' am idx =
  if isJust (I.lookup idx am)
    then deleteAxis am idx
    else error $ "[ERROR]<deleteAxis'> No such Axis like " ++ show idx

changeAxisName :: AxisMap -> AxisIndex -> AxisName -> AxisMap
changeAxisName am idx name = I.insert idx name am

changeAxisName' :: AxisMap -> AxisIndex -> AxisName -> AxisMap
changeAxisName' am idx name =
  if isJust (I.lookup idx am)
    then changeAxisName am idx name
    else error $ "[ERROR]<changeAxisName'> No such Axis like " ++ show idx
