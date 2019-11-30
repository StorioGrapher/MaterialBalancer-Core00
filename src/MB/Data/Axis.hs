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

getAxisName' :: AxisMap -> AxisIndex -> Maybe AxisName
getAxisName' am idx = I.lookup idx am

addAxis :: AxisMap -> AxisName -> (AxisIndex, AxisMap)
addAxis am name = (idx, I.insert idx name am)
  where idx = I.size am

addAxis' :: AxisMap -> AxisName -> (AxisIndex, AxisMap)
addAxis' am name =
  if isExist
    then error "[ERROR]<addAxis'>: auto-axis idx is wrong"
    else addAxis am name
 where
  idx     = I.size am
  isExist = isJust (I.lookup idx am)

deleteAxis :: AxisMap -> AxisIndex -> (AxisName, AxisMap)
deleteAxis am idx = (deletedName, I.mapKeys fixer am)
 where
  deletedName = fromJust $ I.lookup idx am
  fixer prev = if prev > idx then prev - 1 else prev

deleteAxis' :: AxisMap -> AxisIndex -> Maybe (AxisName, AxisMap)
deleteAxis' am idx =
  if isJust (I.lookup idx am)
    then Just $ deleteAxis am idx
    else Nothing

changeAxisName :: AxisMap -> AxisIndex -> AxisName -> AxisMap
changeAxisName am idx name = I.insert idx name am

changeAxisName' :: AxisMap -> AxisIndex -> AxisName -> Maybe AxisMap
changeAxisName' am idx name =
  if isJust (I.lookup idx am)
    then Just $ changeAxisName am idx name
    else Nothing
