module MaterialBalancer.Data.Axis where


import           MaterialBalancer.Data.Primitive

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe


type AxisIndex = Int
type AxisName = Name
type AxisMap = IntMap AxisName


initAxis :: [AxisName] -> AxisMap
initAxis = I.fromList . zip [0 ..]

getAxisName :: AxisIndex -> AxisMap -> AxisName
getAxisName idx am = fromJust $ I.lookup idx am

getAxisName' :: AxisIndex -> AxisMap -> AxisName
getAxisName' idx am = if isJust mAxisName
  then getAxisName idx am
  else error $ "[ERROR]<getAxisName'> No such Axis like " ++ show idx
  where mAxisName = I.lookup idx am

addAxis :: AxisName -> AxisMap -> (AxisIndex, AxisMap)
addAxis name am = (idx, I.insert idx name am) where idx = I.size am

addAxis' :: AxisName -> AxisMap -> (AxisIndex, AxisMap)
addAxis' name am = if isExist
  then
    error
      "[ERROR]<addAxis'> Auto-generated Axis idx is duplicated with another exist"
  else addAxis name am
 where
  idx     = I.size am
  isExist = isJust (I.lookup idx am)

deleteAxis :: AxisIndex -> AxisMap -> (AxisName, AxisMap)
deleteAxis idx am = (deletedName, I.mapKeys fixer am)
 where
  deletedName = fromJust $ I.lookup idx am
  fixer prev = if prev > idx then prev - 1 else prev

deleteAxis' :: AxisIndex -> AxisMap -> (AxisName, AxisMap)
deleteAxis' idx am = if isJust (I.lookup idx am)
  then deleteAxis idx am
  else error $ "[ERROR]<deleteAxis'> No such Axis like " ++ show idx

changeAxisName :: AxisIndex -> AxisName -> AxisMap -> AxisMap
changeAxisName = I.insert

changeAxisName' :: AxisIndex -> AxisName -> AxisMap -> AxisMap
changeAxisName' idx name am = if isJust (I.lookup idx am)
  then changeAxisName idx name am
  else error $ "[ERROR]<changeAxisName'> No such Axis like " ++ show idx
