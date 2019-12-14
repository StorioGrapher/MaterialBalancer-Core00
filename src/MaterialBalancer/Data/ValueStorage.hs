module MaterialBalancer.Data.ValueStorage where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I

import           Data.Maybe
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )


-- TODO: the data definition should guarantee that every leaf's depth are same
data RMap a = RM (IntMap (RMap a)) | IM (IntMap a) deriving Show
type Variable = Maybe Value
type ValueStorage = RMap Variable


getValues :: Keys -> ValueStorage -> [(Key, Variable)]
getValues keys vs = map (\k -> (k, getValue k vs)) keys

-- NOTE: Exclude input data checker to getValue'
getValue :: Key -> ValueStorage -> Variable
getValue key = getValueSub condensedKey
 where
  sortedKey    = sortOn fst key
  condensedKey = map snd sortedKey

getValue' :: Key -> ValueStorage -> Variable
getValue' key vs = if isGood
  then getValue sortedKey vs
  else error $ "[ERROR]<getValue'>: The given Key is illegal: " ++ show key
 where
  sortedKey = sortOn fst key
  isGood    = checker sortedKey
  checker []                          = True
  checker [     _                   ] = True
  checker keys@((x, _) : (y, _) : zs) = (x + 1 /= y) || checker (tail keys)

getValueSub :: [AxisIndex] -> ValueStorage -> Variable
getValueSub [] _ = error "[ERROR]<getValueSub>: Can't reach!"
getValueSub [_] (RM _) =
  error "[ERROR]<getValueSub>: The Key have too less selector"
getValueSub [idx] (IM im) = fromJust $ I.lookup idx im
getValueSub _ (IM _) =
  error "[ERROR]<getValueSub>: The Key have too much selector"
getValueSub (idx : rest) (RM rm) = I.lookup idx rm >>= getValueSub rest


setValue :: Key -> Variable -> ValueStorage -> ValueStorage
setValue key variable vs = setValueSub vs key
 where
  setValueSub :: ValueStorage -> Key -> ValueStorage
  setValueSub (IM im) [(_, cIdx)       ] = IM $ I.insert cIdx variable im
  setValueSub (RM rm) ((_, cIdx) : rest) = RM $ I.insert cIdx newMap rm
    where newMap = setValueSub (fromJust $ I.lookup cIdx rm) rest

-- NOTE: Add a Variable for each selected Axis-map
addColumn :: AxisIndex -> ValueStorage -> ValueStorage
addColumn aIdx vs = advanceUntilTargetAxis 0 vs
 where
    -- Step: Get column size of each Axis
  columnSizes = getColumnSizes vs
  axesSize    = length columnSizes
  -- Step: Advance until the targeted Axis
  advanceUntilTargetAxis :: AxisIndex -> ValueStorage -> ValueStorage
  advanceUntilTargetAxis idx rIM@(IM im) = addNewColumn rIM undefined -- FIXME: Try to remove `undefined`
  advanceUntilTargetAxis idx rRM@(RM rm)
    | idx < aIdx = RM $ I.map (advanceUntilTargetAxis (idx + 1)) rm
    | otherwise  = addNewColumn rRM (blankTree . snd . I.findMin $ rm) -- TODO: This can be generated only one-time by using columnSizes
  addNewColumn :: ValueStorage -> ValueStorage -> ValueStorage
  addNewColumn (IM im) _   = IM $ I.insert (I.size im) Nothing im
  addNewColumn (RM rm) bst = RM $ I.insert (I.size rm) bst rm

blankTree :: ValueStorage -> ValueStorage
blankTree (IM im) = IM $ I.map (const Nothing) im
blankTree (RM rm) = RM $ I.map blankTree rm

getColumnSizes :: RMap a -> [Int]
getColumnSizes (IM im) = [I.size im]
getColumnSizes (RM rm) = I.size rm : (getColumnSizes . snd . I.findMin $ rm)

addAxis :: ValueStorage -> ValueStorage
addAxis vs = vs

deleteAxis :: AxisIndex -> ValueStorage -> ValueStorage
deleteAxis idx vs = vs
-- TODO:
-- * Convert every sub-tree as lists
--   NOTE: Need to make a mixed key - not easy problem
-- * Append every list as a list
-- * Rebuild a tree from the list

-- TODO: Add RemovingMode as a argument
deleteAxisSmartly :: AxisIndex -> ValueStorage -> ValueStorage
deleteAxisSmartly idx vs = vs
