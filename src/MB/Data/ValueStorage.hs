module MB.Data.ValueStorage where


import           MB.Data.Primitive
import           MB.Data.Axis

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I

import           Data.Maybe
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )


-- TODO: the data definition should guarantee that every leaf's depth are same
data RMap a = RM (IntMap (RMap a)) | IM (IntMap a) deriving Show
type Variable = Maybe Value
type ValueStorage = RMap Variable


getValues :: ValueStorage -> Keys -> [(Key,Variable)]
getValues vs = map (\k -> (k, getValue vs k))

-- NOTE: Exclude input data checker to getValue'
getValue :: ValueStorage -> Key -> Variable
getValue vs key = getValueSub condensedKey vs
  where
    sortedKey = sortOn fst key
    condensedKey = map snd sortedKey

getValue' :: ValueStorage -> Key -> Variable
getValue' vs key =
  if isGood
    then getValue vs sortedKey
    else error $ "[ERROR]<getValue'>: The given Key is illegal: " ++ show key
  where
    sortedKey = sortOn fst key
    isGood = checker sortedKey
    checker [] = True
    checker [_] = True
    checker keys@((x,_):(y,_):zs) = (x + 1 /= y) || checker (tail keys)

getValueSub :: [AxisIndex] -> ValueStorage -> Variable
getValueSub [] _ = error "[ERROR]<getValueSub>: Can't reach!"
getValueSub [_] (RM _) = error "[ERROR]<getValueSub>: The Key have too less selector"
getValueSub [idx] (IM im) = fromJust $ I.lookup idx im
getValueSub _ (IM _) = error "[ERROR]<getValueSub>: The Key have too much selector"
getValueSub (idx:rest) (RM rm) = I.lookup idx rm >>= getValueSub rest


setValue :: ValueStorage -> Key -> Variable -> ValueStorage
setValue vs key variable = setValueSub vs key
  where
    setValueSub :: ValueStorage -> Key -> ValueStorage
    setValueSub (IM im) [(_,cIdx)] = IM $ I.insert cIdx variable im
    setValueSub (RM rm) ((_,cIdx):rest) = RM $ I.insert cIdx newMap rm
      where
        newMap = setValueSub (fromJust $ I.lookup cIdx rm) rest

-- NOTE: Add a Variable for each selected Axis-map
addColumn :: ValueStorage -> AxisIndex -> ValueStorage
addColumn vs aIdx = advanceUntilTargetAxis 0 vs
  where
    -- Step: Get column size of each Axis
    columnSizes = getColumnSizes vs
    axesSize = length columnSizes
    -- Step: Advance until the targeted Axis
    advanceUntilTargetAxis :: AxisIndex -> ValueStorage -> ValueStorage
    advanceUntilTargetAxis idx rIM@(IM im) = addNewColumn rIM undefined -- FIXME: Try to remove `undefined`
    advanceUntilTargetAxis idx rRM@(RM rm)
      | idx < aIdx = RM $ I.map (advanceUntilTargetAxis (idx+1)) rm
      | otherwise = addNewColumn rRM (blankTree . snd . I.findMin $ rm) -- TODO: This can be generated only one-time by using columnSizes
    addNewColumn :: ValueStorage -> ValueStorage -> ValueStorage
    addNewColumn (IM im) _ = IM $ I.insert (I.size im) Nothing im
    addNewColumn (RM rm) bst = RM $ I.insert (I.size rm) bst rm

blankTree :: ValueStorage -> ValueStorage
blankTree (IM im) = IM $ I.map (\_ -> Nothing) im
blankTree (RM rm) = RM $ I.map blankTree rm

getColumnSizes :: RMap a -> [Int]
getColumnSizes (IM im) = [I.size im]
getColumnSizes (RM rm) = (I.size rm):(getColumnSizes . snd . I.findMin $ rm)

addAxis :: ValueStorage -> ValueStorage
addAxis vs = vs

deleteAxis :: ValueStorage -> AxisIndex -> ValueStorage
deleteAxis vs idx = vs
-- TODO:
-- * Convert every sub-tree as lists
--   NOTE: Need to make a mixed key - not easy problem
-- * Append every list as a list
-- * Rebuild a tree from the list

-- TODO: Add RemovingMode as a argument
deleteAxisSmartly :: ValueStorage -> AxisIndex -> ValueStorage
deleteAxisSmartly vs idx = vs
