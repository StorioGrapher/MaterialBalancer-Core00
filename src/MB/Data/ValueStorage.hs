module MB.Data.ValueStorage where


import MB.Data.Primitive
import MB.Data.Axis

import Data.IntMap (IntMap)
import qualified Data.IntMap as I

import Data.List (sortOn)
import Data.Ord (comparing)


-- TODO: the data definition should guarantee that every leaf's depth are same
data RMap a = RM (IntMap (RMap a)) | IM (IntMap a)
type ValueStorage = RMap Value


getValues :: ValueStorage -> Keys -> [(Key,Maybe Value)]
getValues vs = map (\k -> (k, getValue vs k))

-- NOTE: Exclude input data checker to getValue'
getValue :: ValueStorage -> Key -> Maybe Value
getValue vs key = getValueSub condensedKey vs
  where
    sortedKey = sortOn fst key
    condensedKey = map snd sortedKey

getValue' :: ValueStorage -> Key -> Maybe Value
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

getValueSub :: [AxisIndex] -> ValueStorage -> Maybe Value
getValueSub [] _ = error "[ERROR]<getValueSub>: Can't reach!"
getValueSub [_] (RM _) = error "[ERROR]<getValueSub>: The Key have too less selector"
getValueSub [idx] (IM im) = I.lookup idx im
getValueSub _ (IM _) = error "[ERROR]<getValueSub>: The Key have too much selector"
getValueSub (idx:rest) (RM rm) = I.lookup idx rm >>= getValueSub rest


removeAxis :: ValueStorage -> AxisIndex -> ValueStorage
removeAxis vs idx = vs
-- TODO:
-- * Convert every sub-tree as lists
--   NOTE: Need to make a mixed key - not easy problem
-- * Append every list as a list
-- * Rebuild a tree from the list

-- TODO: Add RemovingMode as a argument
removeAxisSmartly :: ValueStorage -> AxisIndex -> ValueStorage
removeAxisSmartly vs idx = vs
