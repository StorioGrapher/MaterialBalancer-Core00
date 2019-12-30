module MaterialBalancer.Data.ValueStorage where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis     ( AxisIndex
                                                , AxisName
                                                , AxisMap
                                                )
import qualified MaterialBalancer.Data.Axis    as A

import qualified Data.Foldable                 as F
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I
import           Data.Maybe
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )


-- TODO: the data definition should guarantee that every leaf's depth are same
data RMap a = RM (IntMap (RMap a)) | IM (IntMap a) deriving Show
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


setVariable :: Key -> Variable -> ValueStorage -> ValueStorage
setVariable key variable vs = setVariableSub vs key
 where
  setVariableSub :: ValueStorage -> Key -> ValueStorage
  setVariableSub (IM im) [(_, cIdx)       ] = IM $ I.insert cIdx variable im
  setVariableSub (RM rm) ((_, cIdx) : rest) = RM $ I.insert cIdx newMap rm
    where newMap = setVariableSub (fromJust $ I.lookup cIdx rm) rest

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
addAxis (RM rm) = RM . I.map addAxis $ rm
addAxis (IM im) = RM . I.map (IM . I.singleton 0) $ im


deleteAxis :: AxisIndex -> ValueStorage -> ValueStorage
deleteAxis idx vs = error "[ERROR]<deleteAxis>: Not yet implemented"
-- TODO:
-- * Convert every sub-tree as lists
--   NOTE: Need to make a mixed key - not easy problem
-- * Append every list as a list
-- * Rebuild a tree from the list

-- TODO: Add RemovingMode as a argument
deleteAxisSmartly :: AxisIndex -> ValueStorage -> ValueStorage
deleteAxisSmartly idx vs = vs

checkStructure :: ValueStorage -> Bool
checkStructure = isJust . checkStructureSub 0

checkStructureSub :: Int -> ValueStorage -> Maybe (Int, Int)
checkStructureSub depth (IM im) = Just (depth, I.size im)
-- FIXME: Should I have to get depth from `everySize`?
checkStructureSub depth (RM rm) =
  (\x -> Just (depth, x)) =<< I.foldr checkIt base everySize
 where
  everySize = I.map (checkStructureSub (depth + 1)) rm
  mFirst    = I.lookupMin everySize
  first     = snd =<< mFirst
  base      = if isJust mFirst then Just 0 else Nothing
  checkIt v b = if v == first then (+ 1) <$> b else Nothing


fromList1 :: [Variable] -> ValueStorage
fromList1 = IM . I.fromList . zip [0 ..]

fromList2 :: [[Variable]] -> ValueStorage
fromList2 = RM . I.fromList . zip [0 ..] . map fromList1

fromList3 :: [[[Variable]]] -> ValueStorage
fromList3 = RM . I.fromList . zip [0 ..] . map fromList2

fromList4 :: [[[[Variable]]]] -> ValueStorage
fromList4 = RM . I.fromList . zip [0 ..] . map fromList3

toList1 :: ValueStorage -> [Variable]
toList1 (IM im) = map snd . I.toList $ im
toList1 (RM _ ) = error "[ERROR]<toList1>: You can't apply `toList1` to (RM _)"

toList2 :: ValueStorage -> [[Variable]]
toList2 (IM im) = error "[ERROR]<toList2>: You can't apply `toList2` to (IM _)"
toList2 (RM rm) = map (toList1 . snd) . I.toList $ rm

toList3 :: ValueStorage -> [[[Variable]]]
toList3 (IM im) = error "[ERROR]<toList3>: You can't apply `toList3` to (IM _)"
toList3 (RM rm) = map (toList2 . snd) . I.toList $ rm

toList4 :: ValueStorage -> [[[[Variable]]]]
toList4 (IM im) = error "[ERROR]<toList4>: You can't apply `toList4` to (IM _)"
toList4 (RM rm) = map (toList3 . snd) . I.toList $ rm
