module MaterialBalancer.Data.Debug where


import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis
import           MaterialBalancer.Data.Column
import           MaterialBalancer.Data.ValueStorage
import           MaterialBalancer.Data.TheTable

import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as I

import           Data.Maybe
import           Data.List                      ( sortOn )
import           Data.Ord                       ( comparing )


-- NOTE: For testing, use code:
-- mapM_ putStrLn . showValueStorageWithIdx (IM . I.fromList $ Prelude.zip [0..20] $ Prelude.map ( Just . T.pack . show ) ['a','b'..])
-- mapM_ putStrLn . showValueStorageWithIdx . RM . I.fromList $ [(0,(IM . I.fromList $ Prelude.zip [0..20] $ Prelude.map ( Just . T.pack . show ) ['a','b'..]))]

showValueStorageWithIdx :: ValueStorage -> String
showValueStorageWithIdx vs = unlines this
 where
  this = showValueStorageWithIdxSub vs 0

showValueStorageWithIdxSub :: ValueStorage -> Int -> [String]
showValueStorageWithIdxSub (RM rm) depth = labeledElements preprocessed
 where
  len = getDecimalLen . I.size $ rm
  preprocessed :: [(Int,[String])]
  preprocessed = map (\(k,v) -> (k, (showValueStorageWithIdxSub v (depth+1)))) . I.toList $ rm
  labeledElements :: [(Int,[String])] -> [String]
  labeledElements [] = []
  labeledElements ((key,strings):xs) = processed ++ labeledElements xs
   where
    shown = show key
    shownLen = length shown
    header = show depth ++ "=" ++ replicate (len+1-shownLen) '_' ++ show key ++ "-"
    headerSpace = replicate (length header - 1) ' ' ++ " "
    processed = if length strings > 0 then (header ++ (head strings)) : map (headerSpace ++) (tail strings) else []

showValueStorageWithIdxSub (IM im) depth = labeledElements . I.toList $ im
 where
  len = getDecimalLen . I.size $ im
  labeledElements [] = []
  labeledElements ((key,value):xs) = (header ++ show value) : labeledElements xs
   where
    shown = show key
    shownLen = length shown
    space = replicate (len+1-shownLen) '_'
    header = (if xs == [] then "--" else "+-") ++ show depth ++ "=" ++ space ++ shown ++ ": "

getDecimalLen num = getDecimalLenSub num 0
getDecimalLenSub num acc
 | dived > 0 = getDecimalLenSub dived (acc+1)
 | otherwise = acc
 where
  dived = div num 10
