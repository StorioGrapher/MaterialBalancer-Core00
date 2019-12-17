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
-- putStrLn . showValueStorageWithIdx (IM . I.fromList $ Prelude.zip [0..20] $ Prelude.map ( Just . T.pack . show ) ['a','b'..])
-- putStrLn . showValueStorageWithIdx . RM . I.fromList $ [(0,(IM . I.fromList $ Prelude.zip [0..12] $ Prelude.map ( Just . T.pack . show ) ['a','b'..])),(1,(IM . I.fromList $ Prelude.zip [0..12] $ Prelude.map ( Just . T.pack . show ) ['a','b'..]))]

{- α β γ δ
α--+--+-0=0:1=0:2=0: Just
β  +  +-0=0:1=0:2=1: Nothing
β  +  --0=0:1=0:2=2: Nothing
α  +--+-0=0:1=1:2=0: Just
β  +  +-0=0:1=1:2=1: Just
β  +  --0=0:1=1:2=2: Nothing
γ  ---+-0=0:1=3:2=0: Just
δ     --0=0:1=3:2=1: Nothing
-}

showValueStorageWithIdx :: ValueStorage -> String
showValueStorageWithIdx vs = unlines . showValueStorageWithIdxSub vs [] $ 0

showValueStorageWithIdxSub :: ValueStorage -> Keys -> Int -> [String]
showValueStorageWithIdxSub (RM rm) accKeys depth = headerElements preprocessed
 where
  len         = getDecimalLen . I.size $ rm
  alphaHeader = "+-"
  betaHeader  = "| "
  gammaHeader = "--"
  deltaHeader = "  "
  preprocessed :: [(Int, [String])]
  -- TODO: Do with accKeys
  preprocessed =
    map (\(k, v) -> (k, (showValueStorageWithIdxSub v accKeys (depth + 1))))
      . I.toList
      $ rm
  -- NOTE: headerElements cares whether the element is the last or not
  headerElements :: [(Int, [String])] -> [String]
  headerElements []                    = []
  headerElements ((key, strings) : xs) = processed ++ headerElements xs
   where
    bodies = bodyElements key strings
    -- NOTE: α or γ
    first  = (if xs == [] then gammaHeader else alphaHeader) ++ head bodies
    -- NOTE: β or δ
    rest =
      map ((if xs == [] then deltaHeader else betaHeader) ++) (tail bodies)
    processed = first : rest
    -- NOTE: headerElementsSub cares whether the element is the first or not
  bodyElements :: Int -> [String] -> [String]
  bodyElements _   []      = []
  bodyElements key strings = first : rest
   where
    betaBody  = "-"
    deltaBody = " "
    shownKey  = show key
    shownLen  = length shownKey
    keySpacer = replicate (len - shownLen) '_'
    first =
      betaBody
        ++ show depth
        ++ "="
        ++ keySpacer
        ++ shownKey
        ++ betaBody
        ++ head strings
    bodySpacer = replicate (1 + 1 + 1 + len + 1) ' '
    rest       = map (bodySpacer ++) . tail $ strings

showValueStorageWithIdxSub (IM im) accKeys depth =
  labeledElements . I.toList $ im
 where
  len = getDecimalLen . I.size $ im
  labeledElements []                  = []
  labeledElements ((key, value) : xs) = first : labeledElementsSub xs
   where
    first =
      (if xs == [] then "---" else "-+-")
        ++ show depth
        ++ "="
        ++ space
        ++ shownKey
        ++ ": "
        ++ show value
     where
      shownKey = show key
      shownLen = length shownKey
      space    = replicate (len - shownLen) '_'
    labeledElementsSub []                  = []
    labeledElementsSub ((key, value) : xs) = body : labeledElementsSub xs
     where
      body =
        (if xs == [] then " --" else " +-")
          ++ show depth
          ++ "="
          ++ space
          ++ shownKey
          ++ ": "
          ++ show value
       where
        shownKey = show key
        shownLen = length shownKey
        space    = replicate (len - shownLen) '_'

getDecimalLen num = getDecimalLenSub num 1
getDecimalLenSub num acc | dived > 0 = getDecimalLenSub dived (acc + 1)
                         | otherwise = acc
  where dived = div num 10
