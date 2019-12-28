module Test.MaterialBalancer.Data.Column where


import           Test.Helper
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import           Test.Framework.TH
import           Test.HUnit.Base

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Column

import qualified Data.IntMap                   as I
import           Data.Maybe


tests = $(testGroupGenerator)

blankColumnsMap = I.empty
blankColumnMap = I.empty
(singleIdx, singleColumnsMap) = addAxis blankColumnsMap

--  =Test=  getColumnMap from ColumnsMap
-- ==Test== Success getColumnMap from singleColumnsMap
-- NOTE: How to check when the function does not occur error?
case_getColumnMap01 = assertEqual
  "getColumnMap 0 singleColumnsMap == I.empty"
  (getColumnMap singleIdx singleColumnsMap)
  blankColumnMap
