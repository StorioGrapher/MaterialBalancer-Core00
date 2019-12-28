module Test.MaterialBalancer.Data.ValueStorage where


import Test.MaterialBalancer.Data.TestSet

import Test.Helper
import Test.Framework
import Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.Framework.TH
import Test.HUnit.Base

import MaterialBalancer.Data.Primitive
import MaterialBalancer.Data.ValueStorage


import Data.List
import Data.Maybe
import Data.Ord


tests = $(testGroupGenerator)

k1 = [s1_0,s1_1,s1_2]
k1' = [s1_1,s1_0,s1_2]
k2 = []
s1_0 = (0,1)
s1_1 = (1,1)
s1_2 = (2,1)

-- NOTE: check equality with function `getValue`
case_eq01 = assertEqual "k1 == sorted k1'" k1 (sortOn fst k1')


--  =TEST=  getValue and getValues
-- ==TEST== Get a value with sorted key
-- ==TEST== Get a value with unsorted key
-- ==TEST== Get a value with incomplete key
-- ==TEST== Get a value with
-- ==TEST== Get values with sorted keys
-- ==TEST== Get values with unsorted keys
-- ==TEST== Get values with incomplete keys
