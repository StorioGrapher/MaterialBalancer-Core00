module Test.MaterialBalancer.Data.Axis where


import           Test.Helper
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import           Test.Framework.TH
import           Test.HUnit.Base

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis

import           Data.IntMap                   as I


tests = $(testGroupGenerator)

initialAxis1 = initAxis ["ABC"]
initialAxis2 = initAxis ["A", "B", "C"]
initialAxis2' = initAxis ["A", "B", "C", "D"]

--  ==TEST==  Add and delete an axis
-- ===TEST=== Add an axis
case_eq01 = assertEqual "initAxis [\"ABC\"] == I.fromList [(0,\"ABC\")]"
                        initialAxis1
                        (I.fromList [(0, "ABC")])
