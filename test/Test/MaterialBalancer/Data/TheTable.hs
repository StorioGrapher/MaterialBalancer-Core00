module Test.MaterialBalancer.Data.TheTable where


import Test.Helper
import Test.Framework
import Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.Framework.TH
import Test.HUnit.Base

import MaterialBalancer.Data.Primitive
import MaterialBalancer.Data.ValueStorage


tests = $(testGroupGenerator)


theAxisMap = undefined
theColumnsMap = undefined
theValueStorage = undefined
theTable01 = (theAxisMap, theColumnsMap, theValueStorage)

--   =TEST=   Test interlocking features
--  ==TEST==  Test add a column
