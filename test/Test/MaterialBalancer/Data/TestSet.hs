module Test.MaterialBalancer.Data.TestSet where


import Test.Helper
import Test.Framework
import Test.Framework.Providers.HUnit
--import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.Framework.TH
import Test.HUnit.Base

import MaterialBalancer.Data.Primitive
import MaterialBalancer.Data.Axis
import MaterialBalancer.Data.Column
import MaterialBalancer.Data.ValueStorage
import MaterialBalancer.Data.TheTable


tests = $(testGroupGenerator)


theAxisMap = undefined
theColumnsMap = undefined
theValueStorage = undefined
theTable01 = (theAxisMap, theColumnsMap, theValueStorage)

--   =TEST=   Test interlocking features
--  ==TEST==  Test add a column
