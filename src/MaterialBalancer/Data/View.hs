module MaterialBalancer.Data.View where


import Data.Text (Text)
import Data.Vector

import MaterialBalancer.Data.Primitive
import MaterialBalancer.Data.Axis
import MaterialBalancer.Data.Column
import MaterialBalancer.Data.ValueStorage
import MaterialBalancer.Data.TheTable


{-
Select by Row-index
Arrange based on Column-index

* About (Axis X, Column A) for Axis Y
  * Group I                 => Column B : Column C : Column D
  * Group J(Label "Joshua") => Column E : Column D
  * Group K(Label "Karl")   => Column F : Column C
-}

-- NOTE: View definition for High-Dimension
type Views = [View]
data View = View
  { viewID :: ID
  , baseAxis :: AxisIndex
  , baseColumns :: [ColumnIndex]
  , viewSetting :: [ViewGroup]
  } deriving Show

-- NOTE: View definition only for 2D
type Views2D = [View2D]
data View2D = View2D
  { view2DID :: ID
  , baseAxis2D :: AxisIndex
  , baseColumns2D :: [ColumnIndex]
  , targetAxis2D :: AxisIndex
  , viewSetting2D :: [View2DGroup]
  } deriving Show

type Label = Text
type Labeling = Maybe Label
data View2DGroup = V2DGroup Labeling [(Labeling, ColumnIndex)] deriving Show
data ViewGroup = VGroup Labeling [(Labeling, ViewColumn)] deriving Show
data ViewColumn = VColumnHD AxisIndex ColumnIndex | VColumnHDLattice [ViewColumn] deriving Show

type View2DResult = Vector ViewContent
data ViewContent = VTDelimiter | VTLabel Label | VTVariable Variable deriving Show

-- TODO: Build and modify View2D
-- TODO: Get default View2D
-- TODO: Build View2D from scratch
-- TODO: Modify View2D by argument
