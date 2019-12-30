module MaterialBalancer.Data.Primitive where

import           Data.Text                      ( Text )


type ID = Int
type Name = Text
type Index = Int

type Value = Text
type Variable = Maybe Value

type Keys = [Key]
type Key = [Selector]
type Selector = (Index, Index) -- NOTE: (AxisIndex,ColumnIndex)

type Label = Text
type Labeling = Maybe Label
