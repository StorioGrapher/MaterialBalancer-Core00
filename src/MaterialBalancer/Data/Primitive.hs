module MaterialBalancer.Data.Primitive where

import           Data.Text                      ( Text )

type Value = Text
type Name = Text

type Keys = [Key]
type Key = [Selector]
type Selector = (Int, Int) -- NOTE: (AxisIndex,ColumnIndex)
