module MaterialBalancer.Data.ViewResult where


import           Data.Text                      ( Text )
import           Data.Vector

import           MaterialBalancer.Data.Primitive
import           MaterialBalancer.Data.Axis
import           MaterialBalancer.Data.Column
import           MaterialBalancer.Data.ValueStorage
import           MaterialBalancer.Data.TheTable
import           MaterialBalancer.Data.View

type ViewResultSimple2D = Vector ViewResultSimple
type ViewResultSimple = Vector ViewContentSimple
data ViewContentSimple = VTDelimiter | VTLabel Label | VTColumn ColumnName | VTVariable Variable deriving Show
