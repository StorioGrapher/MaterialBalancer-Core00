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
data ViewContentSimple = VCSDelimiter | VCSLabel Label | VCSColumn ColumnName | VCSVariable Variable deriving Show

-- TODO: ViewResult2D: 2D version but not using Vector

-- TODO: ViewResultNL: Nested lattice version for High-Dimension matrix
