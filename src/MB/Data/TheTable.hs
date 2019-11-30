-- Note: TheTable module provides abstract interface that operate Axis, Columns and ValueStorage with single function

module MB.Data.TheTable where


import MB.Data.Primitive
import MB.Data.Axis
import MB.Data.Column
import MB.Data.ValueStorage


type TheTable = (AxisMap, ColumnsMap, ValueStorage)

addColumnAt :: AxisIndex -> ColumnName -> TheTable -> (ColumnIndex, TheTable)
addColumnAt axisIdx columnName theTable = (0,theTable)

deleteColumnAt :: AxisIndex -> ColumnIndex -> TheTable -> Maybe TheTable
deleteColumnAt axisIdx columnIdx theTable = Just theTable
