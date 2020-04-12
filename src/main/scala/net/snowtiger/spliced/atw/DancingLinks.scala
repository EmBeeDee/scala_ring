package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row

import scala.collection.mutable

/**
 * @author mark
 */

class DancingLinks
{
	val start = new ColumnHeader(-1, null, false)
	private var columnIndex = 0
	private var nMandatory = 0

	def createMatrix(mandatoryChanges: Iterable[Row]): Unit =
	{
		for (change <- mandatoryChanges)
			addColumn(change, false)
		nMandatory = columnIndex
	}

	def addMethodRow(node: MethodNode): Unit =
	{
		val changesRemaining = mutable.Set[Row]() ++ node.rows
		var firstCell: RowCell = null
		var col = start.colRight

		while (col!=start)
		{
			if (changesRemaining.contains(col.change))
			{
				firstCell = addRowCell(col, firstCell, node)
				changesRemaining-= col.change
			}
			col = col.colRight
		}
		for (methodRow <- changesRemaining)
		{
			col = addColumn(methodRow, true)
			firstCell = addRowCell(col, firstCell, node)
		}
	}

	/** Returns new firstCell */
	private def addRowCell(col: ColumnHeader, firstCell: RowCell, node: MethodNode): RowCell =
	{
		val rowCell = new RowCell(col, node)
		rowCell.down = col
		rowCell.up = col.up
		col.up.down = rowCell
		col.up = rowCell
		col.nRows+= 1
		if (firstCell==null)
		{
			rowCell.left = rowCell
			rowCell.right = rowCell
			rowCell
		}
		else
		{
			rowCell.right = firstCell
			rowCell.left = firstCell.left
			firstCell.left.right = rowCell
			firstCell.left = rowCell
			firstCell
		}
	}

	private def addColumn(change: Row, optional: Boolean): ColumnHeader =
	{
		val col = new ColumnHeader(columnIndex, change, optional)
		columnIndex+= 1
		col.right = start
		col.left = start.left
		start.left.right = col
		start.left = col
		col
	}

	var highwater = 0

	def search: Unit =
	{
		highwater = 0
		doSearch(Nil)
	}

	def doSearch(revSolution: List[Cell]): Unit =
	{
		if (revSolution.size>highwater)
		{
			highwater = revSolution.size
			println("New highwater: "+highwater)
		}
		if (start.colRight==start || start.colRight.optional)
		{
			printSolution(revSolution)
		}
		else
		{
			val column = chooseNextColumn
			cover(column)
			var row = column.down
			while (row!=column)
			{
				val newSolution = row::revSolution
				var cell = row.right
				while (cell!=row)
				{
					cover(cell.columnHeader)
					cell = cell.right
				}
				doSearch(newSolution)
				cell = row.left
				while (cell!=row)
				{
					uncover(cell.columnHeader)
					cell = cell.left
				}
				row = row.down
			}
			uncover(column)
		}
	}

	private def cover(col: ColumnHeader): Unit =
	{
		col.right.left = col.left
		col.left.right = col.right
		var row = col.down
		while (row!=col)
		{
			var cell = row.right
			while (cell!=row)
			{
				cell.up.down = cell.down
				cell.down.up = cell.up
				cell.columnHeader.nRows-= 1
				cell = cell.right
			}
			row = row.down
		}
	}

	private def uncover(col: ColumnHeader): Unit =
	{
		var row = col.up
		while (row!=col)
		{
			var cell = row.left
			while (cell!=row)
			{
				cell.up.down = cell
				cell.down.up = cell
				cell.columnHeader.nRows+= 1
				cell = cell.left
			}
			row = row.up
		}
		col.right.left = col
		col.left.right = col
	}

	private def chooseNextColumn(): ColumnHeader =
	{
		var col = start.colRight
		var leastCol = col
		var leastRows = leastCol.nRows
		col = col.colRight
		while (col!=start && !col.optional)
		{
			if (col.nRows<leastRows)
			{
				leastRows = col.nRows
				leastCol = col
			}
			col = col.colRight
		}
		leastCol
	}

	private def printSolution(rows: List[Cell]): Unit =
	{
		val sortedRows = rows.sortBy{_.columnHeader.index}
		println(sortedRows.mkString(", "))
	}
}

trait Cell
{
	var left: Cell = this
	var right: Cell = this
	var up: Cell = this
	var down: Cell = this
	def columnHeader: ColumnHeader
}

class RowCell(val columnHeader: ColumnHeader, val node: MethodNode) extends Cell
{
	override def toString = node.toString
}

class ColumnHeader(val index: Int, val change: Row, val optional: Boolean) extends Cell
{
	override def columnHeader = this
	def colLeft: ColumnHeader = left.asInstanceOf[ColumnHeader]
	def colRight: ColumnHeader = right.asInstanceOf[ColumnHeader]
	var nRows = 0
}