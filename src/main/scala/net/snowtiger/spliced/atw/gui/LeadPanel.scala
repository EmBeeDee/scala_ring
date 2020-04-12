package net.snowtiger.spliced.atw.gui

import java.awt.Font
import javax.swing.table.{AbstractTableModel, TableCellEditor}
import javax.swing.{AbstractCellEditor, JTable}

import scala.swing.ListView.Renderer
import scala.swing._
import scala.swing.event.{FocusLost, SelectionChanged}

/**
 * @author mark
 */

class LeadPanel(val methodPanel: MethodPanel) extends Table
{
	font = new Font("Monospaced", 0, 14)
	rowHeight = 20
	autoResizeMode = Table.AutoResizeMode.Off
	showGrid = false
	val leadData = methodPanel.leadData
	val leadModel = new LeadTableModel
	model = leadModel

	class LeadTableModel extends AbstractTableModel
	{
		def getRowCount = leadData.displayLeadLen
		def getColumnCount = 1
		def getPN(rowIndex: Int) = if (rowIndex<leadData.pnList.size) leadData.pnList(rowIndex).toString else ""
		def getRow(rowIndex: Int) = leadData.displayRows(rowIndex) match
		{
			case Some(row) => row.toString
			case None => 			emptyLine(rowIndex)
		}
		def getValueAt(rowIndex: Int, columnIndex: Int) = getPN(rowIndex).reverse.padTo(4,' ').reverse+" "+getRow(rowIndex)
		override def isCellEditable(rowIndex: Int, columnIndex: Int) =
			rowIndex<getRowCount-1 && methodPanel.leadData.displayRows(rowIndex).isDefined
		override def setValueAt(value: Any, rowIndex: Int, colIndex: Int)
		{
			redrawFrom(rowIndex)
		}
		def updateCellsFrom(rowIndex: Int)
		{
			for (i <- rowIndex until getRowCount)
				fireTableCellUpdated(i, 0)
		}
	}

	def redrawFrom(rowIndex: Int)
	{
		methodPanel.title.text = leadData.name
		leadModel.updateCellsFrom(rowIndex)
	}

	override protected def editor(row: Int, column: Int) = rowEditor

	private val rowEditor = new AbstractCellEditor with TableCellEditor {
		def getCellEditorValue = "?"
		// ouch, we get JTable not scala.swing.Table ...
		def getTableCellEditorComponent(tab: JTable, value: AnyRef, isSelected: Boolean, row: Int, col: Int) =
		{
			selectedRow(row).peer.asInstanceOf[java.awt.Component]
		}
	}

	def deselect()
	{
		rowEditor.stopCellEditing()
	}

	def selectedRow(row: Int) = new ComboBox(methodPanel.getSelections(row)){
		renderer = Renderer(_.toString)
		listenTo(selection)
		reactions+= {
			case FocusLost(source,other,temp) => deselect()
			case SelectionChanged(combo) => {
				selection.item.select(row)
				model.setValueAt("??", row, 0)
				deselect()
			}
		}
	}

	def emptyLine(row: Int) =
	{
		val treble = treblePosition(row)
		"."*treble + "1" + "."*(methodPanel.nbells-1-treble)
	}

	def treblePosition(row: Int) =
	{
		(row/4)*2 + row%2
	}
}
