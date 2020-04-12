package net.snowtiger.spliced.atw.gui

import net.snowtiger.ringing.{NamedMethod, PN}

import scala.swing.Swing.Lowered
import scala.swing._

/**
 * @author mark
 */

class MethodPanel(n: Int) extends BoxPanel(Orientation.Vertical)
{
	val nbells = 8
	val model = AtwUI.model
	def leadData = model.modelData(n)

	val leadPanel = new LeadPanel(this)
	val title = new Label(leadData.name){
		preferredSize = new Dimension(80,20)
	}
	val controlPanel = new ControlPanel(this)

	def getSelections(row: Int): List[Selection] =
	{
		var selections = List[Selection]()
		selections = leadData.pnChoices(row).map{PNSelection(_)}
		selections = selections ++ leadData.methodChoices(row).map{MethodSelection(_)}.sortBy{_.method.name}
		if (selections.isEmpty)
			selections = NoSelections("Dead End")::selections
		selections
	}

	def reset()
	{
		leadData.reset(0)
		refreshAll()
	}

	def refreshAll()
	{
		leadPanel.redrawFrom(0)
	}

	contents+= title
	contents+= leadPanel
	contents+= controlPanel
	border = Swing.CompoundBorder(Swing.CompoundBorder(Swing.EmptyBorder(2), Swing.BeveledBorder(Lowered)), Swing.EmptyBorder(2))

	trait Selection
	{
		def select(row: Int)
	}

	case class MethodSelection(method: NamedMethod) extends Selection
	{
		def select(row: Int)
		{
			leadData.set(method)
		}
		override def toString = method.name
	}

	case class PNSelection(pn: PN) extends Selection
	{
		def select(row: Int)
		{
			leadData.set(row, pn)
		}
		override def toString = pn.toString
	}

	case class NoSelections(text: String) extends Selection
	{
		def select(row: Int)
		{
			// no-op
		}

		override def toString = text
	}

}