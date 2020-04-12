package net.snowtiger.spliced.atw.gui

import scala.swing._

/**
 * @author mark
 */
class AtwFrame extends ScrollPane
{
	val groupedMethodPanels = AtwUI.model.comp.unflattenByCourse((0 until 23).toList.map{new MethodPanel(_)})
	viewportView = new GridPanel(2, 2){
		for (methodPanels<-groupedMethodPanels)
		  contents+= new CoursePanel(methodPanels)
		border = Swing.EmptyBorder(2, 2, 20, 30)
	}

	def refreshAll()
	{
		for (methodPanels<-groupedMethodPanels)
			for (methodPanel<-methodPanels)
				methodPanel.refreshAll()
	}

  class CoursePanel(methodPanels: List[MethodPanel]) extends BoxPanel(Orientation.Horizontal)
	{
		for (panel <- methodPanels)
			contents+= panel
		border = Swing.EmptyBorder(20, 30, 2, 2)
	}
}