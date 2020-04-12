package net.snowtiger.spliced.atw.gui

import scala.swing._

/**
 * @author mark
 */

object AtwUI extends SimpleSwingApplication
{
	val model = new AtwModel

	def top = new MainFrame {
		title = "ATW-23"
		contents = new AtwFrame()
		menuBar = new AtwMenu()
	}

	class AtwMenu extends MenuBar
	{
		contents+= new Menu("Tools"){
			contents+= new MenuItem(Action("Finish Composition"){
				model.finishComposition()
			})
		}
	}

}