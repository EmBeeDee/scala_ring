package net.snowtiger.spliced.atw.gui

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, Orientation}

/**
 * @author mark
 */

class ControlPanel(methodPanel: MethodPanel) extends BoxPanel(Orientation.Horizontal)
{
	val button = new Button("Reset")
	contents+= button
	listenTo(button)
	reactions += {
		case ButtonClicked(b) => methodPanel.reset()
	}
}