package net.snowtiger.spliced.atw.gui

import java.awt.Font

import scala.swing.Label

/**
 * @author mark
 */

class RowPanel(row: String) extends Label(row)
{
	font = new Font("Monospaced", 0, 16)
}