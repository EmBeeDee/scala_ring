package net.snowtiger.spliced.tables

import java.io.PrintStream

/**
 * @author mark
 */
class TableBuildProgress(dotEvery: Int, out: PrintStream)
{
	def this(dotEvery: Int) = this(dotEvery, System.out)

	var c = 0

	def emit
	{
		c += 1;
		if (c%dotEvery==0)
			out.print(".")
		if (c/dotEvery >= 200)
		{
			c = 0;
			out.println()
		}
	}
}
