package net.snowtiger.spliced.graph

import net.snowtiger.spliced.StandardMethods
import net.snowtiger.spliced.composition.Major

/**
 * @author mark
 */

object Regan6Graph extends StandardMethods
{
	val methods = List(yorkshire, superlative, cambridge, london, lessness, bristol)
	val callingPositions = List(Major.Home, Major.Wrong, Major.Middle)
	val calls = List(Major.Bob)

	def main(args: Array[String]): Unit =
	{
		val graph = new SplicedGraph(methods, callingPositions, calls)
		graph.build()
		graph.traverse()
	}
}