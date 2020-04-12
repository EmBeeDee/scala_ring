package net.snowtiger.laminated

import net.snowtiger.ringing.{Music, NamedMethod}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder
import net.snowtiger.spliced.tables.{Node, Splice}

/**
 * @author mark
 */

case class LaminatedNodeBuilder(method: NamedMethod, callingPositions: List[CallingPosition], coursingOrders: List[CoursingOrder], music: Array[Music])
{
	val nbells = method.nbells
	val stage = Major
	assert(nbells==stage.nbells)
	val plainCall: Call = if (method.is2ndsPlace) stage.Plain12 else stage.Plain1N

	val nodes: List[Node] = buildNodes()

	def buildNodes(): List[Node] =
	{
		coursingOrders.flatMap(buildNodes).sortWith((a,b)=> a.music > b.music)
	}

	def buildNodes(co: CoursingOrder): List[Node] =
	{
		var startRow = co.toCourseHead(nbells)
		val nodeType = NodeType(stage.Home, stage.Home, plainCall)
		val splice = new Splice(List.fill(nbells-1)(method))
		val node = Node(startRow, nodeType, splice)
		node.populateMusic(music)
		List(node)
	}

}