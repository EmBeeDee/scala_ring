package net.snowtiger.spliced.graph

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.tables.Numberable


/**
 * @author mark
 */

class GraphNodeDefinition(val desc: String, val startRow: Row, val endRow: Row) extends Numberable
{
	var links = Set[GraphNodeDefinition]()
	var falseNodes = Set[GraphNodeDefinition]()
	var isRounds = startRow.isRounds
	var rows: Set[Row] = _
	var length = 0

	def setRows(rows: Set[Row]): Unit =
	{
		this.rows = rows
		length = rows.size
	}

	def isFalseWith(other: GraphNodeDefinition): Boolean = rows.exists(other.rows)

	def canFollow(other: GraphNodeDefinition): Boolean = desc.head!=other.desc.head && !isFalseWith(other)

	override def toString = desc
}

case class GraphNode(n: Int, links: Set[GraphNodeDefinition])
{
	override def hashCode() = n

	override def canEqual(that: Any) = that.isInstanceOf[GraphNode]

	override def equals(obj: scala.Any) = canEqual(obj) && n==(obj.asInstanceOf[GraphNode].n)
}

class NodeGraph(allNodes: Set[GraphNodeDefinition])
{
	def traverse(startNode: GraphNodeDefinition): Unit =
	{
		traverseSimple(startNode, allNodes, Nil)
	}

	def traverseSimple(nodeDef: GraphNodeDefinition, nodesLeft: Set[GraphNodeDefinition], revComp: List[GraphNodeDefinition]): Unit =
	{
		val newComp = nodeDef::revComp
		val newNodesLeft = nodesLeft--nodeDef.falseNodes
		for (nextDef <- nodeDef.links)
			if (newNodesLeft(nextDef))
			{
				if (nextDef.isRounds)
					output(revComp)
				else
					traverseSimple(nextDef, newNodesLeft, newComp)
			}
	}

	/*
	def traversePruned(nodeDef: GraphNodeDefinition, graph: Map[Int,GraphNode], revComp: List[GraphNodeDefinition]): Unit =
	{
		val newComp = nodeDef::revComp
		val node = graph(nodeDef.n)
		val newGraph = pruneGraph(nodeDef.falseNodes, node, graph)

	}

	def pruneGraph(nodeDef: GraphNodeDefinition, graph: Map[Int,GraphNode]): Map[Int,GraphNode] =
	{
		val falseNodes = nodeDef.falseNodes
		var newGraph = graph
		var nodesVisited = Map[GraphNode,Boolean]()

		def pruneLinks(node: GraphNode): Boolean =
		{
			nodesVisited+= node->false
			var newLinks = Set[GraphNodeDefinition]()
			for (link <- node.links)
			{
				val linkNode = graph(link.n)
				nodesVisited.get(linkNode) match
				{
					case Some(good) => if (good) newLinks += link
					case None => if (!falseNodes(link) && pruneLinks(linkNode)) newLinks += link
				}
			}
			val canReachRounds = !newLinks.isEmpty
			if (canReachRounds)
			{
				newGraph += node.n -> GraphNode(node.n, newLinks)
				nodesVisited+= node->true
			}
			canReachRounds
		}

		pruneLinks()

		def pruneGraph(nodeDef: GraphNodeDefinition): Boolean =
		{
			if (nodeDef.isRounds)
				true
			else
			{
				val node = graph(nodeDef.n)
				nodesVisited.get(node) match {
					case Some(canReachRounds) => canReachRounds
					case None =>
				}
				if (!nodesVisited(node))
				{
					nodesVisited+= node
					val falseNodes = nodeDef.falseNodes
					val newLinks = node.links.filter()
				}
			}
		}

	}
	*/

	def output(revComp: List[GraphNodeDefinition]): Unit =
	{
		val length = revComp.map(_.length).sum
		println(length+" "+revComp.reverse.mkString(" "))
	}

}