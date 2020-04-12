package net.snowtiger.spliced.graph

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.{Call, CallingPosition}
import net.snowtiger.spliced.tables.{Numberer, TableBuildProgress}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author mark
 */

class SplicedGraph(methods: List[NamedMethod], callingPositions: List[CallingPosition], calls: List[Call])
{
	val out = System.out

	val nbells = methods.head.nbells
	val rounds = Row(nbells)

	val nodes = ListBuffer[GraphNodeDefinition]()
	val nodeHeads = mutable.Map[Row,ListBuffer[GraphNodeDefinition]]()
	val graphTable = new Numberer[GraphNodeDefinition]()


	def build(): Unit =
	{
		print("Creating nodes")
		time(" node build", buildNodes(rounds))
		println(nodes.size+" nodes created.")

		print("Building false")
		time(" false build", buildFalse(nodes.toList))

		print("Populating links")
		time(" link build", buildLinks())
	}

	def traverse(): Unit =
	{
		val graph = new NodeGraph(nodes.toSet)
		val startNode = nodeHeads(rounds).head
		println("Traversing "+nodes.size+" nodes starting from "+startNode)
		graph.traverse(startNode)
	}

	val nodesProgress = new TableBuildProgress(1000, out)

	def buildNodes(startRow: Row): Unit =
	{
		for (method <- methods)
			buildSingleMethodNodes(startRow, method)
	}

	def buildSingleMethodNodes(startRow: Row, method: NamedMethod): Unit =
	{
		var rows = ListBuffer[Row]()
		var splice = ""

		def recurseLead(lh: Row): Unit =
		{
			splice+= method.abbrev
			val nextLH = method.generateLead(lh, rows)
			val le = rows.last
			for (call <- calls)
			{
				val callLH = le.apply(call.pn)
				val tenorPos = callLH.placeOf(nbells)
				callingPositions.find(_.positionReached(tenorPos)) match {
					case Some(cp) => addNode(call, cp, callLH)
					case None =>
				}
			}
			if (nextLH!=startRow)
				recurseLead(nextLH)
		}

		def addNode(call: Call, cp: CallingPosition, callLH: Row): Unit =
		{
			val rowsSet = rows.toSet
			if (rowsSet.size==rows.size)
			{
				nodesProgress.emit
				val nodeHead = rows.head
				val node = new GraphNodeDefinition(splice + call, nodeHead, callLH)
				node.setRows(rowsSet)
				nodes+= node
				graphTable.intern(node)
				val nodelist = nodeHeads.getOrElseUpdate(nodeHead, ListBuffer())
				nodelist+= node
				if (!nodeHeads.contains(callLH))
					buildNodes(callLH)
			}
		}

		recurseLead(startRow)
	}

	val falseProgress = new TableBuildProgress(30, out)

	def buildFalse(nodes: List[GraphNodeDefinition]): Unit =
	{
		if (!nodes.isEmpty)
		{
			val node = nodes.head
			val remaining = nodes.tail
			for (other <- remaining)
				if (other.isFalseWith(node))
				{
		      other.falseNodes+= node
					node.falseNodes+= other
				}
			falseProgress.emit
			buildFalse(remaining)
		}
	}

	val linksProgress = new TableBuildProgress(100, out)

	def buildLinks(): Unit =
	{
		for (node <- nodes)
		{
			node.links ++= nodeHeads(node.endRow).filter(_.canFollow(node))
			linksProgress.emit
		}
	}

	def time(name: String, fnToTime: => Unit)
	{
		val t = System.currentTimeMillis()
		fnToTime
		out.println(name+" in "+(System.currentTimeMillis()-t)+"ms")
	}

}