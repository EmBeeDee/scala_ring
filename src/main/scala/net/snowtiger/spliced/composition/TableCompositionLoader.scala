package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.tables.{Node, Tables}

import scala.collection.immutable.TreeSet
import scala.io.Source

/**
 * Loads compositions from file. Interns Nodes to the given Tables instance, and creates fully-fledged Compositions
 * with full music counts etc. The file compositions must therefore exactly match the CompositionPlan used to build
 * the tables.
 *
 * @author mark
 */
class TableCompositionLoader(tables: Tables, methods: List[NamedMethod]) extends CompositionLoader(tables.compPlan, methods)
{
	def loadFromFile(file: Source) =
	{
		val comps = loadNodesFromFile(file).map{makeComp(_)}
		TreeSet[Composition]() ++ comps
	}

	private def makeComp(nodes: List[Node]) =
	{
		val comp = Composition(nodes.map{addNodeToTable(_)}, tables)
		//assert(comp.isTrue, "False comp: "+comp)
		//println(comp)
		comp
	}

	protected def addNodeToTable(node: Node) =
	{
		val internedNode = tables.nodeTable.intern(node)
		if (!internedNode.isInTable)
		{
			tables.addToTable(internedNode)
			if (!internedNode.isInTable)
				throw new Exception("Failed to parse comp; node is false: " + node)
		}
		internedNode
	}
}

