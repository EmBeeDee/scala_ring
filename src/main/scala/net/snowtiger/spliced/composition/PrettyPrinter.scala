package net.snowtiger.spliced.composition

import net.snowtiger.spliced.search.SearchDefinition
import net.snowtiger.spliced.tables.{Node, Tables}

import scala.io.Source

/**
 * @author mark
 */
class PrettyPrinter(searchDef: SearchDefinition)
{
	val compPlan = searchDef.getCompPlan
	val methods = searchDef.methods
	val tables = new Tables(searchDef)
	tables.buildPrepTables()
	println

	def print(file: Source)
	{
		val comps = new TableCompositionLoader(tables, methods).loadNodesFromFile(file)
		for (comp <- comps)	{ print(comp) ; println }
	}

	def print(nodes: List[Node])
	{
		val allCallingPositions = nodes.map{_.nodeType.end.name}.toSet
		// Iterate through all arrangements of calling positions, to find the one with fewest bracketted course ends.
		val allHeadingCombs = allCombs(List("H"), allCallingPositions - "H")
		val allComps = allHeadingCombs.map{PrettyComp.makeCombined(nodes, _)}
		val bestComp = allComps.minBy(_.nBrackets)
		bestComp.print()
		println(MusicCount.sumAll(nodes.map{genNodeMusic}))
	}

	def genNodeMusic(node: Node): MusicCount =
	{
		val leads = node.genLeads(tables.leadTable)
		MusicCount.sumAll(leads.map{_.music})
	}

	def allCombs(prefix: List[String], set: Set[String]): List[List[String]] =
	{
		if (set.isEmpty)
			List(prefix)
		else
		{
			val results = for (s <- set; newPrefix = s::prefix; newSet = set - s) yield allCombs(newPrefix, newSet)
			results.flatten.toList
		}
	}


}