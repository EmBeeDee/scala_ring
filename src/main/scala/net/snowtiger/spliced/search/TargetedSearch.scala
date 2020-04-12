package net.snowtiger.spliced.search

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition.Composition

import scala.collection.immutable.TreeSet
import scala.collection.mutable

/**
 * Finds the "best" node alternatives (ratio of music*com:falseness highest) and attempts to wire them together
 * into a true composition.
 *
 * @author mark
 */
class TargetedSearch(searchDef: SearchDefinition) extends SearchBase(searchDef)
{
	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition]) =
	{
		tables.time("Ranking nodes", tables.rankNodes())
		var comp = sortedSeeds.head.toImmutableComp
		val lhToNodeNum = Map() ++ compPlan.calling.zipWithIndex.map{(p)=> p._1.startLH->p._2}
		// Assumes nodeAlts are already sorted
		var bestNodeAlts = tables.nodeAlternatives.toList.sortBy{-_._2.head.rank}.toList
		val leadBits = mutable.BitSet()
		var replacedNodeheads = mutable.Set[Row]()
		while (!bestNodeAlts.isEmpty && replacedNodeheads.size<5)
		{
			val node = bestNodeAlts.head._2.head
			if (node.trueWith(leadBits))
			{
				leadBits++= node.leads
				replacedNodeheads+= node.startLH
				comp = comp.replaceNode(lhToNodeNum(node.startLH), node)
			}
			bestNodeAlts = bestNodeAlts.tail
		}
		out.println((replacedNodeheads.size)+" nodes replaced: "+comp)
		val (replacedNodes, originalNodes) = comp.nodes.partition{(n)=> replacedNodeheads.contains(n.startLH)}
		val trueComps = remakeFalseComp(comp, replacedNodes, originalNodes, true)
		out.println("True comp: "+trueComps)
	}

	// Pick the best node alternatives, add to composition until there are no more best alts which are true against those already chosen
}