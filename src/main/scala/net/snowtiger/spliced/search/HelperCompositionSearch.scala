package net.snowtiger.spliced.search

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition.{Composition, CompositionPlan, HelperCompSeed}
import net.snowtiger.spliced.tables.Node

import scala.collection.immutable.TreeSet

/**
 * Attempts to use the best composition from another (closely-related) calling to supply good nodes and splices
 * for the current calling. Where the "helper" composition doesn't have a suitable node, varies exhaustively
 * to try and complete the composition. Note that the seed should supply the helper composition.
 *
 * @author mark
 */
class HelperCompositionSearch(searchDef: SearchDefinition, helperPlan: CompositionPlan, helperCompFile: String) extends SearchBase(searchDef)
{
	val helperMap = buildHelperNodeMap()

	/** Builds map of LH -> splice-string. All LHs in the helper node are keyed, so we can jump into the middle of a node */
	def buildHelperNodeMap(): Map[Row,String] =
	{
		val helperNodes = new HelperCompSeed(searchDef, helperCompFile).getHelperSeed(helperPlan)
		def allLHs(n: Node) = n.toLeads.zipWithIndex.map{ (pair) => (pair._1.startLH, n.methods.drop(pair._2).mkString)}
		val helperPairs = helperNodes.flatMap{ (n)=> allLHs(n) }
		Map()++helperPairs
		// Maybe should try and identify split courses, and combine back into a single node where possible.
	}

	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition]) =
	{
		assert(searchDef.getCompPlan.partend==helperPlan.partend)
		assert(searchDef.getCompPlan.singleMethod==helperPlan.singleMethod)
		for (comp <- sortedSeeds)
		{
			helpComp(comp).foreach{ keepBest(_) }
		}
	}

	def helpComp(mainComp: Composition) =
	{
		val replacedNodes = mainComp.nodes.map{ findHelperNode(_) }
		val newComp = Composition(replacedNodes.map{_._1}, tables)
		val (replacedPairs, originalPairs) = replacedNodes.partition{_._2}
		val original = originalPairs.map{_._1}
		val replaced = replacedPairs.map{_._1}
		println(replaced.size+" nodes replaced "+original.size+" irreplaceable in composition "+mainComp)
		if (original.size==0)
			List(newComp)
		else
			remakeFalseComp(newComp, replaced, original, false)
	}

	def findHelperNode(mainNode: Node) =
	{
		val startLH = mainNode.startLH
		if (helperMap.contains(startLH))
		{
			val helperSpliceStr = helperMap(startLH)
			// We try and find a node alternative which is a prefix of the helper node.
			// This allows us to find sub-nodes of the helper node, but doesn't cover the case where two helper nodes could be combined.
			val alternatives = tables.nodeAlternatives(startLH).filter{ (n)=> helperSpliceStr.startsWith(n.methodString) }
			if (alternatives.isEmpty)
				(mainNode, false)
			else
				(alternatives.head, true)
		}
		else
		{
			(mainNode, false)
		}
	}
}