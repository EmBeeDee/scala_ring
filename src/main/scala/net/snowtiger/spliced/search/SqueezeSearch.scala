package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.tables.Node

import scala.collection.BitSet
import scala.collection.immutable.TreeSet

/**
 * Attempts to squeeze more music out of an existing composition by looking at the maximum music available in
 * each node, and, if higher than the node in the composition, attempting to insert it, fixing any falseness
 * that occurs. Note that overall score may still drop - e.g. ATW could be lost, or balance made worse.
 *
 * @author mark
 */
class SqueezeSearch(searchDef: SearchDefinition) extends SearchBase(searchDef)
{
	val maxFalse = 2

	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val seedComp = sortedSeeds.head
		analyzeBest(seedComp)
		squeezeComps(sortedSeeds)
	}

	def squeezeComps(sortedSeeds: TreeSet[Composition])
	{
		for (comp <- sortedSeeds)
			squeezeComp(comp)
	}

	def analyzeBest(comp: Composition)
	{
		out.println("Analyzing: "+comp)
		var totalBest = 0
		for (node <- comp.nodes)
		{
			val alternatives = tables.nodeAlternatives(node.startLH)
			val bestMusic = alternatives.maxBy(_.music(0))
			println(node.toString+", best= "+bestMusic)
			totalBest+= bestMusic.music(0)
		}
		println("Max music = "+totalBest)
	}

	def squeezeComp(comp: Composition)
	{
		for (pos <- 0 until comp.nodes.size)
		{
			val nodeToVary = comp.getNodeAt(pos)
			val nodeMusic = nodeToVary.music(0)
			val leadBits = comp.leadBitSetWithoutNode(pos)		// Required if replaceNode is to be called.
			val alternatives = tables.nodeAlternatives(nodeToVary.startLH).filter{_.music(0)>nodeMusic}
			for (altNode <- alternatives)
				squeezeNode(comp, pos, altNode, leadBits)
		}
	}

	def squeezeNode(comp: Composition, pos: Int, altNode: Node, leadBits: BitSet)
	{
		if (altNode.trueWith(leadBits))
		{
			keepBetter(comp, comp.replaceNode(pos, altNode))
		}
		else
		{
			val madeTrue = makeTrueBest(comp, pos, altNode, maxFalse)
			if (madeTrue.isDefined)
				keepBetter(comp, madeTrue.get)
		}
	}

	def keepBetter(oldComp: Composition, newComp: Composition) =
	{
		if (newComp.music(0)>oldComp.music(0))
		{
			if (newComp.isAtw)
				println("ATW music improvement: "+newComp)
			keepBest(newComp)
		}
	}
}