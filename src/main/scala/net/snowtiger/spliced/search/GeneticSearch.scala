package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.tables.Node

import scala.collection.immutable.TreeSet
import scala.util.Random

/**
 * @author mark
 */

class GeneticSearch(searchDef: SearchDefinition) extends GreedySearch(searchDef)
{
	override def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val MaxChildren = 20
		val MaxBreedingPairs = 20
		//val MaxChildren = 20
		//val MaxBreedingPairs = 4

		out.println("Varying "+sortedSeeds.size+" comps, with best = "+sortedSeeds.head)

		var comps = sortedSeeds
		var nTriesScoreNoBetter = 0

		while (nTriesScoreNoBetter<15)
		{
			val t = System.currentTimeMillis()
			if (comps.size<MaxChildren || nTriesScoreNoBetter>2)
			{
				val oldScore = comps.head.score
				comps = varyAllComps(comps, MaxChildren, 0)._2
				if (comps.head.score>oldScore)
					nTriesScoreNoBetter = 0
			}
			val (nFoundThisTime, newChildren) = findMates(comps, MaxBreedingPairs, MaxChildren)
			val b = if (newChildren.isEmpty) "none found" else newChildren.head.toString
			out.println(nFoundThisTime+" comps found in "+(System.currentTimeMillis()-t)+"ms, best = "+ b)
			if (newChildren.head.score>comps.head.score)
			{
				nTriesScoreNoBetter = 0
				comps = newChildren
			}
			else
			{
				nTriesScoreNoBetter+= 1
			}
		}
	}

	def findMates(comps: TreeSet[Composition], nBreedingPairs: Int, maxChildren: Int) =
	{
		var nFound = 0
		var children = TreeSet[Composition]();
		val nParents = comps.size
		if (nParents>=2)
		{
			val parents = comps.toArray
			if (nParents*(nParents+1)/2 <= nBreedingPairs)
				for (i<-0 until nParents; j<-i+1 until nParents)
				{
					val newComps = mate(parents(i), parents(j))
					nFound+= newComps.size
					children = keepComps(newComps, children, maxChildren)
				}
			else
				for (i<- 0 until nBreedingPairs)
				{
					val newComps = mate(parents(Random.nextInt(nParents)), parents(Random.nextInt(nParents)))
					nFound+= newComps.size
					children = keepComps(newComps, children, maxChildren)
				}
		}
		(nFound, children)
	}

	def mate(male: Composition, female: Composition) =
	{
		def mate(first: List[Node], second: List[Node], revResults: List[List[Node]]): List[List[Node]] =
		{
			def combineOne(node: Node, nodes: List[Node]) = if (node.trueWith(nodes)) List(node::nodes) else Nil

			def combineTwo(node1: Node, node2: Node, nodes: List[Node]) =
			{
				if (node1.trueWith(nodes))
					(node1::nodes) :: combineOne(node2, nodes)
				else
					combineOne(node2, nodes)
			}

			if (first.isEmpty)
				revResults
			else
			{
				val head1 = first.head
				val head2 = second.head
				val newResults = if (head1==head2) revResults.flatMap{combineOne(head1, _)} else revResults.flatMap{combineTwo(head1, head2, _)}
				mate(first.tail, second.tail, newResults)
			}
		}
		val revResults = if (male==female) Nil else mate(male.nodes, female.nodes, List(Nil))
		//out.println("Mating resulted in "+revResults.size+" children.")
		revResults.map{(revNodes) => Composition(revNodes.reverse, tables)}
	}

}