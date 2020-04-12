package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.tables.Node

import scala.collection.immutable.TreeSet


/**
 * First method in list must be the one from the single-method composition.
 * @author mark
 */
class GreedySearch(searchDef: SearchDefinition) extends SearchBase(searchDef)
{
	/**
	 * Vary the set of comps in a series of annealed "levels", finishing only when no more improvements can be found.
	 * @param sortedSeeds
	 * @return
	 */
	def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val QuickRun = true

		val MaxPerLevel = if (QuickRun) 50 else 500
		val MaxFalseNodes = 2

		var nTriesScoreNoBetter = 0
		var tryToMakeTrue = false
		var highScore = sortedSeeds.head.score

		def vary(prevComps: TreeSet[Composition], level: Int): TreeSet[Composition] =
		{
			val t = System.currentTimeMillis()
			val maxFalse = if (tryToMakeTrue) MaxFalseNodes else 0
			val (nFoundThisTime, newComps) = varyAllComps(prevComps, MaxPerLevel, maxFalse)

			// No new comps produced (unlikely!) - return empty list to finish search
			if (newComps.isEmpty)
				TreeSet()
			else
			{
				// If best new comp not as good as best previous, enter final tries: repeat normal search twice,
				// then have a couple of goes with "makeTrue" in place. If at any stage we get an improvement, go
				// back to the normal search. If after five "final" tries we still have no improvement, finish.
				if (newComps.head.score<=highScore)
					nTriesScoreNoBetter+= 1
				// Switch on "make true" behaviour if we're struggling to get better results
				if (nTriesScoreNoBetter>1 && !QuickRun)
					tryToMakeTrue = true
				if (nTriesScoreNoBetter>4)
					TreeSet()
				else
				{
					// See if we have reached a new high score
					if (newComps.head.score>highScore)
					{
						highScore = newComps.head.score
						nTriesScoreNoBetter = 0
						tryToMakeTrue = false
					}
					// Prune list if too big
					val max = if (tryToMakeTrue) MaxPerLevel/2 else MaxPerLevel
					val prunedNew = if (newComps.size>max) newComps.slice(0, max) else newComps
					out.println("Level "+level+" comps found: "+nFoundThisTime+" in "+(System.currentTimeMillis()-t)+"ms, now varying "+prunedNew.size+"; best = "+prunedNew.head)
					prunedNew
				}
			}
		}

		out.println("Varying "+sortedSeeds.size+" comps:")
		var forcedTrue = sortedSeeds.flatMap(forceTouchTrue(_))
		if (forcedTrue.isEmpty)
		{
			out.println("No true comps found")
		}
		else
		{
			var level = 1
			var comps = TreeSet[Composition]()
			for (comp <- forcedTrue)
				comps+= comp
			while (!comps.isEmpty)
			{
				comps = vary(comps, level)
				level+= 1
			}
		}
	}

	def varyAllComps(comps: TreeSet[Composition], maxComps: Int, maxFalse: Int) =
	{
		var nFoundThisTime = 0
		var newComps = TreeSet[Composition]()
		for (comp <- comps)
		{
			val (n, comps) = varyAllNodes(Nil, comp.nodes, newComps, maxComps, maxFalse, 0)
			newComps = comps
			nFoundThisTime+= n
		}
		(nFoundThisTime, newComps)
	}

	def varyAllNodes(preRev: List[Node], post: List[Node], variations: TreeSet[Composition], maxComps: Int, maxFalse: Int, nFound: Int): (Int, TreeSet[Composition]) =
	{
		var newNFound = nFound
		if (post.isEmpty)
			(newNFound, variations)
		else
		{
			val nodeToVary = post.head
			var newVariations = variations
			val pre = preRev.reverse
			val tail = post.tail
			val leadBits = Node.leadBitSet(pre, Node.leadBitSet(tail))
			for (node <- tables.nodeAlternatives(nodeToVary.startLH))
			{
				if (node.n!=nodeToVary.n)
				{
					val newComp = Composition(pre++(node::tail), tables)
					if (node.trueWith(leadBits))
					{
						newVariations = keepComp(newComp, newVariations, maxComps)
						newNFound+= 1
					}
					else if (maxFalse>0)
					{
						val newComps = makeTrue(newComp, pre, node, tail, maxFalse)
						if (newComps.isDefined)
						{
							newVariations = keepComps(newComps.get, newVariations, maxComps)
							newNFound+= newComps.get.size
						}
					}
				}
			}
			varyAllNodes(nodeToVary::preRev, post.tail, newVariations, maxComps, maxFalse, newNFound)
		}
	}


}