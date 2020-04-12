package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition

import scala.collection.immutable.TreeSet

/**
 * @author mark
 */

class OneNudgeSearch(searchDef: SearchDefinition) extends GreedySearch(searchDef)
{
	/**
	 * Just do one round of node varying
	 * @param sortedSeeds
	 * @return
	 */
	override def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val MaxPerLevel = 50

		var nTriesScoreNoBetter = 0
		var highScore = sortedSeeds.head.score

		def vary(prevComps: TreeSet[Composition], level: Int): TreeSet[Composition] =
		{
			val t = System.currentTimeMillis()
			val (nFoundThisTime, newComps) = varyAllComps(prevComps, MaxPerLevel, 0)

			val max = 100
			val prunedNew = if (newComps.size>max) newComps.slice(0, max) else newComps
			out.println("Level "+level+" comps found: "+nFoundThisTime+" in "+(System.currentTimeMillis()-t)+"ms, now varying "+prunedNew.size+"; best = "+prunedNew.head)
			prunedNew
		}

		out.println("Varying "+sortedSeeds.size+" comps:")
		var comps = vary(sortedSeeds, 1)
		comps
		//vary(comps, 2)
	}


}