package net.snowtiger.spliced.search

import net.snowtiger.spliced.composition.Composition

import scala.collection.immutable.TreeSet

/**
 * @author mark
 */

class StochasticTunnellingSearch(searchDef: SearchDefinition, maxK: Int, timeLimit: Long) extends SimulatedAnnealingSearch(searchDef)
{
	def this(searchDef: SearchDefinition) = this(searchDef, 100000000, -1L)

	val baseK = 2.0
	//val kMult = 1.2
	//val kMult = 1.4
	val kMult = 2.0

	//val baseK = 20.0
	//val kMult = 1.4
	//val kMult = 2.0

	var searchHistory: List[SearchHistory] = Nil


	override def varyCompsUntilScoreMaximized(sortedSeeds: TreeSet[Composition])
	{
		val seedComp = sortedSeeds.head
		highestMusicComp = seedComp
		highestMusic = seedComp.music
		tunnelSearch(seedComp)
	}

	def tunnelSearch(comp: Composition)
	{
		out.println("Seeding from: "+comp.toString)
		val t = System.currentTimeMillis()
		var newT = t
		var thisK = baseK
		var current = comp
		var nSearches = 0

		while (thisK<maxK && (timeLimit<0 || newT-t < timeLimit))
		{
			val startTime = System.currentTimeMillis()
			val intK = thisK.toInt
			val newComp = search(current, intK)
			val duration = System.currentTimeMillis() - startTime
			val perfString = " ["+{if (duration==0) "inf" else ""+(intK.toLong*1000/duration)}+" moves/s]"
			keepBest(newComp)
			if (newComp.compare(current)<0)
			{
				out.println("Seeding from new best: "+newComp +perfString)
				if (out!=System.out)
					print(".")
				current = newComp
				searchHistory = SearchHistory(newComp.score, thisK, 1+nSearches)::searchHistory
				thisK = baseK
			}
			else
			{
				thisK*= kMult
				nSearches+= 1
				out.println("Score is still "+current.score+", reseeding with k="+thisK+" from "+newComp +perfString)
			}
			newT = System.currentTimeMillis()
		}
	}

	override def outputSearchHistory()
	{
		if (searchHistory.size==0)
			println("Search failed to improve score")
		else
			println("Search took "+searchHistory.size+" steps to reach best score "+searchHistory.head.score)
	}

	case class SearchHistory(score:Int, k: Double, nSearches: Int)
}