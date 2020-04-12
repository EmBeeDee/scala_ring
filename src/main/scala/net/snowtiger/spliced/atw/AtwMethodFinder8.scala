package net.snowtiger.spliced.atw

import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.atw.construct._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * Uses a fixed spliced (in natural splice order) and constructs methods to fit the composition,
 * based on two sets of tables, one for the first three sections of the halflead (the stalactites) and one for
 * the last section of the halflead, from all possible halflead rows (the stalagmites).
 * A probe search of the stalactites can then rapidly be completed using the stalagmites.
 * <p>
 * Altered April 2015 to append the stalagmites to the stalactites during table build of the latter, to generate
 * completed method "columns". The search can then proceed through all possible methods. This has the advantage
 * of sorting out the links between the 'tites and 'mites, which was very difficult to get right (in particular,
 * avoiding invalid PN transitions tricky), and should be more efficient overall; however it does not find the
 * first solutions as quickly as the probe search based on 3 sections followed by a completion search on the 1-section
 * stalagmites. Of course, this original split at 3 sections in could be mimicked with the right algorithm for
 * selecting the next node to traverse (although might be trickier to do the backtrack pruning).
 *
 * @author mark
 */
class AtwMethodFinder8(nbells: Int)
{
	assert(nbells == 8)

	var search2Hiwater = 0

	/** Used from the old {@link AtwMethodFinder} */
	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		//val lhGroups = "b,f,b,b,a,a, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,b,b,b, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,b,b,b, a,b,b,f,b,a, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}

		//val lhGroups = "f,l,f,f,f,f, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,l,d,b,d, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,j,f,d, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "a,j,f,f,d,a, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}

		val lhGroups = "b,d,b,j,f,d".split(',').toList ::
				comp.courses.tail.zipWithIndex.map
				{ (p) => p._1.mostVariedSplices(p._2 * 1).map
						{
							_.toLower.toString
						}.toList
				}
		buildMethods(comp, lhGroups)
	}

	/** Used from the new {@link AtwMethodBuilder} */
	def buildMethods(comp: InputComp, splices: List[List[String]])
	{
		println(splices.map
		{
			_.mkString(" ")
		}.mkString(", "))
		val outputter = new CompositionOutputter(comp, splices)
		println(outputter.output)

		var t = System.currentTimeMillis()
		println("Building method courses/halfleads...")
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		val methodLeads = splices.zipWithIndex.map
				{ (p) => AtwMethodBuilder.buildMethodLeads(p._2, cos(p._2), p._1) }.flatten
		println("Building composition rotation music per row")
		AtwMethodBuilder.buildCompMusicForRows(methodLeads)

		// Build stalagmites first, so we can discard stalactites which don't join up
		println("Building stalagmites ...")
		var stalagmites = new StalagmiteBuilder(methodLeads).stalagmites
		println("Building stalactites ...")
		val stalactites = new StalactiteBuilder(methodLeads, stalagmites).stalactites
		stalagmites = null
		// Free up space used by row->compMusic map.
		AtwMethodBuilder.clearCompMusic
		t = (System.currentTimeMillis() - t) / 1000
		println("Table build finished in " + t + "s")

		println()
		t = System.currentTimeMillis()
		val searchPositions = stalactites.map
				{
					SearchPosition(_)
				}.toVector
		search(searchPositions)
		t = (System.currentTimeMillis() - t) / 1000
		println("Search finished in " + t + "s")
	}

	def search(searchPositions: Vector[SearchPosition]): Unit =
	{
		//val searchState = new LayerPrunedSearchState(searchPositions, solution)
		val searchState = new MusicPrunedPrefixVarietySearchState(searchPositions, solution)
		//val searchState = new PrefixVarietySearchState(searchPositions, solution)
		//val searchState = new SliceSearchState(searchPositions, solution)
		searchState.search()
		println("Total solutions = " + searchState.totalSolutions)
		/*
		for ( (pns,pos) <- visitedPnPrefixes.zip(searchState.courses))
			println("allowMethods(\""+pos.root+" "+pns.mkString(" ")+"\")")
		*/
	}

	var bestMethodScore = 0
	var bestMethodScoreForBestComp = 0
	var bestCompScoreForBestMethods = CompMusic()
	var bestCompScore = CompMusic()
	var bestCombinedScore = 0

	def solution(state: SliceSearchState): Unit =
	{
		val finishedComp = new FinishedComposition(state)
		val methodScore = finishedComp.methodMusic
		val compScore = finishedComp.bestCompMusic._1
		val combinedScore = methodScore / 10 + compScore.total
		var doPrint = false
		if (combinedScore >= bestCombinedScore)
		{
			doPrint = true
			bestCombinedScore = combinedScore
		}
		if (compScore > bestCompScore || (compScore == bestCompScore && methodScore >= bestMethodScoreForBestComp))
		{
			doPrint = true
			bestCompScore = compScore
			bestMethodScoreForBestComp = methodScore
		}
		else if (compScore.isGoodEnough)
		{
			doPrint = true
		}
		if (methodScore > bestMethodScore || (methodScore == bestMethodScore && compScore >= bestCompScoreForBestMethods))
		{
			//doPrint = true
			bestCompScoreForBestMethods = compScore
			bestMethodScore = methodScore
		}
		if (doPrint)
			println("*** SOLUTION: " + finishedComp)
	}

	val visitedPnPrefixes = Array.fill(23)(Set[String]())

	def solution2(state: SliceSearchState): Unit =
	{
		for (i <- 0 until 23)
			visitedPnPrefixes(i) = visitedPnPrefixes(i)++state.courses(i).headSlice.pnStrings
	}

}