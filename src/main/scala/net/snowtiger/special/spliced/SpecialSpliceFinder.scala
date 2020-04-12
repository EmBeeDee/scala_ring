package net.snowtiger.special.spliced

import net.snowtiger.ringing._
import net.snowtiger.spliced.score.MusicRun

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author mark
 */

object SpecialSpliceFinder extends SpecialMethods
{
	val PipeSearch = SearchParams(libraryMethods, SlinkyCyclicRows.allRows,
		List("1ET907856342", "14523ET90786", "189674523ET0", "1T20E8967453", "15634T20E897", "190785634T2E",
			"123ET9078564", "1674523ET908", "10E89674523T", "134T20E89675", "1785634T20E9"), Row("1234567890ET"))
	PipeSearch.tuning = SearchTuning(9, 11, 5, 8.4, 8.5, 0)

	// These two are for the series "Grandsire Cyclic Max 3". We start with Grandsire, and finish with handstroke home in the Surprise.
	// It is best if we finish with a 2nd's place method - then the last course will naturally finish at the handstroke course end.
	// With this finish, the best peal length comes from 8 leads in even courses and 6 leads in odd.
	val GrandsireNewEven = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsNew,
		List("165432TE0987", "12TE09876543", "1432TE098765", "18765432TE09", "1098765432TE", "1TE098765432"), Row("1798E02T4365"))
	GrandsireNewEven.lastLHMustBe = Some(PN("12"))
	GrandsireNewEven.tuning = SearchTuning(8, 8, 5, 9.0, 9.5, 0)
	val GrandsireNewOdd = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsNew,
		List("175634T20E89", "13T20e896745", "1534T20E8967", "19785634T20E", "1E90785634T2"), Row("1890ET234567"))
	GrandsireNewOdd.tuning = SearchTuning(6, 6, 5, 8.5, 9.0, 0)

	// These three are for the series "Grandsire Cyclic Max 4". We start with the Surprise, and finish with Grandsire in the handstroke home position.
	// We need to do the opening course separately, since it has direct falseness with the final Grandsire leads
	val GrandsireHandEven = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand,
		List("1809TE325476", "168709TE3254", "14658709TE32", "10TE32547698", "1T32547698E0"), Row("1765432TE098"))
	GrandsireHandEven.tuning = SearchTuning(7, 7, 5, 9.0, 9.5, 0)
	val GrandsireHandOdd = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand,
		List("17890ET23456", "134567890ET2", "1567890ET234", "190ET2345678", "1ET234567890"), Row("164523ET9078"))
	GrandsireHandOdd.tuning = SearchTuning(7, 7, 5, 9.0, 9.5, 0)
	val GrandsireHandOpening = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand,
		List("1234567890ET"), Row("1T0E89674523"))
		//List("1234567890ET","17890ET23456", "134567890ET2", "1567890ET234", "190ET2345678", "1ET234567890"), Row("1T0E89674523"))
	GrandsireHandOpening.tuning = SearchTuning(1, 10, 5, 5.5, 6.0, 0)
	val GrandsireHandOpen1 = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand, List("1234567890ET"), Row("1243658709TE"))	// 53246 H->H 52436
	GrandsireHandOpen1.endRowHandstroke = true
	val GrandsireHandOpen2 = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand, List("1423567890ET"), Row("124537698E0T"))	// 52436 H->W 24536
	GrandsireHandOpen2.endRowHandstroke = true
	val GrandsireHandOpen3 = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand, List("14257396E8T0"), Row("1534628709TE"))	// 24536 W->H 25346
	GrandsireHandOpen3.endRowHandstroke = true
	val GrandsireHandOpen4 = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand, List("1354267890ET"), Row("153247698E0T"))	// 25346 H->W 53246
	GrandsireHandOpen4.endRowHandstroke = true
	val GrandsireHandOpen5 = SearchParams(libraryMethods, GrandsireCyclicRows.allRowsHand, List("13527496E8T0"), Row("1T0E89674523"))	// 53246 H->B

	//override val knownMethods: Set[String] = Set("Bristol S","Counter's Creek D","Deimos A","Deira D","Phobos S","Strathclyde S","Strawberry S")

	def main(args: Array[String]): Unit =
	{
		//GrandsireNewOdd.tuning = SearchTuning(6, 6, 5, 6.5, 7.0, 20)
		GrandsireNewOdd.tuning = SearchTuning(6, 6, 0, 38.0, 0.0, 0)
		//GrandsireNewOdd.maxUnknownMethods = 3
		//GrandsireNewEven.tuning = SearchTuning(8, 8, 5, 7.5, 8.0, 0)
		GrandsireNewEven.tuning = SearchTuning(8, 8, 0, 70.0, 0.0, 0)
		////GrandsireNewEven.maxUnknownMethods = 3
		//search(GrandsireNewEven)

		PipeSearch.maxUnknownMethods = 2
		PipeSearch.tuning = SearchTuning(10, 10, 0, 105.0, 0.0, 0)
		search(PipeSearch)

		GrandsireHandOpen5.maxUnknownMethods = 0
		//searchSpecific(0, GrandsireHandOpen5, SearchTuning(1, 10, 5, 5.5, 6.0, 0))

		GrandsireHandOdd.maxUnknownMethods = 2
		GrandsireHandOdd.tuning = SearchTuning(8, 8, 5, 7.5, 8.0, 0)
		GrandsireHandEven.maxUnknownMethods = 0
		GrandsireHandEven.tuning = SearchTuning(8, 8, 5, 6.5, 7.0, 0)
		//search(GrandsireHandOdd)
	}

	val music =	new CompositeMusic(new MusicRun(8), new MusicRun(8), new MusicRun(8), new MusicRun(8), new MusicRun(5))
	val music8 = new MusicRun(8)

	val leadCache: mutable.Map[(NamedMethod,Row),MethodLead] = mutable.Map()
	val methodsFound: mutable.Set[NamedMethod] = mutable.Set()

	def searchAll(params: SearchParams): Unit =
	{
		for (startRow <- params.startRows)
			search(params.cloneForNewStartRow(startRow))
	}

	def searchSpecific(courseNum: Int, params: SearchParams, tuning: SearchTuning): Unit =
	{
		val specificParams = params.cloneForNewStartRow(params.startRows(courseNum))
		specificParams.tuning = tuning
		search(specificParams)
	}

	def search(params: SearchParams): Unit =
	{
		val startRow = params.startRow
		println("Search from "+startRow+" to "+params.endRow)
		search(params, startRow, Set(), Nil, 0, Set())
		println("Search complete. Methods used: "+methodsFound.map{_.namePlusClass}.toList.sorted.mkString(", "))
	}

	def search(params: SearchParams, lh: Row, rowsUsed: Set[Row], revMethodsSoFar: List[NamedMethod], musicSoFar: Int, unknownMethodsSoFar: Set[String]): Unit =
	{
		for (method <- params.methods)
		{
			val methodLead = leadCache.getOrElseUpdate( (method,lh), MethodLead(method, lh, params))
			if (methodLead.trueWithExisting)
			{
				val nextRowsUsed = rowsUsed++methodLead.rows
				if (nextRowsUsed.size == rowsUsed.size + methodLead.rows.size)
				{
					val tuning = params.tuning
					val nextMusic = musicSoFar + methodLead.mus
					val n = 1 + revMethodsSoFar.size
					if (methodLead.mus >= tuning.minMusicPerLead && nextMusic.toFloat/n >= tuning.minIntermediateDensity)
					{
						val nextRevMethods = method :: revMethodsSoFar
						val namePlusClass = method.namePlusClass
						val nextUnknownMethods = if (knownMethods(namePlusClass)) unknownMethodsSoFar else unknownMethodsSoFar+namePlusClass
						if (params.maxUnknownMethods<0 || nextUnknownMethods.size<=params.maxUnknownMethods)
						{
							if (methodLead.checkRow()==params.endRow)
								output(params, nextRevMethods.reverse, nextRowsUsed, nextMusic, nextUnknownMethods)
							else if (n < tuning.maxLeads)
								search(params, methodLead.nextLH, nextRowsUsed, nextRevMethods, nextMusic, nextUnknownMethods)
						}
					}
				}
			}
		}
	}

	def output(params: SearchParams, splice: List[NamedMethod], rows: Set[Row], musicCount: Int, unknownMethods: Set[String]): Unit =
	{
		if (splice.size>=params.tuning.minLeads)
			if (params.lastLHMustBe.isEmpty || splice.last.leadheadPN==params.lastLHMustBe.get)
				if (params.mustHaveRow.isEmpty || rows(params.mustHaveRow.get))
				{
					val density = musicCount.toFloat/splice.size
					if (density>=params.tuning.minFinalDensity)
					{
						val run8 = calcRun8Score(params, splice)
						if (run8 >= params.tuning.minRun8)
						{
							val densityStr = f"$density%1.2f"
							println(splice.size+"\t"+densityStr+"\t"+musicCount+"\t"+run8+"\t"+splice.map{_.abbrev}.mkString(","))
							methodsFound++= splice
						}
					}
				}
	}

	private def calcRun8Score(params: SearchParams, splice: List[NamedMethod]): Int =
	{
		var lh = params.startRow
		var mus8 = 0
		for (method <- splice)
		{
			val methodLead = leadCache( (method,lh) )
			mus8+= methodLead.mus8
			lh = methodLead.nextLH
		}
		mus8
	}

	case class MethodLead(method: NamedMethod, lh: Row, params: SearchParams)
	{
		val (le, nextLH, rows, mus, mus8) = genLead

		private def genLead =
		{
			val buf = new ListBuffer[Row]()
			val nextLH = method.generateLead(lh, buf)
			val le = buf.last
			for (perm <- params.extraRowPerms)
			{
				val extraLH = lh.permuteBy(perm)
				method.generateLead(extraLH, buf)
			}
			if (buf.last.isRounds)
				buf.remove(buf.size-1)
			val mus = music.countMusic(buf)
			val mus8 = music8.countMusic(buf)
			(le, nextLH, buf.toSet, mus, mus8)
		}

		lazy val trueWithExisting = rows.forall{!params.existingRows(_)}

		def checkRow() = if (params.endRowHandstroke) le else nextLH
	}

	case class SearchParams(methods: List[NamedMethod], existingRows: Set[Row], startRowStrs: List[String], endRow: Row)
	{
		val startRows = startRowStrs.map{Row(_)}
		val startRow = startRows.head
		val extraRowPerms = startRows.tail.map{startRows.head.permutationBetween(_)}

		var endRowHandstroke = false
		var mustHaveRow: Option[Row] = None
		var lastLHMustBe: Option[PN] = None

		var tuning: SearchTuning = SearchTuning(1, 11, 5, 0.0, 0.0, 0)
		var maxUnknownMethods = -1

		def cloneForNewStartRow(newStartRow: Row): SearchParams =
		{
			val endRowPerm:Perm = endRow.permuteBy(startRow.toPerm.inverse).toPerm
			val clone = SearchParams(methods, existingRows, List(newStartRow.toString), newStartRow.apply(endRowPerm))
			clone.endRowHandstroke = endRowHandstroke
			clone.mustHaveRow = mustHaveRow
			clone.lastLHMustBe = lastLHMustBe
			clone.tuning = tuning
			clone.maxUnknownMethods = maxUnknownMethods
			clone
		}
	}

	case class SearchTuning(minLeads: Int, maxLeads: Int, minMusicPerLead: Int, minIntermediateDensity: Double, minFinalDensity: Double, minRun8: Int)

}
