package net.snowtiger.methodmaker

import net.snowtiger.ringing.{MethodLibrary, NamedMethod, Row}

/**
 * Some additional stats over and above what's provided in MethodAnalysis, plus string output of a summary
 *
 * @author mark
 */
case class MethodSearchStats(method: NamedMethod)
{
	val nbells = method.nbells
	val handstrokeHL = method.firstLead(method.leadLength/2-1)
	val handstrokeSnapHL = method.firstLead(method.leadLength/2-3)

	lazy val nameLookup = MethodLibrary.delightLibraries.getLibrary(nbells).pnToName.getOrElse(method.lead,
		MethodLibrary.surpriseLibraries.getLibrary(nbells).pnToName.getOrElse(method.lead, ""))

	lazy val starIfHLIsPB =
	{
		if (handstrokeSnapHL.isPlainBob)
			("*"+noteOppositeStroke(handstrokeSnapHL))
		else if (handstrokeHL.isPlainBob)
			("**"+noteOppositeStroke(handstrokeHL))
		else
			""
	}

	lazy val stats: String =
	{
		val (nRollupTypes,frontBackInCommon) = unmatchedRollupScore(method)
		val pureForwardCoursing = (if (pureForwardPBCoursing(method)) "PPB" else "")

		method.lhGroup.padTo(4,' ')+ "," + method.name.padTo(20, ' ') + "," +
				cStats(4) + cStats(5) + cStats(6) +
				"R4=" + method.potential4RunsAndNearMisses + ", R5=" + method.count5Runs + ", R6=" + method.count6Runs + ", "+
				"D=" + method.difficulty + ", FCH=" + method.fch + ", " +
				nRollupTypes + ", " + frontBackInCommon + ", " + "OP=" + method.oddPointCount + ", " +
				pureForwardCoursing + ", " +
				method.outputPN()+" ,"+starIfHLIsPB+"  "+nameLookup
	}

	lazy val csvStats: String =
	{
		method.lhGroup+ "," + method.name + "," +
				cStatsRaw(4) + "," + cStatsRaw(5) + "," + cStatsRaw(6) + "," +
				method.potential4RunsAndNearMisses + "," + method.count5Runs + "," + method.count6Runs + ","+
				method.difficulty + "," + method.oddPointCount + "," + method.fch + "," +
				method.outputPN()+","+starIfHLIsPB
	}

	def noteOppositeStroke(row: Row) = if (Row.isCoursingPairRightHunting(row.bellAt(1), row.bellAt(2), nbells)) "H" else ""

	def cStats(size: Int) = "C"+size+"="+cStatsRaw(size)+", "
	def cStatsRaw(size: Int) = (method.firstLead.map{_.coursingScore(size)}.sum)

	def pureForwardPBCoursing(m: NamedMethod): Boolean =
	{
		// Exclude the treble and the course/after bells of the tenor
		val bellsPassed = removeDups(m.getBellsPassed.filter{(b)=> b > 1 && b < m.nbells-2})
		val firstOddBell = 2*(m.nbells/2)-3
		val lastEvenBell = 2*((m.nbells+1)/2)-4
		val mustPassIn = (firstOddBell to 3 by -2).toList ++ (2 to lastEvenBell by 2).toList
		val nTimesPassed = bellsPassed.size/mustPassIn.size
		bellsPassed == List.tabulate(nTimesPassed)((i)=> mustPassIn).flatten
	}

	private def removeDups[T](list: List[T]) =
	{
		if (list.isEmpty)
			list
		else
		{
			var revNew = List[T](list.head)
			for (elt <- list.tail)
				if (elt!=revNew.head)
					revNew = elt::revNew
			revNew.reverse
		}
	}

	/** Returns pair (nRollupTypes,frontBackInCommon) */
	def unmatchedRollupScore(m: NamedMethod): (Int, Int) =
	{
		def getRollups(pos: Int) =
		{
			val size = nbells-4
			val (startPos,endPos,seventhPos,tenorPos) = pos match
			{
				case 0 => (1,size, 2,1)
				case 1 => (1,size, size-1,size)
				case 2 => (nbells-size+1,nbells, nbells-size+2,nbells-size+1)
				case 3 => (nbells-size+1,nbells, nbells-1,nbells)
			}
			val rows = m.fullCourse.filter{(r)=> r.bellAt(seventhPos)==nbells-1 && r.bellAt(tenorPos)==nbells}
			var rollups = rows.map{_.extract(startPos, endPos)}.filter{_.placeOf(1)==0}
			if (tenorPos<seventhPos)
				rollups = rollups.map(_.reverse)
			rollups
		}

		val frontRollups = getRollups(0)++getRollups(1)
		val backRollups = getRollups(2)++getRollups(3)
		val allByCount = (frontRollups++backRollups).groupBy(identity).mapValues(_.size)
		val backRollupSet = backRollups.toSet
		val bothByCount = frontRollups.filter(backRollupSet).groupBy(identity).mapValues(_.size)
		(allByCount.size, bothByCount.values.sum)
	}

}