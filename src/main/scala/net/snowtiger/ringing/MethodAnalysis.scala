package net.snowtiger.ringing

import java.io.ByteArrayOutputStream
import java.util.zip.{ZipEntry, ZipOutputStream}

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.Row.Rounds


/**
 * @author mark
 */
trait MethodAnalysis
{
	this: Method =>

	lazy val treblePositions = firstLead.map{_.placeOf(1)}

	def isLittle = treblePositions.toSet != (1 to nbells).toSet

	private lazy val treblePositionCounts = treblePositions.groupBy
			{ (n) => n}.mapValues
			{
				_.size
			}.values.toSet

	def isPlain = treblePositionCounts == Set(2)

	def isTD = treblePositionCounts == Set(4)

	def isRightPlace = wrongPlaceFactor==0

	def wrongPlaceAbove: Double = wrongPlaceFactor(workAbove)
	def wrongPlaceBelow: Double = wrongPlaceFactor(workBelow)

	def isDouble = isSymmetric && isSymmetricFrontToBack

	def isSymmetricFrontToBack =
	{
		val halfLead = lead.length/2
		val quarterLead = halfLead/2
		val frontHalfLead = lead.slice(quarterLead-1, lead.length-quarterLead)
		val backHalfLead = lead.slice(lead.length-quarterLead-1, lead.length) ++ lead.slice(0, quarterLead)
		frontHalfLead==PN.reverse(backHalfLead, nbells)
	}


	private lazy val baseClassification = if (isPlain) "" else if (isTD) tdType else if (isSymmetric) "A" else "H"
	lazy val shortClassification = classifyLittle(baseClassification)
	lazy val longClassification = classifyLittle(MethodAnalysis.longClassifications(baseClassification))

	private def classifyLittle(s: String) = if (isLittle) "Little " + s else s

	private def tdType = lead.slice(0, leadLength/2-1).count(_ == PN("1" + Rounds.charAt(nbells - 1))) match
	{
		case 0 => "S"
		case n if (n == (nbells / 2) - 1) => "TD"
		case _ => "D"
	}

	def isPlainBobType = firstLeadHead.isPlainBob

	def coursingFourScore(): Int = firstLead.map{_.coursingScore(4)}.sum

	private final val LH_GROUPS_2NDS = "abcdef"
	private final val LH_GROUPS_NTHS = "ghjklm"
	private final val LH_GROUP_NON_PB = "z"

	def is2ndsPlace = lead.last==PN("12")
	def isNthsPlace = lead.last==PN("1"+Rounds(nbells-1))

	/** Only deals with even-stage single-hunt methods at the moment */
	lazy val lhGroup: String =
	{
		if (!isPlainBobType)
			LH_GROUP_NON_PB
		else if (is2ndsPlace)
			lhGroup(firstLeadHead.bellAt(2), LH_GROUPS_2NDS)
		else if (isNthsPlace)
			lhGroup(firstLeadHead.bellAt(2), LH_GROUPS_NTHS)
		else
			LH_GROUP_NON_PB
	}

	private def lhGroup(bellIn2nds: Int, groups: String): String =
	{
		if (bellIn2nds==2)
			"-"
		else
		{
			val n = (bellIn2nds-3)/2
			val capped = n.min(2)
			val letter = if (bellIn2nds%2==0) groups(groups.length-1-capped) else groups(capped)
			val suffix = if (n>2) ""+(n-2) else ""
			letter+suffix
		}
	}

	def differentWorks(other: MethodAnalysis) = workAbove!=other.workAbove && workBelow!=other.workBelow

	private def leadZippedTreblePos = lead.zip(treblePositions).take(lead.length/2-1)

	/** Doesn't include half-lead or leadhead  */
	def workAbove =
	{
		def placesAbove(pair: (PN, Int)) = pair._1.placesAbove(pair._2)
		leadZippedTreblePos.map { placesAbove(_) }
	}

	/** Doesn't include half-lead or leadhead  */
	def workBelow =
	{
		def placesBelow(pair: (PN, Int)) = pair._1.placesBelow(pair._2)
		leadZippedTreblePos.map { placesBelow(_) }
	}

	def pathDirections = path(2).sliding(2).toList.map{(p)=> p.last-p.head}

	private def removeKentPlaces(dirs: List[Int]): List[Int] =
	{
		dirs match
		{
			case 0 :: a :: 0 :: rest =>	a::removeKentPlaces(rest)
			case a :: rest => a::removeKentPlaces(rest)
			case Nil => Nil
		}
	}

	private def removeDodges(dirs: List[Int]): List[Int] =
	{
		dirs match
		{
			case 1 :: -1 :: 1 :: rest =>	removeDodges(1::rest)
			case -1 :: 1 :: -1 :: rest =>	removeDodges(-1::rest)
			case a :: rest => a::removeDodges(rest)
			case Nil => Nil
		}
	}

	private def countHuntChanges(dirs: List[Int]): Int =
	{
		dirs match
		{
			case -1 :: 1 :: rest => 1+countHuntChanges(1::rest)
			case 1 :: -1 :: rest => 1+countHuntChanges(-1::rest)
			case 1 :: 0 :: 1 :: rest => 1+countHuntChanges(1::rest)
			case -1 :: 0 :: -1 :: rest => 1+countHuntChanges(-1::rest)
			case a :: rest => 0+countHuntChanges(rest)
			case Nil => 0
		}
	}

	def oddPointCount = countOddPoints(path(2))

	private def countOddPoints(path: List[Int]): Int =
	{
		path match
		{
			case a :: b :: c :: rest if (a<b && c<b && b%2==1)=> 1+countOddPoints(b::c::rest)
			case a :: b :: c :: rest if (a>b && c>b && b%2==0)=> 1+countOddPoints(b::c::rest)
			case a :: rest => 0+countOddPoints(rest)
			case Nil => 0
		}
	}

	def zipSize =
	{
		val directions = pathDirections.map{_.toByte}
		val buf = new ByteArrayOutputStream()
		val zip = new ZipOutputStream(buf)
		zip.putNextEntry(new ZipEntry("method"))
		zip.write(directions.toArray)
		zip.closeEntry()
		zip.close()
		buf.size()
	}

	//lazy val difficulty = zipSize
	//lazy val difficulty = zipSize-174+countHuntChanges(removeDodges(removeKentPlaces(pathDirections)))
	lazy val difficulty =
	{
		zipSize-174 +
		countHuntChanges(removeDodges(removeKentPlaces(pathDirections))) +
		2*oddPointCount
	}

	// TODO could do with caching these somewhere globally
	lazy val goodPN = GoodPn(nbells).oneConsec.toSet
	lazy val decentPN = GoodPn(nbells).noUltimatePlace.toSet
	//val decentPN = GoodPn(nbells).allPN.toSet

	def isGoodPN = isNoRepeatedPlaces() && lead.slice(0,leadLength/2-1).forall(goodPN.contains(_))
	def isDecentPN = isNoRepeatedPlaces() && lead.slice(0,leadLength/2-1).forall(decentPN.contains(_))

	def isNoRepeatedPlaces(): Boolean = isNoRepeatedPlaces(lead)

	private def isNoRepeatedPlaces(pn: Seq[PN]): Boolean = pn match
	{
		case a::b::tail => a.acceptableConsecutive(b) && isNoRepeatedPlaces(b::tail)
		case _ => true
	}

	/** Uses the first lead to count potential 4-runs front or back, including only 2345/5432 type positions. */
	lazy val count4Runs: Int = firstLead.map(_.countRunPositions(4)).sum

	/** Uses the first lead to count potential 4-runs front or back, including 4678 and 5378 type positions. Works best on 8. */
	lazy val potential4Runs: Int = firstLead.map(_.countPotentialRuns(4, false)).sum

	/** Uses the first lead to count potential 4-runs front or back, including 4678 and 5378 type positions and swaps like 6478 or 6578.
		* Works best on 8. */
	lazy val potential4RunsAndNearMisses: Int = firstLead.map(_.countPotentialRuns(4, true)).sum

	/** Uses the first lead to count potential 5-runs front or back, including only 23456/65432 type positions. */
	lazy val count5Runs: Int = firstLead.map(_.countRunPositions(5)).sum

	/** Uses the first lead to count potential 6-runs front or back, including only 234567/765432 type positions. */
	lazy val count6Runs: Int = firstLead.map(_.countRunPositions(6)).sum

	/** Returns the list of bells between the given (inclusive) positions, for all rows in the first lead */
	def bellsInPosition(lowest: Int, highest: Int): List[List[Int]] =
	{
		firstLead.map { _.asBellNumbers.slice(lowest-1, highest)}
	}

	def longestRunInSameDodgingPosition =
	{
		val all = (1 to nbells by 2).toList.map{(p)=>bellsInPosition(p, p+1)}
		val longestRuns = all.map{longestRunOfSameBells(_)}
		longestRuns.max
	}

	def longestRunOfSameBells(bellsPerRow: List[List[Int]]) = longestRun(bellsPerRow.map{_.toSet})

	/** Helper method, finds the length of the longest run of identical consecutive items in the given sequence */
	def longestRun[T](xs: Seq[T]): Int =
	{
		def pairEqual(pair: Seq[T]) = pair.head==pair.tail.head
		def runLengths(runs: List[Boolean], revResults:List[Int]): List[Int] =
		{
			if (runs.isEmpty)
				revResults.reverse
			else
			{
				val (run, remaining) = runs.span{(b)=>b}
				if (run.isEmpty)
					runLengths(remaining.tail, revResults)
				else
					runLengths(remaining, run.size::revResults)
			}
		}
		val runs = xs.sliding(2).toList.map{pairEqual(_)}
		1 + runLengths(runs, Nil).max
	}

	/** Given a list of course heads, returns pairs (music, coursehead) in sorted order, with highest music first */
	def findBestCourses(courseHeads: List[Row], music: Music) =
	{
		def courseMusic(ch: Row) = generateFullCourse(ch).map{music.countMusic(_)}.sum
		courseHeads.map{(ch)=> (courseMusic(ch), ch) }.sortBy{-_._1}
	}

	/** Get the bells passed (by the tenor) during a plain course of the method. */
	def getBellsPassed =
	{
		val bellToFollow = nbells
		var myPlace = nbells
		var beforeMe = nbells-1
		var afterMe = 0
		var revBellsPassed = List(0)	// Start with a zero to avoid checks for empty list
		for (row <- fullCourse.tail)
		{
			val place = row.placeOf(bellToFollow)
			// Only add the passed bell if it is different from the previous, i.e. we are not just dodging with it.
			if (place>myPlace && afterMe!=revBellsPassed.head)
				revBellsPassed = afterMe::revBellsPassed
			else if (place<myPlace && beforeMe!=revBellsPassed.head)
				revBellsPassed = beforeMe::revBellsPassed
			myPlace = place
			beforeMe = if (place==1) 0 else row.bellAt(place-1)
			afterMe = if (place==nbells) 0 else row.bellAt(place+1)
		}
		// Must now reverse the list, strip off the initial 0, and check first and last are not the same bell
		// (this could occur if the course end is a dodge).
		val lastBellPassed = revBellsPassed.head
		val bellsPassed = revBellsPassed.reverse.tail
		if (bellsPassed.head==lastBellPassed)
			revBellsPassed.tail.reverse.tail
		else
			bellsPassed
	}

	def getBestCourses(candidateCourseHeads: List[Row], music: Music): List[(Row,Int)] =
	{
		def musicForCourse(ch: Row) = (ch, music.countMusic(generateFullCourse(ch)))
		candidateCourseHeads.map(musicForCourse).sortBy(-_._2)
	}
}

object MethodAnalysis
{
	val longClassifications = Map("S" -> "Surprise", "D" -> "Delight", "TB" -> "TB", "A" -> "Alliance", "H" -> "Hybrid", ""->"")

}

