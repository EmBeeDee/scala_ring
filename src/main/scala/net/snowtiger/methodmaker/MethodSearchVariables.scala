package net.snowtiger.methodmaker

import net.snowtiger.ringing.Row._
import net.snowtiger.ringing._


/**
 * Class tracks all dynamic search variables - those which update during search. Can be overridden if necessary to
 * track other data (see e.g. {@link PathBasedSearchVars}) but then we need to override {@link MethodSearch} in order
 * to specify the type parameter, and generate the correctly-typed "next" search variable instance.
 */
class MethodSearchVariables(val searchParms: MethodSearchParams, val lastRow: Row, val maskedRows: Map[String,Set[Row]],
														val nRows: Int, val score: Int)
{
	def this(searchParms: MethodSearchParams, nbells: Int) = this(searchParms, Row(nbells), Map(), 0, 0)

	def makeNext(nextRow: Row, nextFalseRows: Map[String,Set[Row]], nextScore: Int): MethodSearchVariables =
		new MethodSearchVariables(searchParms, nextRow, nextFalseRows, nRows+1, nextScore)
}

/** Keeps track of the path of each working bell, as a map bell number -> reverse list of places, as the search proceeds */
class PathBasedSearchVars(val pathSearchParms: PathSearchParams, lastRow: Row, maskedRows: Map[String,Set[Row]], nRows: Int,
													score: Int, val paths: Map[Int,List[Int]], val pathScore: Int)
		extends MethodSearchVariables(pathSearchParms, lastRow, maskedRows, nRows, score)
{
	def this(searchParms: PathSearchParams, nbells: Int) = this(searchParms, Row(nbells), Map(), 0, 0,
			(2 to nbells).map((b)=> (b,List(b))).toMap, 0)

	def makeNext(nextRow: Row, nextFalseRows: Map[String,Set[Row]], nextScore: Int, nextPaths: Map[Int,List[Int]],
							 nextPathScore: Int): PathBasedSearchVars =
		new PathBasedSearchVars(pathSearchParms, nextRow, nextFalseRows, nRows+1, nextScore, nextPaths, nextPathScore)
}


/** Parameters of the search - fixed values and overridden methods controlling in-search pruning and method acceptance. */
abstract class MethodSearchParams(val requiredScorePerRow: Double, val requiredTotalScore: Int)
{
	def scoreFn(row: Row): Int = 0

	def getFalseRows(lastRow: Row, falsePerms: Set[Perm]) = getFalseRowsNormal(lastRow, falsePerms)

	def acceptPN(pn: PN, revPrevPNs: List[PN]): Boolean =	GoodPn.isAllowableConsecutivePn(pn, revPrevPNs)

	def acceptLHPN(lhPN: PN, pns: List[PN]) = !pns.head.hasConsecutivePlacesWith(lhPN)

	def acceptMethod(pns: List[PN], score: Int): Boolean = score>= requiredTotalScore

	def acceptMethod(method: NamedMethod): Boolean = method.isPlainBobType && method.nLeads==method.nbells-1
	// Useful ideas for overridden classes:
	// method.positiveFCH.size<=1 && !method.isRightPlace && method.shortClassification=="D"

	def acceptMethod(mStats: MethodSearchStats): Boolean = true
	// Useful ideas for overridden classes:
	// mStats.contains("*H")

	protected def getFalseRowsNormal(lastRow: Row, falsePerms: Set[Perm]) = falsePerms.map(lastRow.permuteBy(_))

	/** Finds CPS methods only */
	protected def getFalseRowsCPS(lastRow: Row, falsePerms: Set[Perm]) = falsePerms.map(lastRow.permuteBy(_).maskFrontBellsNotTreble(6))

	protected def isKentPlaces(newPN: PN, revPNs: List[PN]) =
	{
		def isKent(one: PN, two: PN, three: PN) =
		{
			def _isKent(pair: List[Int]) = pair.size==2 && pair.head==pair.tail.head-1 && two.allowsSwap(pair.head)
			val intersect = one.intersection(three).toString.map{Rounds.indexOf(_)+1}.toList
			val kentPlaces = intersect.sliding(2).filter{_isKent(_)}
			!kentPlaces.isEmpty
		}
		revPNs match
		{
			case mid::old::tail => isKent(newPN, mid, old)
			case _ => false
		}
	}
}

class NoScoreSearch extends MethodSearchParams(0, 0)

class Potential4RunSearch(requiredScorePerRow: Double, requiredTotalScore: Int)
		extends MethodSearchParams(requiredScorePerRow, requiredTotalScore)
{
	override def scoreFn(row: Row) = row.countPotentialRuns(4, true)
}

class Potential4RunDifficultySearch(requiredScorePerRow: Double, requiredTotalScore: Int, requiredPathScore: Int, minDifficulty: Int)
		extends PathSearchParams(requiredScorePerRow, requiredTotalScore, requiredPathScore)
{
	val graceRows = 6
	override def calcRequiredIncrementalPathScore(nRows: Int) = (nRows-graceRows)*requiredPathScore.toDouble/(16-graceRows)

	override def scoreFn(row: Row) = row.countPotentialRuns(4, true)

	override def acceptMethod(method: NamedMethod) = super.acceptMethod(method) && method.difficulty>=minDifficulty
}

class Course4Search(requiredScorePerRow: Double, requiredTotalScore: Int)
		extends MethodSearchParams(requiredScorePerRow, requiredTotalScore)
{
	/** Double for the other halflead */
	override def scoreFn(row: Row) = row.coursingScore(4)*2

	/** Override to avoid Kent places */
	override def acceptPN(pn: PN, revPrevPNs: List[PN]) = super.acceptPN(pn, revPrevPNs)  && !isKentPlaces(pn, revPrevPNs)
}

abstract class PathSearchParams(requiredScorePerRow: Double, requiredTotalScore: Int, val requiredPathScore: Int)
		extends MethodSearchParams(requiredScorePerRow, requiredTotalScore)
{
	def calcRequiredIncrementalPathScore(nRows: Int): Double
}


class Course4DifficultySearch(requiredScorePerRow: Double, requiredTotalScore: Int, requiredPathScore: Int, minDifficulty: Int)
		extends PathSearchParams(requiredScorePerRow, requiredTotalScore, requiredPathScore)
{
	val graceRows = 12 // 6
	override def calcRequiredIncrementalPathScore(nRows: Int) = (nRows-graceRows)*requiredPathScore.toDouble/(24-graceRows)

	override def scoreFn(row: Row) = row.coursingScore(4)*2

	/** Override to avoid Kent places */
	override def acceptPN(pn: PN, revPrevPNs: List[PN]) = super.acceptPN(pn, revPrevPNs)  && !isKentPlaces(pn, revPrevPNs)

	override def acceptMethod(method: NamedMethod) = super.acceptMethod(method) && method.difficulty>=minDifficulty

}

