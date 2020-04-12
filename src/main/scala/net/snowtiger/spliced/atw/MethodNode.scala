package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.score.StandardMaxMusic
import net.snowtiger.spliced.tables.Numberer

import scala.collection.mutable.ListBuffer
import scala.collection.{BitSet, mutable}

class MethodNode(val course: MethodCourse, val method: SearchMethod)
{
	val rows = genRows.toSet
	lazy val isTrue = rows.size==course.cos.size*method.leadLength && trueWithRowsAlreadyRung

	protected def trueWithRowsAlreadyRung = true

	/** Calculates the set of all rows for each lead of the method from the given leadheads */
	protected def genRows =
	{
		val buf = ListBuffer[Row]()
		for (lh <- course.leadheads)
			method.generateLead(lh, buf)
		buf.toList
	}

	override def toString = method.name+" ("+method.lhGroup+")"

}

/**
 * A given method choice, including the rows for all (N-1) leads of the method, across the "method" (horizontal) course.
 * Also contains truth tables with other method choices for different courses.
 *
 * @author mark
 */
abstract class TruthTableMethodNode(course: MethodCourse, method: SearchMethod) extends MethodNode(course, method)
{
	val music = new StandardMaxMusic()
	lazy val score = calcScore(rows)

	def trueTable: mutable.Map[MethodCourse, Set[TruthTableMethodNode]]
	def revTruth: mutable.Set[TruthTableMethodNode]
	def buildTable(otherCourse: MethodCourse, candidateMethods: Set[TruthTableMethodNode]): Unit
	def isPrunable: Boolean
	def removePruned(other: TruthTableMethodNode): Unit

	protected def isTrueWith(other: TruthTableMethodNode) = method.isAllowedWith(other.method) && TruthTableMethodNode.quickTruthCheck(rows, other.rows)

	override protected def trueWithRowsAlreadyRung =
		AtwMethodFinder.rowsAlreadyRung.isEmpty || TruthTableMethodNode.quickTruthCheck(rows, AtwMethodFinder.rowsAlreadyRung)

	protected def init =
	{
		val rowsList = genRows
		(rowsList.toSet, calcScore(rowsList))
	}

	def calcScore(rowsList: Iterable[Row]) = music.countMusic(rowsList)

}

class MethodNodeWithTruth(course: MethodCourse, method: SearchMethod) extends TruthTableMethodNode(course, method)
{
	/** Intended to hold "forward" pointers only - methods in future courses true against this one */
	val trueTable = mutable.Map[MethodCourse, Set[TruthTableMethodNode]]()
	/** Intended to hold "reverse" pointers - methods in previous courses which are true with this one. Used in pruning. */
	val revTruth = mutable.Set[TruthTableMethodNode]()

	def isPrunable = trueTable.exists{(p)=> p._2.isEmpty}

	def removePruned(allMethods: Map[MethodCourse, List[TruthTableMethodNode]])
	{
		for (course <- trueTable.keys)
			trueTable(course) = trueTable(course).filter{(m)=> allMethods(course).contains(m)}
	}

	def removePruned(other: TruthTableMethodNode)
	{
		val course = other.course
		trueTable(course) = trueTable(course).filter{_!=other}
	}

	def buildTable(otherCourse: MethodCourse, candidateMethods: Set[TruthTableMethodNode])
	{
		val trueMethods = (candidateMethods.filter{(m)=> isTrueWith(m)})
		trueTable+= otherCourse->trueMethods
		trueMethods.foreach{_.revTruth+=this}
		// Free up rows space
		//rows = null
	}
}

class NumberedMethodNode(course: MethodCourse, method: SearchMethod) extends TruthTableMethodNode(course, method) with NumberHashable
{
	course.numberedNodeTable.intern(this)

	lazy val nextLevelNodes = method.nextLevelMethods.map{(m)=> new ChildMethodNode(course, m, this)}.filter{_.isTrue}

	def trueTable = throw new UnsupportedOperationException("bitset truth")
	val revTruth = mutable.Set[TruthTableMethodNode]()

	/** Intended to hold "forward" pointers only - methods in future courses true against this one */
	val bitTruth = mutable.Map[MethodCourse, BitSet]()

	def buildTable(otherCourse: MethodCourse, candidateMethods: Set[TruthTableMethodNode])
	{
		val trueMethods = (candidateMethods.filter{(m)=> isTrueWith(m)})
		bitTruth+= otherCourse-> (BitSet()++trueMethods.map{_.asInstanceOf[NumberedMethodNode].n})
		trueMethods.foreach{_.revTruth+=this}
		// Free up rows space
		//rows = null
	}

	def isPrunable = bitTruth.exists{(p)=> p._2.isEmpty}
	def removePruned(other: TruthTableMethodNode)
	{
		val course = other.course
		bitTruth(course) = bitTruth(course).filter{_!=other.asInstanceOf[NumberedMethodNode].n}
	}

	def canEqual(obj: Any) = obj match
	{
		case that: NumberedMethodNode => course==that.course
		case _ => false
	}
}

class ChildMethodNode(course: MethodCourse, method: SearchMethod, val parent: NumberedMethodNode) extends MethodNodeWithTruth(course, method)

class MethodNodeWithPrefixTruth(course: MethodCourse, method: SearchMethod) extends MethodNodeWithTruth(course, method)
{
	val N = 4

	var prefix: Set[Row] = _
	override protected def init =
	{
		var rowsList = genRows
		prefix = getPrefixRows(rowsList)
		var rows = rowsList.toSet -- prefix
		val score = calcScore(rowsList)
		rowsList = null
		(rows, score)
	}

	override protected def trueWithRowsAlreadyRung =
		AtwMethodFinder.rowsAlreadyRung.isEmpty || (TruthTableMethodNode.quickTruthCheck(prefix, AtwMethodFinder.rowsAlreadyRung) && TruthTableMethodNode.quickTruthCheck(rows, AtwMethodFinder.rowsAlreadyRung))
	override lazy val isTrue = rows.size+prefix.size==course.cos.size*method.leadLength && trueWithRowsAlreadyRung

	private def getPrefixRows(rows: List[Row]) =
	{
		var prefix = Set[Row]()
		val leads = rows.grouped(method.leadLength)
		for (lead <- leads)
		{
			prefix++= lead.slice(0, N)
			prefix++= lead.slice(lead.length-N, lead.length)
		}
		prefix
	}

}

object TruthTableMethodNode
{
	/** Quicker if smaller set first. TODO move somewhere more sensible. */
	def quickTruthCheck(rows1: Set[Row], rows2: Set[Row]) = !rows1.exists{(r)=> rows2.contains(r)}
}

class MethodNodeTable extends Numberer[NumberedMethodNode]
