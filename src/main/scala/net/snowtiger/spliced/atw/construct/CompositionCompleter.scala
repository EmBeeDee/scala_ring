package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw._
import net.snowtiger.spliced.atw.construct.SectionTables.PnTree

/**
 * @author mark
 */

class CompositionCompleter(possibleHalfLeads: List[Map[Row,PN]]) extends TreblePath
{
	val base = possibleHalfLeads.zipWithIndex.map{ (p) => MethodPos(p._2, p._1)}

	var rowsLeft = 0

	def complete(partialComp: CourseAtwComposition): Int =
	{
		val startingCourses = partialComp.courses.zip(base).map{ (p) => new MethodBeingCompleted(p._2, p._1)}
		val startingComp = CompositionBeingCompleted(startingCourses, partialComp.rows)
		rowsLeft = startingComp.rowsLeft
		complete2(startingComp, 0)
		rowsLeft
	}

	private def complete2(comp: CompositionBeingCompleted, rowsIncreased: Int): Int =
	{
		if (rowsLeft>comp.rowsLeft)
		{
			rowsLeft = comp.rowsLeft
			println("Lowater: "+rowsLeft)
		}
		//rowsLeft = rowsLeft.min(comp.rowsLeft)
		val incomplete = comp.comp.filter{!_.isComplete}
		if (incomplete.isEmpty)
		{
			println("SOLUTION: " + comp)
			assert(comp.allRows.size==CompLength)
			rowsIncreased
		}
		else
		{
			// TODO Should maybe do in loop, stop if we get to 0 or 1
			val (leastChoiceMethod, pnChoices) = incomplete.map{ (m) => (m, m.nextPnChoices(comp.allRows))}.minBy{ (p) => p._2.size * 100 + p._1.rowReached}
			if (pnChoices.size == 0)
				rowsIncreased
			else
			{
				var maxIncrease = 1+rowsIncreased
				for (pn <- pnChoices)
				{
					val updatedMethod = leastChoiceMethod.applyPn(pn, comp.allRows)
					if (updatedMethod.isDefined)
					{
						val newMethod = updatedMethod.get
						val newComp = comp.add(newMethod)
						val increase = newMethod.rowReached-leastChoiceMethod.rowReached
						val newIncrease = complete2(newComp, rowsIncreased+increase)
						maxIncrease = maxIncrease.max(newIncrease)
					}
				}
				maxIncrease
			}
		}
	}

	val BranchMax = 2

	def findBranchiness(partialComp: CourseAtwComposition): Int =
	{
		val startingCourses = partialComp.courses.zip(base).map{ (p) => new MethodBeingCompleted(p._2, p._1)}
		val startingComp = CompositionBeingCompleted(startingCourses, partialComp.rows)
		branchiness(startingComp, 1)
	}

	private def branchiness(comp: CompositionBeingCompleted, nBranches: Int): Int =
	{
		val incomplete = comp.comp.filter{!_.isComplete}
		if (incomplete.isEmpty)
		{
			println("SOLUTION: " + comp)
			assert(comp.allRows.size==CompLength)
			BranchMax
		}
		else
		{
			// TODO Should maybe do in loop, stop if we get to 0 or 1
			val (leastChoiceMethod, pnChoices) = incomplete.map{ (m) => (m, m.nextPnChoices(comp.allRows))}.minBy{ (p) => p._2.size * 100 + p._1.rowReached}
			if (pnChoices.size == 0)
				0
			else
			{
				var newBranches = nBranches+pnChoices.size-1
				var maxNextBranches = newBranches
				val pnIterator = pnChoices.iterator
				while (pnIterator.hasNext && maxNextBranches<BranchMax)
				{
					val updatedMethod = leastChoiceMethod.applyPn(pnIterator.next, comp.allRows)
					if (updatedMethod.isDefined)
					{
						val newMethod = updatedMethod.get
						val newComp = comp.add(newMethod)
						val increase = newMethod.rowReached-leastChoiceMethod.rowReached
						maxNextBranches = branchiness(newComp, newBranches).max(maxNextBranches)
					}
				}
				// If we return branch values bigger than the max, skews scoring compared to rows remaining.
				maxNextBranches.min(BranchMax)
			}
		}
	}
}

case class CompositionBeingCompleted(comp: List[MethodBeingCompleted], allRows: MultiBitSet) extends TreblePath
{
	def incompleteAt(level: Int) = comp.filter{_.rowReached<level}
	def add(updatedMethod: MethodBeingCompleted) =
	{
		val i = updatedMethod.col.indexInComp
		val (first, last) = comp.splitAt(i)
		CompositionBeingCompleted(first++(updatedMethod::last.tail), allRows+updatedMethod.allRows)
	}

	def rowsLeft = comp.map{(m)=> (HalfLeadLength-m.rowReached)*7*2}.sum

	override def toString = comp.mkString(", ")
}

case class MethodPos(indexInComp: Int, possibleHalfLeads: Map[Row,PN])

case class MethodBeingCompleted(col: MethodPos, allRows: MultiBitSet, rowReached: Int, pnTree: PnTree, endRows: List[Row], revPN: List[PN], name: Option[String]) extends TreblePath
{
	def this(col: MethodPos, node: PartialMethodNodeBitSet) =
		this(col, node.bitset, node.rowReached, SectionTables.tdPNs(node.rowReached - 1).acceptableConsecutive(node.lastPN.head), node.sectionEnds, node.lastPN, node.nameIfComplete)

	def isComplete = rowReached == HalfLeadLength

	def nextPnChoices(restOfComp: MultiBitSet): List[PN] =
	{
		var possiblePNs = List[PN]()
		for (pn <- pnTree.pns.keys)
		{
			val newEndRows = endRows.map
			{
				_.apply(pn)
			}
			if (rowReached < HalfLeadLength - 1 || validHL(newEndRows.head, pn))
			{
				val newSet = RowBitSet(newEndRows)
				if (restOfComp.trueWith(newSet))
					possiblePNs = pn :: possiblePNs
			}
		}
		possiblePNs
	}

	def validHL(endRow: Row, prevPN: PN) = col.possibleHalfLeads.get(endRow) match
	{
		case Some(pn) => pn.acceptableConsecutive(prevPN)
		case None => false
	}

	/** If this PN works, keeps going with further changes until there is a choice of more than one PN. None if dead end. */
	def applyPn(pn: PN, restOfComp: MultiBitSet): Option[MethodBeingCompleted] =
	{
		val newEndRows = endRows.map
		{
			_.apply(pn)
		}
		val newSet = RowBitSet(newEndRows)
		val newAllRows = allRows + newSet
		val nextMethod = MethodBeingCompleted(col, newAllRows, rowReached + 1, pnTree.pns(pn), newEndRows, pn :: revPN, name)
		if (nextMethod.isComplete)
			Some(nextMethod)
		else
		{
			val nextChoices = nextMethod.nextPnChoices(restOfComp)
			nextChoices.size match
			{
				case 0 => None
				case 1 => nextMethod.applyPn(nextChoices.head, restOfComp + newSet)
				case _ => Some(nextMethod)
			}
		}
	}

	def displayPN =
	{
		if (isComplete)
			(col.possibleHalfLeads.get(endRows.head).get :: revPN).reverse
		else
			revPN.reverse
	}

	override def toString =
		col.indexInComp + ": " + (name match
		{
			case Some(method) => method
			case None => PN.output( displayPN )
		})
}