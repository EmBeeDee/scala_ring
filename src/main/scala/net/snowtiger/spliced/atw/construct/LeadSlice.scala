package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.PN
import net.snowtiger.spliced.atw.RowArray

import scala.collection.mutable
import scala.util.Random

/**
 * A "slice" is a section through the lead of a method course containing all the rows with the treble in one position.
 * At each level, a slice holds the options for the next available slice; for example, the slice for the treble at
 * lead will contain a Map from the set of such rows to all the options with the treble in second's. The final slice
 * simply gives the set of place notations which can produce the rows visited so far.
 */
trait LeadSlice
{
	/** Total number of row-slices (i.e. down to, but not including the PN sets). */
	def size: Int
	def isEmpty: Boolean
	/** The method music count, cumulatively for all slices above ourselves (but does not include rowmaps we hold). */
	def methodMusic: Int
	/** The composition music count, for all rotations, but just our slice (not cumulative). */
	def compSliceMusic: CompRotMusic

	/** Returns true for the SliceFinish */
	def isFinishLevel:Boolean
	/** Only defined for the SliceFinish - the set of method place notations which produce this finish */
	def getFinishMethods: Set[List[PN]]

	/** For falseness pruning and backtracking. */
	var isAvailable = true
	/** These wrong-place above/below work counts are set in a separate pass, after pruning and forcing are complete. */
	var nWrongBelow = 0
	var nWrongAbove = 0
	def countWrongWork(): Unit
	def wrongBelowAvailable: Boolean
	def wrongAboveAvailable: Boolean

	def nChildrenAvailable: Int
	def pnStrings: Set[String]

	/** Adds an additional set of rows (for this slice and all below) and a final SliceFinish */
	def add(newRows: List[RowArray], music: MusicTracker, finish: SliceFinish): Unit

	/** For the simulated annealing search */
	def randomSlice: (RowArray,LeadSlice)
	
	def visitFinishes(revSlices: List[(RowArray,LeadSlice)], fn: (List[(RowArray,LeadSlice)], SliceFinish)=>Unit): Unit

	/** Descends all branches to the specified level, then marks rows false against otherRow.
		* Must populate the "marked" record with any and all slices which have been marked false (and weren't false before!).
		* Returns true if there are some true options left, else false. */
	def markFalse(atLevel: Int, otherRow: RowArray, marked: FalsenessRecord): Boolean

	/** Descends all branches, marking false if no methods with wrong-place below work */
	def markRightPlaceBelowFalse(marked: FalsenessRecord): Boolean

	/** Descends all branches to the quarter-lead, marking false if no methods with wrong-place above work */
	def markRightPlaceAboveFalse(marked: FalsenessRecord): Boolean

	/** Descends to the branch which matches the given sample PN string, marking false if it exists */
	def markWorkFalse(pnToMatch: String, level: Int, marked: FalsenessRecord): Boolean

	def anyTrue(trueWith: List[RowArray]): Boolean

	/** Remove any sub-slices that are false against all options in at least one other method course. */
	def pruneAgainst(others: Vector[Stalactite], revRowsSoFar: List[RowArray], untilLevel: Int): Int

	/** Prune all branches other than those that match the given methods */
	def forceMethods(rowLists: List[List[RowArray]]): Unit

	/** Prune the branch matching the given method. Returns true if this slice does still have options remaining. */
	def avoidMethod(rows: List[RowArray]): Boolean

	/** Extract the branches that match the given method */
	def descendMethod(rows: List[RowArray]): List[LeadSlice]

	def describeChild(rows: RowArray): String
}

object LeadSlice
{
	val WrongPnsRequiredBelow = Set(PN("14"), PN("16"), PN("36"))
	val WrongPnsRequiredAbove = Set(PN("38"), PN("36"), PN("58"), PN("1258"))
	val TreblePosToPnSize = Map(1->2, 2->3, 3->6, 4->7, 5->10, 6->11, 7->14, 8->15)
}

/** At the bottom of the slice tree is a set of different method PNs, all of which give the rows we have visited in the tree above.
	* Note the finishing (halflead) rows are no longer needed, nor held. There might have been different finishing rows because
	* e.g. 34-34 and -34- visit the same sets of rows, but end up in different rows.
	* There may be more than one pn string for a given finishing row, because e.g. 1238-12- and 38.12-12 both end up in the same row.
	* We also hold the music score - this is the same, no matter what PNs we use and which row we end up in, because
	* we have in total visited the same rows to get here.
	*/
class SliceFinish(val methods: mutable.Set[List[PN]], val methodMusic: Int, val compSliceMusic: CompRotMusic, val totalCompMusic: CompRotMusic) extends LeadSlice
{
	override def size = 1
	override def isEmpty = methods.isEmpty
	override def nChildrenAvailable = 0

	override def isFinishLevel = true
	override def getFinishMethods = methods.toSet

	override def pnStrings = derivePnStrings(8)

	def derivePnStrings(treblePos: Int): Set[String] =
	{
		if (treblePos==0)
			Set()
		else
		{
			val n = LeadSlice.TreblePosToPnSize(treblePos)
			// Don't bother storing pnString below the quarter-lead
			if (n>10)
				Set("")
			else
			{
				// On or above the quarter-lead, only store the places above the treble
				var reducedPns = getFinishMethods.map{_.zip(SectionTables.TreblePath).take(n).map{(p)=> p._1.placesAbove(p._2)}}
				reducedPns.map(PN.output)
			}
		}
	}

	override def add(newRows: List[RowArray], music: MusicTracker, finish: SliceFinish) =
	{
		assert(methodMusic==finish.methodMusic)
		methods++= finish.methods
	}

	def visitAvailableChildren(fn: (RowArray, LeadSlice)=>Unit): Unit = ???

	override def randomSlice = ???

	def visitFinishes(revSlices: List[(RowArray,LeadSlice)], fn: (List[(RowArray,LeadSlice)], SliceFinish) => Unit) = fn(revSlices, this)

	override def markFalse(atLevel: Int, otherRow: RowArray, marked: FalsenessRecord) = ???

	override def markRightPlaceBelowFalse(marked: FalsenessRecord) =
	{
		if (nWrongBelow==0)
		{
			marked.markFalse(this)
			false
		}
		else
			true
	}

	override def markRightPlaceAboveFalse(marked: FalsenessRecord) = ???
	override def markWorkFalse(pnToMatch: String, level: Int, marked: FalsenessRecord) = ???

	override def anyTrue(trueWith: List[RowArray]) = true

	override def countWrongWork() =
	{
		for (pns <- methods)
		{
			val aboveToCheck = List(pns(0),pns(2),pns(4),pns(6))
			if (aboveToCheck.exists(LeadSlice.WrongPnsRequiredAbove.contains))
				nWrongAbove+= 1
			val revPns = pns.reverse.take(8).toArray
			val belowToCheck = List(revPns(1),revPns(3),revPns(5),revPns(7))
			if (belowToCheck.exists(LeadSlice.WrongPnsRequiredBelow.contains))
				nWrongBelow+= 1
		}
	}

	override def wrongBelowAvailable = nWrongBelow>0
	override def wrongAboveAvailable = nWrongAbove>0

	/** Must never prune down to the PNs */
	override def pruneAgainst(others: Vector[Stalactite], revRowsSoFar: List[RowArray], untilLevel: Int) = ???

	override def forceMethods(rowLists: List[List[RowArray]]) = {}	// No-op
	override def avoidMethod(rows: List[RowArray]) = false

	override def descendMethod(rows: List[RowArray]) = List(this)

	override def toString = getFinishMethods.map{(pn)=> PN.output(pn)}.mkString(",")+" ("+methodMusic+")"
	def describeChild(rows: RowArray) = ???
}

/** MidSlices are a map of possibilities for this treble position (as a RowArray) -> slices at the next level down. */
class MidSlice(val rowMap: mutable.Map[RowArray, LeadSlice], var pnStrings: Set[String], val methodMusic: Int, val compSliceMusic: CompRotMusic)
		extends LeadSlice
{
	override def size = rowMap.values.map{_.size}.sum
	override def isEmpty = rowMap.isEmpty
	override def nChildrenAvailable = rowMap.values.count{_.isAvailable}

	override def isFinishLevel = false
	override def getFinishMethods = ???

	/** For the simulated annealing search */
	override def randomSlice =
	{
		val r = Random.nextInt(rowMap.size)
		val it = rowMap.iterator
		var i = 0
		while (i<r)
		{
			i+= 1
			it.next()
		}
		it.next()
	}

	override def visitFinishes(revSlices: List[(RowArray,LeadSlice)], fn: (List[(RowArray,LeadSlice)], SliceFinish) => Unit) =
	{
		for (p <- rowMap)
			p._2.visitFinishes(p::revSlices, fn)
	}

	override def add(newRows: List[RowArray], music: MusicTracker, finish: SliceFinish) =
	{
		val rows = newRows.head
		rowMap.get(rows) match
		{
			case Some(slice) => slice.add(newRows.tail, music, finish)
			case None => rowMap+= rows-> makeLowerLevel(newRows.tail, music, finish)
		}
		pnStrings = pnStrings++finish.derivePnStrings(newRows.head.treblePos-1)
	}

	/** Factory method. If there are rows left, make a (populated) MidSlice, else just return the SliceFinish. */
	protected def makeLowerLevel(newRows: List[RowArray], music: MusicTracker, finish: SliceFinish): LeadSlice = newRows match
	{
		case Nil => finish
		case rows::tail =>
		{
			val newChildMap = mutable.Map(rows -> makeLowerLevel(tail, music, finish))
			val treblePos = rows.treblePos-1
			val newPnStrings = finish.derivePnStrings(treblePos)
			new MidSlice(newChildMap, newPnStrings, music.getMethodMusic(treblePos), music.getCompMusic(treblePos))
		}
	}

	override def markFalse(atLevel: Int, otherRow: RowArray, marked: FalsenessRecord): Boolean =
	{
		assert(isAvailable)
		var someTrue = false
		if (atLevel==0)
		{
			for ((row, nextLevel) <- rowMap)
				if (nextLevel.isAvailable)
				{
					if (row.trueWith(otherRow))
						someTrue = true
					else
						marked.markFalse(nextLevel)
				}
		}
		else
		{
			for ((row,nextLevel) <- rowMap)
				if (nextLevel.isAvailable)
					someTrue|= nextLevel.markFalse(atLevel-1, otherRow, marked)

		}
		// If no children true, mark us as false
		if (!someTrue)
			marked.markFalse(this)
		someTrue
	}

	override def markRightPlaceBelowFalse(marked: FalsenessRecord) =
	{
		assert(isAvailable)
		var someTrue = false
		for ((row,nextLevel) <- rowMap)
			if (nextLevel.isAvailable)
				someTrue|= nextLevel.markRightPlaceBelowFalse(marked)
		// If no children true, mark us as false
		if (!someTrue)
			marked.markFalse(this)
		someTrue
	}

	override def markRightPlaceAboveFalse(marked: FalsenessRecord) =
	{
		assert(isAvailable)
		var someTrue = false
		if (rowMap.head._1.treblePos==5)
		{
			if (nWrongAbove>0)
				someTrue = true
		}
		else
		{
			for ((row, nextLevel) <- rowMap)
				if (nextLevel.isAvailable)
					someTrue |= nextLevel.markRightPlaceAboveFalse(marked)
		}
		// If no children true, mark us as false
		if (!someTrue)
			marked.markFalse(this)
		someTrue
	}

	override def markWorkFalse(pnToMatch: String, level: Int, marked: FalsenessRecord) =
	{
		var someTrue = false
		if (level==0)
		{
			for (slice <- rowMap.values; if slice.isAvailable)
				if (slice.pnStrings(pnToMatch))
					marked.markFalse(slice)
				else
					someTrue = true
		}
		else
		{
			for (slice <- rowMap.values; if slice.isAvailable)
				if (slice.pnStrings.exists(pnToMatch.startsWith))
					someTrue|= slice.markWorkFalse(pnToMatch, level-1, marked)
				else
					someTrue = true
		}
		if (!someTrue)
			marked.markFalse(this)
		someTrue
	}

	override def anyTrue(trueWith: List[RowArray]) =
	{
		val otherRow = trueWith.head
		val remainingRows = trueWith.tail
		rowMap.keys.exists{(r) => r.trueWith(otherRow) && (remainingRows.isEmpty || rowMap(r).anyTrue(remainingRows))}
	}

	override def countWrongWork() =
	{
		for ((row,nextLevel) <- rowMap)
		{
			nextLevel.countWrongWork()
			nWrongAbove+= nextLevel.nWrongAbove
			nWrongBelow+= nextLevel.nWrongBelow
		}
	}

	/** Checks at least one of our children is still available and can be wrong-below; doesn't look lower than that. */
	override def wrongBelowAvailable =
		nWrongBelow>0 && rowMap.values.exists{(child)=> child.isAvailable && child.nWrongBelow>0}

	/** Checks at least one of our children is still available and can be wrong-above; doesn't look lower than that. */
	override def wrongAboveAvailable =
		nWrongAbove>0 && rowMap.values.exists{(child)=> child.isAvailable && child.nWrongAbove>0}

	/** Recursively descends the tree to prune at a specified level. Returns the number of items pruned. */
	def pruneAgainst(others: Vector[Stalactite], revRowsSoFar: List[RowArray], untilLevel: Int) =
	{
		var totalPruned = 0
		val newUntil = untilLevel-1
		if (newUntil==0)
		{
			// Prune at this level
			for ((row,nextLevel) <- rowMap.toList)
			{
				val check = row::revRowsSoFar
				if (others.exists{!_.anyTrue(check.reverse)})
				{
					rowMap-= row
					totalPruned+= nextLevel.size
				}
			}
		}
		else
		{
			// Descend to prune at a lower level
			for ((row,nextLevel) <- rowMap.toList)
			{
				val check = row::revRowsSoFar
				val nPruned = nextLevel.pruneAgainst(others, check, newUntil)
				totalPruned+= nPruned
				if (nextLevel.isEmpty)
					rowMap-= row
			}
		}
		totalPruned
	}

	def forceMethods(rowLists: List[List[RowArray]]) =
	{
		val nonEmptyLists = rowLists.filter{(xs)=> xs.nonEmpty && xs.head.size>0}
		val headRows = nonEmptyLists.map{_.head}.toSet
		var oldRowMap = rowMap.toMap
		rowMap.clear()
		for (headRow <- headRows)
		{
			val childRowLists = nonEmptyLists.filter{_.head==headRow}.map{_.tail}
			oldRowMap.get(headRow) match
			{
				case Some(child) =>
				{
					child.forceMethods(childRowLists)
					rowMap+= headRow -> child
				}
				case None =>
				{
					if (headRow.size>=oldRowMap.head._1.size)
						throw new Exception("Method not found at slice "+(8-nonEmptyLists.head.size))
					else
					{
						val retained = oldRowMap.filterKeys{_.containsAll(headRow)}.toMap
						if (retained.isEmpty)
							throw new Exception("Method not found at slice "+(8-nonEmptyLists.head.size))
						rowMap++= retained
					}
				}
			}
		}
	}

	def avoidMethod(rows: List[RowArray]): Boolean =
	{
		val headRows = rows.head
		if (headRows.size>0)
		{
			rowMap.get(headRows) match
			{
				case Some(child) =>
				{
					if (!child.avoidMethod(rows.tail))
						rowMap-= headRows
				}
				case None =>
				{
					if (headRows.size>=rowMap.head._1.size)
						throw new Exception("Method not found at slice "+(8-rows.size))
					else
					{
						rowMap.retain{(r,s)=> !r.containsAll(headRows)}
					}
				}
			}
		}
		!rowMap.isEmpty
	}

	def descendMethod(rows: List[RowArray]): List[LeadSlice] =
	{
		if (rows.isEmpty || rows.head.size==0)
			Nil
		else rowMap.get(rows.head) match
		{
			case None => throw new Exception("Method not found at slice "+(8-rows.size))
			case Some(slice) => slice::(slice.descendMethod(rows.tail))
		}
	}

	override def toString = "/"
	def describeChild(rows: RowArray) = ""+rows.treblePos+rowMap(rows)
}

