package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.atw.{AtwMethodBuilder, RowArray}

import scala.collection.mutable
import scala.util.Random

class CompositionState(val courses: Vector[SearchPosition])
{
	/** Total method music, then methods */
	override def toString = music+": "+courses.mkString(" ")

	def music = courses.map{_.methodMusic}.sum
}

class FinishedComposition(courses: Vector[SearchPosition]) extends CompositionState(courses)
{
	def this(state: CompositionState) = this(state.courses)

	/** Total method music then best comp music and rot, then methods */
	override def toString = methodMusic+" "+bestCompMusic+": "+courses.mkString(" ")

	lazy val methodMusic: Int = music

	lazy val bestCompMusic: (CompMusic, Row) = AtwMethodBuilder.bestRot(courses)
}

class FalsenessRecord
{
	var slicesMarked = List[LeadSlice]()
	var positionsMarked = Set[SearchPosition]()
	var lastNMarked = 0
	var deadEnd = false

	def someMarked = positionsMarked.size>2 //nonEmpty

	def enterPos {lastNMarked = slicesMarked.size}

	def markFalse(slice: LeadSlice) =
	{
		slice.isAvailable = false
		slicesMarked = slice::slicesMarked
	}

	def leavePos(pos: SearchPosition): Unit =
	{
		if (slicesMarked.size>lastNMarked)
		{
			positionsMarked = positionsMarked + pos
			if (!pos.headSlice.isAvailable)
				deadEnd = true
		}
	}
}

case class CompletionTracker(complete: Double, scale: Double)
{
	def spawn(i: Int, n: Int) =
	{
		val newScale = scale/n
		CompletionTracker(complete+newScale*i, newScale)
	}

	override def toString = "%1.5f".format(complete*100)
}

/**
 * Identifies a state within an ongoing tree search, represented as an array of 23 SearchPositions,
 * each of which are mutable instances which can be traversed up and down.
 */
class SliceSearchState(courses: Vector[SearchPosition], solutionFn: (SliceSearchState)=>Unit)
	extends CompositionState(courses) 
{
	/** Special return values for nextPos() */
	val NO_OPTIONS = -1
	val FINISHED = -2
	val nCourses = courses.size

	var totalSolutions: Long = 0

	var hiwater = 0
	var completionTracker = List[CompletionTracker](CompletionTracker(0, 1.0))
	var nodesSearched: Long = 0
	println("Slice search starts...")

	/** The search proceeds by mutual recursion between this method and the individual course search */
	def search(): Int  =
	{
		val progress = courses.map{_.depth}.sum
		if (progress>hiwater)
		{
			hiwater = progress
			println("Hiwater "+progress+": "+this)
		}
		nodesSearched+= 1
		if (nodesSearched%10000000==1)
			println(completionTracker.head+"% complete")
		val idx = nextPos
		if (idx>=0)
			courses(idx).search(this, idx)
		else if (idx==FINISHED)
		{
			totalSolutions+= 1
			solutionFn(this)
		}
		hiwater
	}

	/** Called to determine whether a new slice is acceptable for addition to the composition. */
	def shouldAcceptSlice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) = true

	/** Accepted slices next have their falseness marked; this method should return the record of slices marked false. */
	def markSliceFalse(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice): FalsenessRecord =
	{
		val marked = new FalsenessRecord()
		val it = courses.iterator
		// Mark false, abandoning loop if we dead-end.
		while (it.hasNext && !marked.deadEnd)
		{
			val other = it.next()
			if (pos!=other)
			{
				// Don't mark courses which have already moved beyond our depth
				val comparativeDepth = pos.depth - other.depth
				if (comparativeDepth >= 0)
				{
					marked.enterPos
					other.headSlice.markFalse(comparativeDepth, newRows, marked)
					marked.leavePos(other)
				}
			}
		}
		marked
	}

	/** Deep pruning, called after ordinary falseness marked has completed, but before the new slice is added. */
	def prune(currentPos: SearchPosition, marked: FalsenessRecord): Unit =
	{
		def isTrue(rows: RowArray, other: LeadSlice) = other.asInstanceOf[MidSlice].rowMap.exists{(p)=> p._2.isAvailable && p._1.trueWith(rows)}
		def isUnattainable(rows: RowArray, others: List[LeadSlice]) = others.exists{!isTrue(rows, _)}

		val pruneDepth = currentPos.depth
		while (!marked.deadEnd && marked.someMarked)
		{
			val markedPositionsToCheck = marked.positionsMarked.toList
			marked.positionsMarked = Set()
			val it = courses.iterator
			while (it.hasNext && !marked.deadEnd)
			{
				val other = it.next()
				if (currentPos!=other && other.depth==pruneDepth)
				{
					val otherSlice = other.headSlice
					val markedSlicesToCheck = markedPositionsToCheck.filter{(p)=> p.depth==pruneDepth && p!=other}.map{_.headSlice}
					if (markedSlicesToCheck.nonEmpty)
					{
						marked.enterPos
						for ((rows, childSlice) <- otherSlice.asInstanceOf[MidSlice].rowMap; if childSlice.isAvailable)
							if (isUnattainable(rows, markedSlicesToCheck))
								marked.markFalse(childSlice)
						marked.leavePos(other)
					}
				}
			}
		}
	}

	/** Called after a slice has been added to the composition; allows the state to update internal counters. */
	def sliceAccepted(idx: Int, pos: SearchPosition): Unit =
	{
		// No-op in the base class.
	}

	/** Can return true if an early backtrack (exit from current search position loop) is required */
	def backoutSplice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice): Boolean =
	{
		// Never any backouts in the base class
		false
	}

	def unmarkSliceFalse(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice, marked: List[LeadSlice]): Unit =
	{
		// Unmark false - only works properly if the marked list only includes slices which were *newly* marked false.
		for (slice <- marked)
			slice.isAvailable = true
	}

	/** The next position to search - by default the least branchy, for best pruning efficiency.
		* Returns -1 if we are stuck (no options remaining) or -2 if we are finished (have a solution). */
	protected def nextPos: Int = leastBranchy

	/** Efficiently returns the course position with the least branchiness, or -1 if the least branchy position has
		* no options remaining, or -2 if we are finished (have a solution) */
	protected def leastBranchy: Int =
	{
		var bestIdx = FINISHED
		var bestVal = Integer.MAX_VALUE.toDouble
		var i = 0
		while (bestVal>0 && i<nCourses)
		{
			val pos = courses(i)
			if (posAvailable(pos))
			{
				val branchiness = posBranchiness(pos)
				// Prefer courses with fewer slices filled in
				if (bestIdx<0 || branchiness<bestVal || (branchiness==bestVal && courses(i).depth<courses(bestIdx).depth))
				{
					bestVal = branchiness
					if (bestVal==0)
						bestIdx = NO_OPTIONS
					else
						bestIdx = i
				}
			}
			i+= 1
		}
		bestIdx
	}

	//protected def posAvailable(pos: SearchPosition) = pos.depth<4 // pos.isEmpty
	protected def posAvailable(pos: SearchPosition) = !pos.isFinished

	protected def posBranchiness(pos: SearchPosition) = pos.optionsRemaining //* (pos.depth*7.0+51.0) / 100

}

/** Lots of pruning ideas implemented here, all around variety of method - right/wrong place, starts/prefixes, above work. */
class PrefixVarietySearchState(courses: Vector[SearchPosition], solutionFn: (SliceSearchState)=>Unit)
		extends SliceSearchState(courses, solutionFn)
{
	val maxPrefixCount = 6	//3
	val prefixCounts = mutable.Map[String,Int]()
	val maxRightPlaceBelow = 14
	var rightPlaceBelow = 0
	val maxRightPlaceAbove = 14
	var rightPlaceAbove = 0
	val requiredPrefixes = Map("-38"->3, "-58"->4, "38-"->1, "58-"->3, "-36"->1).map{(p)=> ("("+p._1+")")->p._2}	// Must add () around prefix.
	val maxOtherPrefixes = 23 - requiredPrefixes.values.sum
	var otherPrefixes = 0
	val preventNeighbouringPrefixes = false

	/** Called to determine whether a new slice is acceptable for addition to the composition. */
	override def shouldAcceptSlice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		def prefixesAvailable(requiredPrefix: String) =
			courses.map{(sp) => if (sp!=pos && sp.isPrefixAvailable(requiredPrefix)) 1 else 0 }.sum

		/** Only works when adding a new root-level slice */
		def sufficientPrefixes(requiredPrefix: String) =
		{
			var inComp = prefixCounts.getOrElse(requiredPrefix, 0)
			val myPrefix = pos.root.pnPrefixString(newRows)
			if (myPrefix == requiredPrefix)
				inComp += 1
			val requiredLeft = requiredPrefixes(requiredPrefix) - inComp
			if (requiredLeft<=0)
				true
			else
			{
				val available = prefixesAvailable(requiredPrefix)
				available >= requiredLeft
			}
		}

		def goodPrefixUnavailable(sp: SearchPosition) = sp.isEmpty && !sp.root.isPrefixAvailable(requiredPrefixes.keySet)

		def totalGoodPrefixesAvailable =
		{
			val myPrefix = pos.root.pnPrefixString(newRows)
			requiredPrefixes.contains(myPrefix) || otherPrefixes+courses.map{goodPrefixUnavailable}.count(_==true)<=maxOtherPrefixes
		}

		def wrongPlaceBelowAvailable(sp: SearchPosition) = sp!=pos && sp.headSlice.wrongBelowAvailable

		def wrongPlaceAboveAvailable(sp: SearchPosition) = sp!=pos &&
				(if (sp.depth<3) sp.headSlice.wrongAboveAvailable else sp.headSlice.nWrongAbove>0)

		def sufficientWrongWork =
		{
			if (newSlice.isFinishLevel)
			{
				if (newSlice.nWrongBelow==0)
					courses.map(wrongPlaceBelowAvailable).count(_==true) >= 23-maxRightPlaceBelow
				else
					true
			}
			else if (pos.depth==3)
			{
				if (newSlice.nWrongAbove==0)
					courses.map(wrongPlaceAboveAvailable).count(_==true) >= 23-maxRightPlaceAbove
				else
					true
			}
			else
				true
		}

		if (super.shouldAcceptSlice(idx, pos, newRows, newSlice) &&
				(if (pos.isEmpty) totalGoodPrefixesAvailable && requiredPrefixes.keys.forall(sufficientPrefixes) else sufficientWrongWork))
			true
		else
			false
	}

	// These cache the new search values between the calls to markSliceFalse and acceptNewSlice.
	var ourPrefix = ""
	var newPrefixCount = 0
	var newOtherPrefixes = 0

	override def markSliceFalse(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		val marked = super.markSliceFalse(idx, pos, newRows, newSlice)
		if (!marked.deadEnd)
		{
			// Only check prefixes when we are adding a new root-level slice
			if (pos.isEmpty)
			{
				ourPrefix = pos.root.pnPrefixString(newRows)
				// Ensure we don't exceed the specified maxPrefixCount for our prefix, by marking others false if we've reached it.
				newPrefixCount = 1+prefixCounts.getOrElse(ourPrefix, 0)
				assert(newPrefixCount<=maxPrefixCount)
				if (newPrefixCount==maxPrefixCount)
				{
					val it = courses.iterator
					while (it.hasNext && !marked.deadEnd)
					{
						val other = it.next()
						if (other!=pos)
							other.markPrefixFalse(ourPrefix, marked)
					}
				}
				// Ensure we don't have methods in consecutive courses with the same prefix, by marking others false
				if (preventNeighbouringPrefixes && !marked.deadEnd)
				{
					courses((idx+23-1)%23).markPrefixFalse(ourPrefix, marked)
					if (!marked.deadEnd)
						courses((idx+1)%23).markPrefixFalse(ourPrefix, marked)
				}
			}
			// Check right-place below when we've finished the slice; tricky to check earlier than this because, even if
			// all slices below us are right-place, we don't want to end up counting twice when we descend.
			else if (newSlice.isFinishLevel)
			{
				if (newSlice.nWrongBelow==0)
				{
					if (rightPlaceBelow+1==maxRightPlaceBelow)
					{
						val it = courses.iterator
						while (it.hasNext && !marked.deadEnd)
						{
							val other = it.next()
							if (other!=pos)
								other.markRightPlaceBelowFalse(marked)
						}
					}
				}
			}
			// If at the quarter-lead, we can mark other instances of the same backwork false,
			// and also check for right-place backwork limits
			else if (pos.depth==3)
			{
				if (newSlice.nWrongAbove==0)
				{
					if (rightPlaceAbove+1==maxRightPlaceAbove)
					{
						val it = courses.iterator
						while (it.hasNext && !marked.deadEnd)
						{
							val other = it.next()
							if (other!=pos)
								other.markRightPlaceAboveFalse(marked)
						}
					}
				}
			}
			else if (pos.depth==4)
			{
				val pnToMatch = newSlice.pnStrings.head
				// Only mark above work false for positions which share our lh group - differing groups will give different lines
				// even for the same above-work.
				val it = courses.iterator
				while (it.hasNext && !marked.deadEnd)
				{
					val other = it.next()
					if (other!=pos && other.lhGroup==pos.lhGroup)
						other.markWorkFalse(pnToMatch, marked)
				}
			}
		}
		marked
	}

	override def sliceAccepted(idx: Int, pos: SearchPosition) =
	{
		val prevDepth = pos.depth-1
		if (prevDepth==0)
		{
			prefixCounts+= ourPrefix->newPrefixCount
			if (!requiredPrefixes.contains(ourPrefix))
				otherPrefixes+= 1
		}
		else if (pos.isFinished)
		{
			if (pos.headSlice.nWrongBelow==0)
				rightPlaceBelow+= 1
		}
		// If at the quarter-lead, we can mark other instances of the same backwork false,
		// and also check for right-place backwork limits
		else if (prevDepth==3)
		{
			if (pos.headSlice.nWrongAbove==0)
				rightPlaceAbove+= 1
		}
	}

	/** Can return true if an early backtrack (exit from current search position loop) is required */
	override def backoutSplice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		// Need to reduce prefixCounts if backing out a root-level slice
		if (pos.isEmpty)
		{
			val ourPrefix = pos.root.pnPrefixString(newRows)
			val n = prefixCounts(ourPrefix)
			assert(n<=maxPrefixCount)
			prefixCounts(ourPrefix) = n-1
			if (!requiredPrefixes.contains(ourPrefix))
				otherPrefixes-= 1
		}
		// Need to reduce rightPlaceBelow count if backing out a finished right-place slice
		else if (newSlice.isFinishLevel)
		{
			if (newSlice.nWrongBelow==0)
				rightPlaceBelow-= 1
		}
		// And reduce rightPlaceAbove count if backing out a right-place quarter-lead slice
		else if (pos.depth==3 && newSlice.nWrongAbove==0)
		{
			rightPlaceAbove-= 1
		}
		false
	}
}

class MusicPrunedPrefixVarietySearchState(courses0: Vector[SearchPosition], solutionFn0: (SliceSearchState)=>Unit)
		extends PrefixVarietySearchState(courses0, solutionFn0)
{
	val queensRequired = true
	var queensPossible = Array.fill(23)(true)
	var nSlicesEmpty = 23

	val requiredMusicPerSlice = 0.77
	var requiredMusic: Double = 0
	val compMusic = Array.fill(23)(0)

	val required5678MusicPerSlice = 0.17
	var required5678Music: Double = 0
	val comp5678Music = Array.fill(23)(0)

	override def shouldAcceptSlice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		if (super.shouldAcceptSlice(idx, pos, newRows, newSlice))
		{
			val newRequired = requiredMusic + requiredMusicPerSlice
			val newRequired5678 = required5678Music + required5678MusicPerSlice
			var i = 0
			var it = newSlice.compSliceMusic.perRotation.iterator
			var foundRot = false
			while (it.hasNext && !foundRot)
			{
				val rotMusic = it.next()
				if (queensPossible(i) && rotMusic.total+compMusic(i)>=newRequired && rotMusic.r5678+comp5678Music(i)>newRequired5678)
					foundRot = true
				i+= 1
			}
			foundRot
		}
		else
			false
	}


	override def sliceAccepted(idx: Int, pos: SearchPosition) =
	{
		if (pos.depth==1)
		{
			nSlicesEmpty -= 1
			if (queensRequired && nSlicesEmpty==0)
				for (i <- 0 until 23)
					queensPossible(i) = courses.exists{_.revSlices.last._2.compSliceMusic.perRotation(i).queens}
		}
		requiredMusic+= requiredMusicPerSlice
		required5678Music+= required5678MusicPerSlice
		var i = 0
		for (m <- pos.headSlice.compSliceMusic.perRotation)
		{
			compMusic(i)+= m.total
			comp5678Music(i)+= m.r5678
			i+= 1
		}
		super.sliceAccepted(idx, pos)
	}

	override def backoutSplice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		if (pos.isEmpty)
		{
			if (queensRequired && nSlicesEmpty==0)
				for (i <- 0 until 23)
					queensPossible(i) = true
			nSlicesEmpty += 1
		}
		requiredMusic-= requiredMusicPerSlice
		required5678Music-= required5678MusicPerSlice
		var i = 0
		for (m <- newSlice.compSliceMusic.perRotation)
		{
			compMusic(i)-= m.total
			comp5678Music(i)-= m.r5678
			i+= 1
		}
		super.backoutSplice(idx, pos, newRows, newSlice)
	}
}

/**
 * This one uses all the previous pruning types, but changes the search order so that a layer of slices
 * must be filled before moving onto the next, and layers after the first must be filled in the same order.
 */
class LayerPrunedSearchState(courses0: Vector[SearchPosition], solutionFn0: (SliceSearchState)=>Unit)
		extends SliceSearchState(courses0, solutionFn0)
{
	/** The cumulative number of slices in each layer. Here we have three layers, with 4, 2 and 2 slices in each. */
	val layers = Array(2,4,6,8)
	/** The layer we are currently searching. */
	var currentLayer = 0
	/** How many search positions have been completely filled to the current layer. */
	var currentLayerFillCount = 0
	/** Keep track of the order we visit search positions in the first layer. This order is re-used for higher layers. */
	var firstLayerFillOrder = Array.fill(nCourses)(-1)
	/** The maximum number of search positions we have filled in a layer. Reset to zero whenever we start searching a new layer. */
	val layerHighwaters = Array.fill(layers.size)(0)
	/** Set if failure of a higher layer (hiwater<nCourses) should cause forced backtrack at the next lower level. */
	var forceBacktrack = -1

	override protected def nextPos =
	{
		// Are we at the end of a layer? If so we're either finished, or we need to start with the first search position from layer 0
		if (currentLayerFillCount==nCourses)
		{
			if (currentLayer+1==layers.size)
				FINISHED
			else
				firstLayerFillOrder(0)
		}
		// Are we in layer 0, requiring selection of a new position? (Triggered by special -1 value in the firstLayerFillOrder array).
		else if (firstLayerFillOrder(currentLayerFillCount)<0)
		{
			leastBranchy
		}
		// Otherwise, keep adding slices to the current layer, following (for higher layers) the fill order of layer 0.
		else
		{
			firstLayerFillOrder(currentLayerFillCount)
		}
	}

	/** Override this to ensure {@link #leastBranchy} only ever considers layer 0 positions. */
	override protected def posAvailable(pos: SearchPosition) = pos.isEmpty

	override def sliceAccepted(idx: Int, pos: SearchPosition) =
	{
		val layerDepth = layers(currentLayer)
		val prevDepth = pos.depth-1
		// Are we spilling over this layer into a new one?
		if (prevDepth==layerDepth)
		{
			currentLayer += 1
			currentLayerFillCount = 0
			layerHighwaters(currentLayer) = 0
		}
		// Are we filling the current layer?
		else if (prevDepth==layerDepth-1)
		{
			currentLayerFillCount += 1
			// For layer 0, ensure we reset the fill order of the next position to -1, to trigger a new search position selection
			if (currentLayer==0 && currentLayerFillCount<nCourses)
				firstLayerFillOrder(currentLayerFillCount) = -1
			// The hiwater in the current layer may have increased
			if (currentLayerFillCount > layerHighwaters(currentLayer))
				layerHighwaters(currentLayer) = currentLayerFillCount
		}
		else if (currentLayer==0)
			firstLayerFillOrder(currentLayerFillCount) = idx
		super.sliceAccepted(idx, pos)
	}

	override def backoutSplice(idx: Int, pos: SearchPosition, newRows: RowArray, newSlice: LeadSlice) =
	{
		var backtrack = super.backoutSplice(idx, pos, newRows, newSlice)
		// Are we returning to a previous layer?
		if (currentLayerFillCount==0 && currentLayer>0 && pos.depth==layers(currentLayer-1))
		{
			// If the hiwater of the higher layer was less than the maximum, we should force a backtrack
			if (layerHighwaters(currentLayer)<nCourses)
			{
				backtrack = true
				forceBacktrack = layerHighwaters(currentLayer)
			}
			currentLayer -= 1
			currentLayerFillCount = nCourses
		}
		else
		{
			// Are we reducing a position which had previously filled the current layer?
			if (pos.depth==layers(currentLayer)-1)
				currentLayerFillCount -= 1
			// Are we still forcibly backtracking?
			if (forceBacktrack>0)
			{
				if (currentLayerFillCount>forceBacktrack)
					backtrack = true
				else
					forceBacktrack = -1
			}
		}
		backtrack
	}

}


/** Mutable implementation. */
case class SearchPosition(root: Stalactite)
{
	var revSlices = List[(RowArray, LeadSlice)]()

	def isEmpty = revSlices.isEmpty
	def isFinished = headSlice.isFinishLevel
	def depth = revSlices.size
	def headSlice = if (isEmpty) root else revSlices.head._2
	def optionsRemaining = headSlice.nChildrenAvailable
	def allRows = revSlices.flatMap{_._1.toRows}
	def lhGroup = root.course.lhGroup

	def search(state: SliceSearchState, currentIdx: Int): Unit =
	{
		val head = headSlice.asInstanceOf[MidSlice]
		val nOptions = head.nChildrenAvailable
		var i = 0
		val it = head.rowMap.filter{_._2.isAvailable}.iterator
		var earlyBacktrack = false
		while (it.hasNext && !earlyBacktrack)
		{
			val (rows, childSlice) = it.next()
			val prevCompletion = state.completionTracker.head
			// Allow state to reject slice based on other courses already found - e.g. ensure enough variety in PN prefix.
			if (state.shouldAcceptSlice(currentIdx, this, rows, childSlice))
			{
				// This call to the state is responsible for false count marking. Note the slice hasn't been added on yet.
				val marked = state.markSliceFalse(currentIdx, this, rows, childSlice)
				if (!marked.deadEnd)
				{
					// If we have done any false marking, run a deep prune operation. Will result in further false-marking.
					//if (marked.someMarked)
						state.prune(this, marked)
					if (!marked.deadEnd)
					{
						// Only if the composition still has positions available do we accept the slice and proceed.
						revSlices = (rows, childSlice) :: revSlices
						state.sliceAccepted(currentIdx, this)
						state.completionTracker = prevCompletion.spawn(i, nOptions)::state.completionTracker
						state.search()
						state.completionTracker = state.completionTracker.tail
						revSlices = revSlices.tail
						earlyBacktrack = state.backoutSplice(currentIdx, this, rows, childSlice)
					}
				}
				state.unmarkSliceFalse(currentIdx, this, rows, childSlice, marked.slicesMarked)
			}
			i+= 1
		}
	}

	/** Only mark false if we have not already started this position.
		* Marking false after use may prevent other markFalse operations descending this tree to the right level... */
	def markPrefixFalse(prefix: String, marked: FalsenessRecord): Unit =
	{
		if (isEmpty)
		{
			marked.enterPos
			root.markPrefixFalse(prefix, marked)
			marked.leavePos(this)
		}
	}

	def isPrefixAvailable(prefix: String) = isEmpty && root.isPrefixAvailable(prefix)

	def markRightPlaceBelowFalse(marked: FalsenessRecord): Unit =
	{
		if (!isFinished && headSlice.isAvailable)
		{
			marked.enterPos
			headSlice.markRightPlaceBelowFalse(marked)
			marked.leavePos(this)
		}
	}

	def markRightPlaceAboveFalse(marked: FalsenessRecord): Unit =
	{
		if (depth<=3 && headSlice.isAvailable)
		{
			marked.enterPos
			headSlice.markRightPlaceAboveFalse(marked)
			marked.leavePos(this)
		}
	}

	def markWorkFalse(pnToMatch: String, marked: FalsenessRecord): Unit =
	{
		val level = 4-depth
		if (level>=0 && headSlice.isAvailable)
		{
			marked.enterPos
			headSlice.markWorkFalse(pnToMatch, level, marked)
			marked.leavePos(this)
		}
	}

	/** For the simulated annealing search */
	def randomMove(composition: Vector[SearchPosition]): Unit =
	{
		val ascend = revSlices.nonEmpty && Random.nextInt(4)==0
		if (ascend)
		{
			revSlices = revSlices.tail
		}
		else if (!isFinished)
		{
			val head = headSlice
			val n = head.nChildrenAvailable / 2
			var i = n
			while (i >= 0)
			{
				val (rows, childSlice) = headSlice.randomSlice
				if (sliceTrue(rows, composition))
				{
					revSlices = (rows, childSlice) :: revSlices
					i = 0
				}
				i-= 1
			}
		}
	}

	def descendMethod(pn: String): Unit =
	{
		revSlices = root.descendMethod(pn).reverse
	}

	def branchiness(composition: Vector[SearchPosition]): Double =
	{
		var b = nSliceTrue(composition).toDouble
		if (b>1)
			b = 1 + (b/10)
		b
	}

	def nSliceTrue(composition: Vector[SearchPosition]): Int =
	{
		if (isFinished)
			12
		else
		{
			var n = 0
			for ( (rows, childSlice) <- headSlice.asInstanceOf[MidSlice].rowMap; if childSlice.isAvailable)
				if (sliceTrue(rows, composition)) n += 1
			n
		}
	}

	def sliceTrue(rows: RowArray, composition: Vector[SearchPosition]): Boolean =
		composition.forall{(pos)=> pos==this || sliceTrue(rows, pos)}

	private def sliceTrue(rows: RowArray, other: SearchPosition): Boolean =
	{
		var isTrue = true
		var otherSlices = other.revSlices
		var finished = otherSlices.isEmpty
		while (!finished)
		{
			val otherRows = otherSlices.head._1
			if (otherRows.treblePos <= rows.treblePos)
			{
				finished = true
				isTrue = otherRows.treblePos<rows.treblePos || rows.trueWith(otherRows)
			}
			else
			{
				otherSlices = otherSlices.tail
				finished = otherSlices.isEmpty
			}
		}
		isTrue
	}

	/** Slightly complicated, to get a nice print for half-completed leads as well as full PN information for solutions */
	override def toString =
	{
		var buf = new mutable.StringBuilder()
		buf++= root.toString()
		if (isFinished)
		{
			buf++= root.describeLink(revSlices.last._1)
			buf++= revSlices.head._2.toString
		}
		else
		{
			var slice: LeadSlice = root
			var slices = revSlices.reverse
			while (slices.nonEmpty)
			{
				buf++= slice.describeChild(slices.head._1)
				slice = slices.head._2
				slices = slices.tail
			}
		}
		buf.toString()
	}

	def methodMusic =	headSlice.methodMusic

	override def equals(obj: scala.Any) = obj match
	{
		case other: SearchPosition => root.course.pos==other.root.course.pos
		case _ => false
	}
}

