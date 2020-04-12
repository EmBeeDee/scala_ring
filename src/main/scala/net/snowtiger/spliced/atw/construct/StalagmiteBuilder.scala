package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw._
import net.snowtiger.spliced.tables.TableBuildProgress

import scala.collection.mutable

/**
 * Builds all possible stalagmites (inner sections around the half-lead) for the given method-courses of the ATW composition.
 * @author mark
 */
class StalagmiteBuilder(methodLeads: List[MethodLeads])
{
	val stalagmites: Array[Stalagmite] = methodLeads.map(buildSection).toArray

	def buildSection(methodLeads: MethodLeads): Stalagmite =
	{
		print("Build stalagmites: "+methodLeads)
		val progess = new TableBuildProgress(100)
		val methodCourse = methodLeads.course
		val firstLH = methodCourse.leadheads.head
		def permHLwithinCourse(lh: Row, row: Row) = row.permuteBy(firstLH.permutationBetween(lh))
		var result = new Stalagmite(methodLeads)
		for ((hlRow,hlPN) <- methodLeads.halfleads)
		{
			def recursiveBuild(headRows: List[Row], allRows: MultiBitSet, music: MusicTracker, pnsFound: List[PN], pnsToTry: List[List[PN]]): Unit =
			{
				pnsToTry match
				{
					case Nil =>
					{
						val firstPn = pnsFound.head
						val startRow = headRows.head
						result.add(allRows, pnsFound, startRow, hlRow, music)
						progess.emit
					}
					case nextPns::remainingPns =>
					{
						val requiredRowsSize = allRows.size+headRows.size
						val lastPN = pnsFound.head
						// The extra test ensures mid-section of the form -P- or P-Q
						for (pn <- nextPns; if pn.acceptableConsecutive(lastPN) && ((pnsFound.size/2 &1)==0 || /*pn.disjointPlaces(lastPN) */ pn.isRightPlaceAgainst(lastPN)  ))
						{
							val newHeadRows = headRows.map(_.apply(pn))
							val newAllRows = allRows + MultiBitSet(newHeadRows)
							val newMusic = music.add(newHeadRows)
							if (newAllRows.size==requiredRowsSize)
								recursiveBuild(newHeadRows, newAllRows, newMusic, pn::pnsFound, remainingPns)
						}
					}
				}
			}
			val handstrokeHLs = methodCourse.leadheads.map{permHLwithinCourse(_, hlRow)}
			val backstrokeHLs = handstrokeHLs.map{_.apply(hlPN)}
			val allHLrows = handstrokeHLs++backstrokeHLs
			val pnsToTry = SectionTables.stalagmitePNs.reverse
			recursiveBuild(allHLrows, MultiBitSet(allHLrows), new MusicTracker(allHLrows), List(hlPN), pnsToTry)
		}
		println()
		result
	}
}

/**
 * The set of all valid options for the last section of a given method, held in a map which separates
 * different starting rows and PNs. It is no longer a MidSlice in its own right. Note that because the
 * last sections are separated by starting row and PN, it may be necessary to merge some finishes back
 * together when joining up with a Stalactite, but this is done implicitly by the Stalactite add().
 * <p>
 * See {@link Stalactite} for full details about the tree of RowArrays and the SliceFinish roots.
 */
class Stalagmite(course: MethodLeads)
{
	val starts = mutable.Map[Row, mutable.Map[PN, MidSlice]]()

	def add(bitset: MultiBitSet, pn: List[PN], startRow: Row, finalRow: Row, music: MusicTracker): Unit =
	{
		val newRows = (7 to 8).toList.map{(n)=> RowArray(bitset.treblePosSets.getOrElse(n, RowBitSet(n)))}
		// No method music for Stalagmites, since they are not calculated from rounds
		// Also only use the per-slice comp music for Stalagmites - cumulative values will be calculated when adding to the stalactites.
		val finish = new SliceFinish(mutable.Set(pn), 0, music.compSliceMusic.perSlice(8), music.getTotalCompMusic())
		val startPn = pn.head
		val newRowStart = starts.getOrElseUpdate(startRow, mutable.Map[PN, MidSlice]())
		val slice = newRowStart.getOrElseUpdate(startPn, new MidSlice(mutable.Map(), Set(), 0, music.compSliceMusic.perSlice(7)))
		slice.add(newRows, music, finish)
	}

	/** Link up to the stalagmites, using only those compatible with the link PN, and create completed columns with
		* music calculated from rounds for all 32 rows. Note linkPN = stalacPN.last */
	def addToStalactite(stalactite: Stalactite, stalagStart: Row, stalacPNs: List[PN], linkPN: PN, stalacRows: MultiBitSet, music: MusicTracker): Unit =
	{
		if (starts.contains(stalagStart))
		{
			// Convert rows to slices (row arrays with the treble in one place)
			val stalacRowArrays = (1 to 6).toList.map{(n)=> RowArray(stalacRows.treblePosSets.getOrElse(n, RowBitSet(n)))}

			def visitor(revSlices: List[(RowArray,LeadSlice)], finish: SliceFinish): Unit =
			{
				var compMusic = music
				var stalagRowArrays = List[RowArray]()
				for ( (rows,slice) <- revSlices)
				{
					stalagRowArrays = rows::stalagRowArrays
					compMusic = compMusic.addStalagCompMusic(rows.treblePos, slice.compSliceMusic)
				}
				// The finishes are already true to the method's leads in the composition, so we do not have to do further checking
				val finishedRowArrays = stalacRowArrays++stalagRowArrays
				for (stalagPNs <- finish.getFinishMethods)
				{
					val methodPN = stalacPNs++stalagPNs
					// Ignore library methods (other than Lessness)
					if (!StalactiteBuilder.isLibraryMethod(methodPN, stalactite.course.lhGroup))
					{
						// Note as usual the finalRow is with respect to this method's starting leadhead in the composition,
						// whereas the musicRow has started from rounds (to assess the method's quality in its own right).
						var finalRow = stalagStart
						var finalMusic = compMusic
						for (pn <- stalagPNs.reverse.tail.reverse)
						{
							finalRow = finalRow.apply(pn)
							finalMusic = finalMusic.next(pn)
						}
						// Note that the Stalactite.add() method may choose to ignore this method, if e.g. music not good enough.
						stalactite.add(finishedRowArrays, methodPN, finalMusic, finalRow)
					}
				}
			}

			for ( (pn,stalag) <- starts(stalagStart); if pn.acceptableConsecutive(linkPN))
				stalag.visitFinishes(Nil, visitor)
		}
	}

	override def toString = "E"+course.pos
}

