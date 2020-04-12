package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.atw._
import net.snowtiger.spliced.atw.construct.SectionTables._
import net.snowtiger.spliced.atw.construct.StalactiteBuilder._
import net.snowtiger.spliced.tables.TableBuildProgress

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StalactiteBuilder
{
	val minMusic = 0

	val onlySurprise = true
	val onlyDelight = false

	var totalMethods: Long = 0

	val Lessness = PN.parse("-38-14-56-16-12-58-14-58")
	val libraryMethodLookup = (SolutionOutputter.libraryMethodsByPn-Lessness).mapValues{_.map{_.lhGroup}.toSet}.view.force

	def isLibraryMethod(method: Seq[PN], lhg: String) = libraryMethodLookup.get(method) match
	{
		case None => false
		case Some(lhGroups) => lhGroups.contains(lhg)
	}
}

/**
 * Builds the stalactites (the first 3 sections) for a given method course and LH group, for all possible methods.
 * It now marries them up to the stalagmites (the 4th sections) to create completed columns (methods).
 *
 * @author mark
 */
class StalactiteBuilder(val methodLeads: List[MethodLeads], val stalagmites: Array[Stalagmite])
{
	val stalactites: Vector[Stalactite] = methodLeads.map{new Stalactite(_, new TableBuildProgress(1000))}.toVector
	val stalacLookup = stalactites.map{(s)=> (s.course.pos, s)}.toMap
	/** Only one method per position can be forced, although the depth of PN can be as small as you like. No other PN prefixes will be considered. */
	val methodsForced = mutable.Map[String,List[PN]]()
	/** Multiple methods (PN prefixes) can be avoided - they are removed from the finished stalactite+stalagmite columns. */
	val methodsAvoided = mutable.Map[String,List[List[PN]]]()
	/** Mutiple methods can be allowed, but this is only done if no single method is forced. It is the same as avoiding all methods *not* in the list. */
	val methodsAllowed = mutable.Map[String,List[List[PN]]]()

	println("Loading library methods... ")
	assert(!isLibraryMethod(StalactiteBuilder.Lessness, "f"))
	forceMethods()
	buildColumns()

	println("Total methods: "+totalMethods)
	println(Atw23Music.musicAnalysis)

	pruneAll()
	countWrongWork()

	def buildColumns(): Unit =
	{
		stalactites.zip(stalagmites).foreach{(p)=> buildColumns(p._1, p._2)}
	}

	def buildColumns(stalactite: Stalactite, stalagmite: Stalagmite): Stalactite =
	{
		val methodLeads = stalactite.course
		print("Build stalactites: "+methodLeads)
		val lhg = methodLeads.lhGroup
		val leadheads = methodLeads.course.leadheads
		val leadends = methodLeads.course.leadends(lhg)
		val lhPN = methodLeads.course.lhPN(lhg)
		// This now includes the link PN (between 3rd+4th sections)
		var pnsToTry = stalactitePNs
		if (onlySurprise)
			pnsToTry = surpriseOnly(pnsToTry)
		val pos = methodLeads.pos
		// A forced method overrides any allowed methods
		if (methodsForced.contains(pos))
		{
			val pnsToForce = methodsForced(pos).take(pnsToTry.size)
			pnsToTry = pnsToTry.drop(pnsToForce.size)
			pnsToTry = pnsToForce.map{List(_)} ++ pnsToTry
		}
		else if (methodsAllowed.contains(pos))
		{
			// We can't completely specify the branches allowed; the best we can do is, at each change in the lead,
			// force the pns allowed to be the superset of the set of PNs at that change position from each allowed method.
			var pnsAllowed = methodsAllowed(pos)
			val shortest = pnsAllowed.map{_.size}.min
			// If the methods allowed are different lengths, can't really do anything here with the longer ones
			pnsAllowed = pnsAllowed.map{_.take(shortest)}
			// Transpose to group all the PNs for a given change/position together
			pnsAllowed = pnsAllowed.transpose.map{_.toSet.toList}
			pnsToTry = pnsToTry.drop(shortest)
			pnsToTry = pnsAllowed++pnsToTry
		}

		def recursiveBuild(headRows: List[Row], allRows: MultiBitSet, music: MusicTracker, revPnFound: List[PN], pnsToTry: List[List[PN]]): Unit =
		{
			val lastPN = revPnFound.head
			pnsToTry match
			{
				case Nil =>
				{
					val stalagStart = headRows.head
					val stalacPNs = revPnFound.reverse.tail
					if (!onlyDelight || isDelight(stalacPNs))
					{
						// Link up to the stalagmites, creating completed columns with music calculated from rounds for all 32 rows.
						// The stalagmite instance will only give us finishes which start from the right row and are compatible with the link PN.
						stalagmite.addToStalactite(stalactite, stalagStart, stalacPNs, lastPN, allRows, music)
					}
				}
				case nextPns::remainingPns =>
				{
					val requiredRowsSize = allRows.size+headRows.size
					// The extra test ensures mid-sections of the form -P- or P-Q
					val rowsFound = revPnFound.size
					for (pn <- nextPns; if pn.acceptableConsecutive(lastPN) &&
							( (rowsFound<4 && pn.disjointPlaces(lastPN)) || (rowsFound/2 &1)==0 || pn.isRightPlaceAgainst(lastPN) || (rowsFound>4 && rowsFound<8) )
							//(rowsFound>4 && rowsFound<8) (rowsFound>=4 || pn.disjointPlaces(lastPN))
					)
					{
						val newHeadRows = headRows.map(_.apply(pn))
						val newAllRows = allRows + MultiBitSet(newHeadRows)
						if (newAllRows.size==requiredRowsSize)
						{
							// Since the Stalactites cross over into the start row of the Stalagmite, must make sure we don't double-count music.
							val newMusic = if (remainingPns.isEmpty) music.next(pn) else music.next(pn, newHeadRows)
							recursiveBuild(newHeadRows, newAllRows, newMusic, pn :: revPnFound, remainingPns)
						}
					}
				}
			}
		}

		val rows = leadheads++leadends
		val startMusic = new MusicTracker(Row(8), rows)
		recursiveBuild(rows, MultiBitSet(rows), startMusic, List(lhPN), pnsToTry)
		println
		stalactite
	}

	def forceMethods(): Unit =
	{
		forceMethod("C3.5 -38-14-56-16-12-58-14-58")  // Lessness

		// l1
		//forceMethod("C0.0 58-34.16-56.12.36-34-38-14-58")
		//forceMethod("C3.4 -56-16-56-38-12-58-14-18")
		//forceMethod("C3.5 -38-14-56-16-12-58-14-58")

		// q7
		//forceMethod("C0.0 58-56.14.58.12-38-12-58.14-34.78")
		//forceMethod("C3.1 38-56.14-12.58.36-34-1258-16-18")
		//forceMethod("C3.4 -38-14-12-16-12-58.12-36.18")

		//forceMethod("C0.0 36-38.14-12-")
		//forceMethod("C0.4 38-36.1458-58-")
		//forceMethod("C0.2 -58-1458-12-")

		// p4
		//forceMethod("C0.4 38-56.14-56-38-12-1458.36-36.18")
		//forceMethod("C3.0 -56-14.56-56.38-14-58-14-18")
		//forceMethod("C3.5 -38-14-56-16-12-58-14-58")

		// Forcing 0.2, 3.2, 3.5 took 45 hours on music-pruned search, giving 805 (142/28,13462578)
		//forceMethod("C0.4 38-56.14-56-38-12-1458.36-36.18")
		//forceMethod("C1.0 56-56.14-12-36-12-58-16-58")
		//forceMethod("C0.2 -38-14-12.58.36-34-1458.36-36.78")
		//forceMethod("C1.4 -38-14.56.12.56.38-34-1458-36-58")
		//forceMethod("C3.2 -38-14-56-38.14-34.58-56-58")
		//forceMethod("C3.5 -38-14-56-16-12-58-14-58")

		/*
		allowMethods("C0.0 -58-")
		allowMethods("C0.4 38-56")
		allowMethods("C0.1 56-38 56-36 -56- 56-56")
		allowMethods("C0.3 -58-")
		allowMethods("C0.6 -36-")
		allowMethods("C1.0 58-36 -56- 56-56")
		allowMethods("C1.3 34-36 -34- -34.56 34-34 34-56 38-58 34.56-")
		allowMethods("C1.5 58-38 58-58")
		allowMethods("C1.6 -38- -56- 56-56 56-38")
		allowMethods("C1.1 58-36 58-38 58.34.58")
		allowMethods("C2.0 -36- 58-38 58-58")
		allowMethods("C2.1 -56- 56-56 56-34 -56.34 56-58")
		allowMethods("C2.3 -34- 34-34 -36- -34.58 34-58")
		allowMethods("C2.6 -56- -38- 56-56 56-36 56-38")
		allowMethods("C2.5 -58- -58.34")
		allowMethods("C3.0 -56- -38- 56-56 -58- 56-36 56-38")
		allowMethods("C3.2 -38-")
		allowMethods("C3.1 38-56 -58- 38-38 58-56 38-58")
		allowMethods("C3.4 -56- 56-56 -58- 56-36 56-58")
		allowMethods("C3.6 -36- -56- 56-56 -58-")
		*/

		//forceMethod("C0.2 -38-14.56.12.58.36.12-14.58-36-78")
		//forceMethod("C1.6 -38-14-58.12.38.14-14.38-36-38")

		//avoidMethod("C3.0 -58-14.58-56.38-14-58-14-18")
		//forceMethod("C0.2 -38-14-12.58.36-34-1458.36-36.78")
		//forceMethod("C1.4 -38-14.56.12.56.38-34-1458-36-58")

		// p2
		//forceMethod("C0.4 -56-14-56-38.12-12.58.14-14.58")
		//forceMethod("C1.6 -38-14.56-56.38-14-38.12-12.78")

		// n2
		//forceMethod("C0.6 -56-14-56-38-14-58-14-58")	// Cornwall
		//forceMethod("C3.5 -38-14-56-16-12-58-14-58")  // Lessness

		// m2
		//forceMethod("C0.1 -58-14-56-38-14-1258-36-38")
		//forceMethod("C2.5 -36-16-58-38")
		//forceMethod("C3.4 -56-16-56-38-34-1458-14-18")
		//forceMethod("C3.5 -38-14-56-16-12-58-14-58")  // Lessness

		// l2
		/*
		forceMethod("C0.1 -58-14.56-12.36-12-58.34-36.18")
		forceMethod("C3.5 -38-14.58-12.38.14-12.58.14-34.58")
		*/
		//forceMethod("C1.6 56-34.16.58-12.36-12-58.16-12.38")
		//forceMethod("C2.0 34.58-1458-12-38-34-38.12-12.78")

		// o2
		//forceMethod("C0.1 -58-14-56-38.14-12.58-36-18")
		//forceMethod("C3.6 -36-14-56-36.14-34.58-16-38")

		// Panserbjorn
		//forceMethod("2.5", "-34.58.14-58-16-14-38.16-56.18")
	}

	def pruneAll(): Unit =
	{
		// Finish off forced and avoided methods
		for (pos <- methodsForced.keys)
		{
			val stalactite = stalacLookup(pos)
			val rows = stalactite.pnToRows(methodsForced(pos))
			stalactite.forceMethods(List(rows))
		}
		for (pos <- methodsAvoided.keys; method <- methodsAvoided(pos))
		{
			val stalactite = stalacLookup(pos)
			for (pn <- methodsAvoided(pos); rows = stalactite.pnToRows(pn))
				stalactite.avoidMethod(rows)
		}
		for (pos <- methodsAllowed.keys; method <- methodsAllowed(pos))
		{
			val stalactite = stalacLookup(pos)
			val rowLists = methodsAllowed(pos).map{stalactite.pnToRows}
			stalactite.forceMethods(rowLists)
		}

		println("Sizes before pruning: "+stalactites.map{_.size}.mkString(" "))
		var totalPruned = 1
		while (totalPruned>0)
		{
			totalPruned = 0
			for (level <- 1 to 4)
			{
				print("Pruning level "+level+":")
				for (i <- 0 until stalactites.size)
				{
					val courseToPrune = stalactites(i)
					val others = stalactites.take(i) ++ stalactites.drop(i + 1)
					val nPruned = courseToPrune.pruneAgainst(others, level)
					print(" "+nPruned)
					totalPruned+= nPruned
				}
				println
			}
		}
		val prunedSizes = stalactites.map{_.size}
		println("Sizes after pruning: "+prunedSizes.mkString(" ")+" total "+prunedSizes.sum)
	}

	def countWrongWork(): Unit =
	{
		for (pos <- stalactites)
			pos.countWrongWork()
	}

	def forceMethod(cMethod: String): Unit =
	{
		println("Force method "+cMethod)
		val split = cMethod.split(' ')
		val pos = split(0).substring(1)
		val method = PN.parseToList(split(1), 8)
		methodsForced+= pos -> method
	}

	def avoidMethod(cMethod: String): Unit =
	{
		println("Avoid method "+cMethod)
		val split = cMethod.split(' ')
		val pos = split(0).substring(1)
		val method = PN.parseToList(split(1), 8)
		val newList = method::methodsAvoided.getOrElse(pos, List())
		methodsAvoided+= pos->newList
	}

	def allowMethods(cMethods: String): Unit =
	{
		println("Allow methods "+cMethods)
		val split = cMethods.split(' ')
		val pos = split(0).substring(1)
		val methods = split.tail.toList.map{PN.parseToList(_, 8)}
		val newList = methods++methodsAllowed.getOrElse(pos, List())
		methodsAllowed+= pos->newList
	}

}

/**
 * The set of all valid options for the first three sections of a given method for a given start row. The start row
 * (the 1st leadhead of the method-course) is held externally to this class (e.g. in an Array of method courses).
 * <p>
 * We hold the set of all sections from the start row in an optimised form, basically a series of nested Maps:
 * (valid rows with treble in 1st's) -> (rows in 2nd's) -> (3rd's) -> (4th's) -> (5th's) -> (6th's). This helps rapid
 * truth-checking, since if the treble-in-1st's rows are false against a candidate first section, then there is no point
 * checking all the options for 2nd's-6th's. We use RowArrays instead of RowBitSets to save memory, since there are only
 * 4*7 = 28 16-bit rows in each set, compared with around 150 32-bit words in a RowBitSet.
 * <p>
 * The key in the final Map is the RowArray with the treble in 6th's; if we get this far then we have RowArrays
 * for the complete first three sections. However there may still have been different PN strings which generated these
 * rows, because (for example) 56-56 generates the same rows as -56-. Hence, the key in the final Map must link to a set
 * of PN lists. These are held in reverse order, so that the head element is the final PN; the final PN may differ, which
 * can can affect what options are available for the final section.
 */
class Stalactite(val course: MethodLeads, val progress: TableBuildProgress) extends MidSlice(mutable.Map(), Set(), 0, new CompRotMusic())
{
	def add(newRows: List[RowArray], newMethod: List[PN], music: MusicTracker, finalRow: Row): Unit =
	{
		val totalMusic = music.methodMusic.total
		Atw23Music.addMethodMusic(totalMusic)
		if (totalMusic >= minMusic)
		{
			totalMethods+= 1
			val finish = new SliceFinish(mutable.Set(newMethod), totalMusic, music.getCompMusic(8), music.getTotalCompMusic())
			add(newRows, music, finish)
		}
		progress.emit
	}

	/** Overridden for LinkedInnerSections - allows us to output the linking PN */
	def describeLink(rows: RowArray) = "/"

	def markPrefixFalse(prefix: String, marked: FalsenessRecord): Unit =
	{
		var someTrue = false
		for ( (rows, slice) <- rowMap)
			if (slice.isAvailable)
				if (prefix==pnPrefixString(rows))
					marked.markFalse(slice)
				else
					someTrue = true
		if (!someTrue)
			marked.markFalse(this)
	}

	def isPrefixAvailable(prefix: String) = rowMap.exists{(p)=> p._2.isAvailable && prefix==pnPrefixString(p._1)}

	/** Returns true if any one prefix is available at the root level */
	def isPrefixAvailable(prefixes: Set[String]) = rowMap.exists{(p)=> p._2.isAvailable && prefixes.contains(pnPrefixString(p._1))}

	/** Returns the number of items pruned. */
	def pruneAgainst(others: Vector[Stalactite], pruneLevel: Int): Int =
	{
		pruneAgainst(others, Nil, pruneLevel)
	}

	def pnToRows(method: List[PN]): List[RowArray] =
	{
		var rows = course.course.leadheads++course.leadends
		val allRows = ListBuffer[Row]()
		for (pn <- method)
		{
			allRows++= rows
			rows = rows.map(_.apply(pn))
		}
		val bitset = MultiBitSet(allRows)
		val rowArrays = (1 to 8).toList.map{(n)=> RowArray(bitset.treblePosSets.getOrElse(n, RowBitSet(n)))}
		rowArrays
	}

	/** Returns the list of slices corresponding to the given method PN (which can be partial). */
	def descendMethod(method: String): List[(RowArray, LeadSlice)] =
	{
		var rows = course.course.leadheads++course.leadends
		val allRows = ListBuffer[Row]()
		for (pn <- PN.parse(method, 8))
		{
			allRows++= rows
			rows = rows.map(_.apply(pn))
		}
		val bitset = MultiBitSet(allRows)
		val rowArrays = (1 to 8).toList.map{(n)=> RowArray(bitset.treblePosSets.getOrElse(n, RowBitSet(n)))}
		val slices = descendMethod(rowArrays)
		rowArrays.take(slices.size).zip(slices)
	}

	override def describeChild(rows: RowArray) = pnPrefixString(rows)+"/"

	def pnPrefixString(rows: RowArray) = rowMap(rows).pnStrings.mkString("(",",",")")

	override def toString = "C"+course.pos

}
