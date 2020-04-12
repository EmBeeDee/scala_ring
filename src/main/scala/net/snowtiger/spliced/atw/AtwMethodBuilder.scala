package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.atw.construct._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder


/**
 * Builds new methods to fulfill an ATW composition.
 * @author mark
 */
object AtwMethodBuilder
{
	val nbells = 12

	val inputCompReader = new InputCompReader(nbells)
	val inputFile = "major4sd.lst"
	//val inputFile = "atw8s4e.out"
	val comps = inputCompReader.read(inputFile, 5)

	val search = new AtwMethodFinder8(nbells)

	def main(args: Array[String])
	{
		println("Atw method build starts, from input file "+inputFile)

		buildComp(comps, 2)

		//for (n <- 0 until comps.size)
		//	buildComp(comps, n)

	}

	def buildComp(comps: List[InputComp], n: Int): Unit =
	{
		println("************************* COMP "+n+" *****************************")
		val comp = comps(n)
		println()
		println(comp)
		buildComp(comp)
	}

	case class SpliceSpace(name: String, n: Int, lhg8: Char, lhgCounts: Map[String,Int])
	{
		assert(lhgCounts.contains(lhg8.toString))
		def splices(comp: InputComp) =
		{
			println(this)
			val suitableStructures = comp.courses.map{determineSuitableSplices(_).filter(plentyGoodEnoughG(_, lhg8))}
			val suitableSplices = determineSplicesMeetingCriteria(suitableStructures, lhgCounts)
			suitableSplices(n-1)
		}
		override def toString = "Splice space = "+name+n
	}

	def buildComp(comp: InputComp): Unit =
	{
		// Both work
		val spaceL = SpliceSpace("l", 1, 'g', Map("a"->4, "b"->6, "c"->2, "d"->2, "e"->2, "f"->6, "g"->1)) // 2
		val spaceNone1 = SpliceSpace("1none", 4, 'j', Map("a"->4, "b"->7, "c"->2, "d"->1, "e"->2, "f"->6, "j"->1)) // None of these!
		// These 12 are interesting:
		// idx 4 and 8 reach compMusic=142
		// idx 5 fails with minPrefixCount=6
		// idx 6 works with minPrefixCount=3!
		val spaceM = SpliceSpace("m", 2, 'j', Map("a"->3, "b"->6, "c"->2, "d"->2, "e"->2, "f"->7, "j"->1)) // 12

		val spaceNone3 = SpliceSpace("3none", 0, 'j', Map("a"->4, "b"->7, "c"->2, "d"->1, "e"->2, "f"->6, "j"->1)) // 0
		// Only n2 is good (what about n4?)
		val spaceN = SpliceSpace("n", 2, 'l', Map("a"->4, "b"->6, "c"->1, "d"->2, "e"->2, "f"->7, "l"->1)) // 4
		// Only 2, 4 and 8 work
		val spaceO = SpliceSpace("o", 2, 'h', Map("a"->4, "b"->7, "c"->1, "d"->2, "e"->2, "f"->6, "h"->1)) // 8

		val spaceP = SpliceSpace("p", 4, 'k', Map("a"->4, "b"->7, "c"->2, "d"->2, "e"->1, "f"->6, "k"->1)) // 8
		val spaceQ = SpliceSpace("q", 7, 'h', Map("a"->5, "b"->5, "c"->2, "d"->2, "e"->2, "f"->6, "h"->1)) // 22
		val spaceR = SpliceSpace("r", 1, 'k', Map("a"->4, "b"->5, "c"->2, "d"->2, "e"->2, "f"->7, "k"->1)) // 2
		val spaceS = SpliceSpace("s", 1, 'g', Map("a"->5, "b"->5, "c"->2, "d"->2, "e"->1, "f"->7, "g"->1)) // 0

		val splices = spaceL.splices(comp)

		// Need to get the sizes of the courses right, so unflattenByCourse works!
		val newComp = new InputComp(comp, comp.courses.zip(splices).map{(p)=> p._1.preferLength(p._2.size)})
		search.buildMethods(newComp, splices)
		//val builder = buildSlices(newComp, splices)
		//val allMethods = builder.outerSections.zip(builder.innerSections).map{(p)=> getAllMethods(p._1, p._2)}
	}

	/** This takes a list of possible splices for each course (so that there are four elements in the top-level list, one
		* per course, each of which contains a list of splices), and turns it into a list of possible splices (so that the
		* top-level list has potentially many elements, each one a splice split into four elements, one per course).
		* It chooses resulting splices whose LHG counts match the given criteria, and excludes all other possibilities. */
	def determineSplicesMeetingCriteria(allSplices: List[List[CourseStructure]], lhgCriteria: Map[String,Int]) =
	{
		assert(lhgCriteria.values.sum==23)
		var found = List[List[List[String]]]()
		def recurse(splicedLeft: List[List[CourseStructure]], revFound: List[List[String]], lhgSoFar: Map[String,Int]): Unit = splicedLeft match
		{
			case Nil =>
				found = revFound.reverse::found
			case Nil::rest => // dead end
			case (cs::remaining)::rest =>
			{
				val splice = cs.lhGroups
				val newLHG = splice.foldRight(lhgSoFar){(lhg,map)=> map+(lhg->(1+map.getOrElse(lhg,0)))}
				if (newLHG.forall{p=> lhgCriteria.getOrElse(p._1, 0) >= p._2})
					recurse(rest, splice::revFound, newLHG )
				recurse(remaining::rest, revFound, lhgSoFar)
			}
		}
		recurse(allSplices, Nil, Map())
		found
	}

	/** Filters the spliced possibilites for each course, choosing only those splices with the maximum number of different
		* LH groups available. */
	def determineMostVariedSuitableSplices(inputCourse: InputCourse): List[List[String]] =
	{
		val suitableStructures = determineSuitableSplices(inputCourse)
		val maxHLsVisited = suitableStructures.map{_.nSeventhsPlaceHLsVisited}.max
		val strucuturesWithBestHLs = suitableStructures.filter{maxHLsVisited==_.nSeventhsPlaceHLsVisited}

		val splices = suitableStructures.map{_.lhGroups}
		val splices2 = strucuturesWithBestHLs.map{_.lhGroups}
		val mostVaried = findMostVaried(splices)
		val mostVaried2 = findMostVaried(splices2)
		mostVaried
	}

	def findMostVaried(splices: List[List[String]]) =
	{
		val maxLHgroups = splices.map{(s)=> s.toSet.size}.max
		val mostVaried = splices.filter{_.toSet.size==maxLHgroups}
		mostVaried
	}

	def sufficientlyVaried(cs: CourseStructure, nDistinctLHG: Int) = cs.lhGroups.toSet.size >= nDistinctLHG
	def noRepeats(cs: CourseStructure) = cs.lhGroups.sliding(2).forall{(xs)=> xs.head!=xs.tail.head}
	def force18lhg(cs: CourseStructure, lhg8: Char) = ("ghjklm".toSet-lhg8).forall{(c)=> !cs.lhGroups.contains(""+c)}
	def goodEnoughG(cs: CourseStructure, lhg8: Char) = force18lhg(cs, lhg8) && sufficientlyVaried(cs, 4)
	def plentyGoodEnoughG(cs: CourseStructure, lhg8: Char) = force18lhg(cs, lhg8) && sufficientlyVaried(cs, 4) && noRepeats(cs) &&
			cs.lhGroups.count(_=="b")<3 && cs.lhGroups.count(_=="f")>=1

	/**
	 * Attempts to restrict the splice options for each course to those likely to work well against Simon Linford's
	 * LHG criteria: 25% b, 25% f, 20% c/d, 20% a/e, one g.
	 * @param inputCourse
	 * @param lhg8 the LHG of the one 8th's place method we expect to have
	 * @return
	 */
	def determineBestBalancedSuitableSplices(inputCourse: InputCourse, lhg8: Char): List[List[String]] =
	{
		def balanceOK(cs: CourseStructure) =
		{
			val counts = "abcdef".map{(c)=> val lhg=""+c; (lhg, cs.lhGroups.count(_==lhg))}.toMap
			val countBF = counts("b")+counts("f")
			countBF>=3 && countBF<=4 &&
			counts("a")==1 && counts("b")>=1 && counts("b")<=3 && counts("c")<=1 &&
			counts("d")<=1 && counts("e")<=2 && counts("f")>=1 && counts("f")<=3
		}
		var suitableStructures = determineSuitableSplices(inputCourse)
		suitableStructures = suitableStructures.filter{force18lhg(_,lhg8)}
		// Ensure Panserbjorn (2.5 e) is present in the small group - can't then filter for balance
		//if (suitableStructures.head.lhGroups.size==5)
		//	suitableStructures = suitableStructures.filter{_.leadMap(5) == "e"}
		//else
			suitableStructures = suitableStructures.filter(balanceOK)
		val balancedSpliced = suitableStructures.map{_.lhGroups}
		val variedSpliced = findMostVaried(balancedSpliced)
		variedSpliced
	}

	/** Ensures that the two potential 5-lead courses are treated properly: one as a 5-lead course, one as a 6-lead course
		* but with an 8th's place method in the mix. */
	private def determineSuitableSplices(inputCourse: InputCourse): List[CourseStructure] =
	{
		val minLeadNums = inputCourse.leadNumsVisited.map{_.toSet}.reduce{_.intersect(_)}
		val unusedLeadNums = (Set(0,1,2,3,4,5,6) -- minLeadNums).toList.sorted
		val nUnused = unusedLeadNums.size
		assert(nUnused==1 || nUnused==2)
		val suitableStructures = if (nUnused==2)
		{
			val unusedLeadNum = unusedLeadNums.head
			// If the two unused leadnums are consecutive, e.g. lead 4 and 5, then there is an opportunity for an 8th's place method.
			// The method must take us from any other leadnum in the course to the first unused leadnum (e.g. 4).
			// The second unused leadnum (e.g. 5) is necessarily not rung to avoid falseness from the 8th's to 2nd's place leadends.
			// The result is a six-lead course with one 8th's place method which undergoes no calls.
			if (unusedLeadNums.tail.head-unusedLeadNum==1)
			{
				val requiredLeadNums = (minLeadNums+unusedLeadNum).toList.sorted
				inputCourse.courseStructures.filter{_.leadsVisited==requiredLeadNums}.map{_.replace2ndsWith8ths(nbells, unusedLeadNum)}
			}
			// If the two unused leadnums are not consecutive, omit them both, leaving us the 5-lead course.
			else
			{
				val requiredLeadNums = minLeadNums.toList.sorted
				inputCourse.courseStructures.filter{_.leadsVisited==requiredLeadNums}
			}
		}
		else
		{
			// The other courses have 6 leads
			inputCourse.courseStructures
		}
		suitableStructures
	}

	def buildSlices(comp: InputComp, splices: List[List[String]]): StalactiteBuilder =
	{
		println(splices.map{_.mkString(" ")}.mkString(", "))
		val outputter = new CompositionOutputter(comp, splices)
		println(outputter.output)

		var t = System.currentTimeMillis()
		println("Building method courses/halfleads...")
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		val methodLeads = splices.zipWithIndex.map{(p)=> buildMethodLeads(p._2, cos(p._2), p._1)}.flatten

		// Build inners first, so we can discard outers whose tail does not join to an inner (or not enough inners)
		println("Building inner sections ...")
		val innerSections = new StalagmiteBuilder(methodLeads).stalagmites
		println("Building outer sections ...")
		val outerSectionBuilder = new StalactiteBuilder(methodLeads, innerSections)
		t = (System.currentTimeMillis()-t)/1000
		println("Table build finished in "+t+"s")
		outerSectionBuilder
	}

	/** Builds method leads for a given (naturally-ordered) splice  */
	def buildMethodLeads(courseSet: Int, cos: List[CoursingOrder], splice: List[String]) =
	{
		/** This is in splice order */
		val leadNums: List[Int] = CourseStructure.lhGroupsToLeadNums(nbells, splice)
		val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		val methodLeads = methodCourses.zip(splice).map{(p)=> MethodLeads(p._1, p._2)}
		methodLeads
	}

	var compRotations: List[Row] = _
	var compMusicForRows: Map[Row,CompRotMusic] = _

	def buildCompMusicForRows(courses: List[MethodLeads]): Unit =
	{
		print("Building comp music...")
		val allRows = RowBitSet.rowsByTreblePosition.values.flatten
		compRotations = courses.map{_.course.courseHead}
		val courseHeadPerms = compRotations.map{_.toPerm.inverse}
		compMusicForRows = allRows.map{(r)=> (r, CompRotMusic(courseHeadPerms.map{CompMusic(_,r)}))}.toMap
		println(" done")
	}

	def clearCompMusic: Unit =
	{
		compMusicForRows = Map()
		print("GC...")
		System.gc()
		println(" done")
	}

	def bestRot(courses: Vector[SearchPosition]) =
	{
		val rotMusics = courses.map{_.headSlice}.flatMap{
			case sf: SliceFinish => Some(sf.totalCompMusic)
			case _ => None
		}
		if (rotMusics.isEmpty)
			(CompMusic(), Row(8))
		else
			rotMusics.reduce(_+_).bestRot
	}

}

