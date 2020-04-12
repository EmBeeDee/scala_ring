package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row

/** Encapsulates the structure of the given splice course with a sorted list of lead numbers visited - e.g. 0 2 4 5 6;
	* also provides the course as a map of (lead num -> LH group).
	* Effectively this allows us to convert from the natural splice order, e.g. ffb visiting leads 0 6 5, to the
	* sorted leadnum order, in this example 0 5 6, giving fbf if we look up in the leadMap. */
case class CourseStructure(leadNums: List[Int], lhGroups: List[String])
{
	val leadsVisited = leadNums.sorted
	val leadMap = leadNums.zip(lhGroups).toMap
	/** Go back the other way, from the leadnum order to the natural splice order; assumes lead 0 is rung! */
	def splice(nbells: Int): List[String] = {
		val n = nbells-1
		var i = 0
		leadNums.map{ (x)=> val lhg = leadMap(i); i = (i+CourseStructure.LHOrders(lhg)+n)%n; lhg }
	}

	/** Returns the course structure with one method substituted for its 8th's place version; this is the method
		* which brings up the given "finishLeadNum". */
	def replace2ndsWith8ths(nbells: Int, finishLeadNum: Int): CourseStructure =
	{
		val n = nbells - 1
		def replace(i: Int) =
		{
			var lhg = leadMap(i)
			val j = (i+CourseStructure.LHOrders(lhg)+n)%n
			if (j==finishLeadNum)
				lhg = CourseStructure.LHOrders.find{(p)=> p._1!=lhg && p._2==CourseStructure.LHOrders(lhg)}.get._1
			lhg
		}
		CourseStructure(leadNums, leadNums.map(replace))
	}

	/** Only for Major! See {@link CourseStructure#SeventhsPlaceHLOffset}. */
	def seventhsPlaceHLsVisited = leadMap.map{(p)=> (p._1+CourseStructure.SeventhsPlaceHLOffset(p._2))%7}.toSet

	lazy val nSeventhsPlaceHLsVisited = seventhsPlaceHLsVisited.size
}

object CourseStructure
{
	def apply(nbells:Int, splice: String): CourseStructure =
	{
		val course = new ProtoSplice(splice, nbells)
		CourseStructure(course.leadNums, course.lhGroups)
	}

	def apply(nbells: Int, lhGroups: List[String]): CourseStructure =
	{
		val lhOrders = lhGroupsToLHOrders(nbells, lhGroups)
		val leadNums = lhOrdersToLeadNums(nbells, lhOrders)
		CourseStructure(leadNums, lhGroups)
	}

	val LHOrders = Map("a"->1, "b"->2, "c"->3, "c1"->4, "c2"->5, "d2"-> -5, "d1"-> -4, "d"-> -3, "e"-> -2, "f"-> -1,
										 "g"->1, "h"->2, "j"->3, "j1"->4, "j2"->5, "k2"-> -5, "k1"-> -4, "k"-> -3, "l"-> -2, "m"-> -1)

	/** This one only for Major at the moment. For each LH group, it gives the "offset" to the 7th's place PB halflead
		* for the group. E.g. for group a, leadhead 0 (12345678) needs a 23456781/32547681 halflead, equivalent (when
		* reversed) to leadhead 4, 18674523, so its offset is 4 - 0 = 4. */
	val SeventhsPlaceHLOffset = Map("a"->4, "b"->1, "c"->5, "d"->2, "e"->6, "f"->3,
																	"g"->1, "h"->5, "j"->2, "k"->6, "l"->3, "m"->0)

	// test
	assert(CourseStructure(List(0,1,6,3,5), List("a","e","d","b","b")).splice(8)==List("a","e","d","b","b"))

	def lhGroupToLHOrder(nbells: Int, lhg: String): Int =
	{
		val n = nbells-1
		(LHOrders(lhg)+n)%n
	}

	def lhGroupsToLHOrders(nbells: Int, lhGroups: List[String]): List[Int] =
	{
		val n = nbells-1
		lhGroups.map{(lhg)=> (LHOrders(lhg)+n)%n}
	}

	def lhOrdersToLeadNums(nbells: Int, lhOrders: List[Int]) =
	{
		val n = nbells-1
		var lead = 0
		lhOrders.map{ (i)=> val lastLead = lead; lead = (lead+i)%n; lastLead }
	}

	/** This is just the two methods above applied one after the other, but re-implemented for efficiency */
	def lhGroupsToLeadNums(nbells: Int, lhGroups: List[String]): List[Int] =
	{
		val n = nbells-1
		var lead = 0
		lhGroups.map{ (lhg)=> val lastLead = lead; lead = (lead+LHOrders(lhg)+n)%n; lastLead }
	}
}

/** A sequence of proto-methods generating a whole splice course */
case class ProtoSplice(methods: List[ProtoMethod])
{
	def this(splice: String, nbells: Int) = this(splice.toList.map{(m)=> ProtoMethod(m, nbells)})

	val nbells = methods.head.nbells
	val bucket = methods.map{_.abbrev}.sorted.mkString
	val leadheads = genLHsFrom(Row(nbells))
	val methodString = methods.mkString

	lazy val lhOrders = methods.map{_.lhOrder}
	lazy val lhGroups = methods.map{_.lhGroup}

	lazy val leadNums = CourseStructure.lhOrdersToLeadNums(nbells, lhOrders)

	def hasCallingPositions(calls: List[Set[Int]]): Boolean =
		calls.zipWithIndex.forall{(p)=> hasCallingPositions(p._1, p._2)}

	/** Checks from the rotation of the course starting at the given leadnum */
	def hasCallingPositions(calls: Set[Int], leadNumStart: Int): Boolean =
	{
		val n = nbells-1
		// Convert -1 Middles to the nbells-2 form
		val positiveCalls = calls.map{ (c)=> (c+n)%n}
		callDistribution(leadNumStart).intersect(positiveCalls) == positiveCalls
	}

	def genLHsFrom(row: Row) =
	{
		var revResults = List(row)
		for (m <- methods)
			revResults = m.applyTo(revResults.head)::revResults
		revResults.reverse
	}

	/** List of pairs (method, row) */
	def genMethodLeadsFrom(row: Row) = methods.zip(genLHsFrom(row))

	/** Change the abbreviation of an individual method */
	def changeAbbreviation(index: Int, newAbbrev: Char) =
	{
		val array = methods.toArray
		array(index) = ProtoMethod(newAbbrev, array(index).perm)
		ProtoSplice(array.toList)
	}

	def rotateOne = ProtoSplice( (methods.head::methods.tail.reverse).reverse )

	def rotate(i: Int) = ProtoSplice( methods.drop(i)++methods.take(i) )

	def allRots = for (i <- 0 until methods.size) yield rotate(i)

	/** Rotate so that the splice is alphabetically lowest */
	def canonicalForm = allRots.sortBy(_.methodString).head

	/** Where a course has LH groups used more than once, attempt to poke in different abbreviations for each method
		* sharing the same LH group. E.g. AABCCDE would go to aABcCDE (without loss of generality).
		*/
	def splitSameLHGroupMethods: Option[ProtoSplice] =
	{
		// Map of each ProtoMethod -> List of indexes where it occurs in the course
		val occurrences = methods.zipWithIndex.groupBy{_._1}.mapValues{_.map{_._2}}

		def replaceSameLH(m: ProtoMethod, maybeCourse: Option[ProtoSplice]): Option[ProtoSplice] =
		{
			if (maybeCourse.isEmpty)
				None
			else
			{
				val course = maybeCourse.get
				occurrences(m) match
				{
					case Nil => None
					case List(i) => Some(course)
					case List(i,j) => Some(course.changeAbbreviation(i, course.methods(i).bumpAbbrev))
					//case List(i,j,k) => Some(course.changeAbbreviation(i, course.methods(i).bumpAbbrev).changeAbbreviation(k, course.methods(j).bumpAbbrev2))
					case _ => None
				}
			}
		}

		var course: Option[ProtoSplice] = Some(this)
		for (m <- occurrences.keys)
			course = replaceSameLH(m, course)
		course
	}

	/** The lead numbers for each course considered from tenor lead positions 0 ... nbells-1 */
	lazy val callDistribution =
	{
		var callsPerCourse = List[Set[Int]]()
		def plusOne(i: Int) = (i+1)%(nbells-1)
		def plusFour(i: Int) = (i+4)%(nbells-1)
		var course = leadNums
		for (i <- 0 until nbells)
		{
			val calls = course.filter{ plusOne(_)<=2 }.toSet
			callsPerCourse = calls::callsPerCourse
			course = course.map{ plusOne(_) }
		}
		callsPerCourse.reverse
	}

	override def toString = methodString
}

