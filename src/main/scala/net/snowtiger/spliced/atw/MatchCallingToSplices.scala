package net.snowtiger.spliced.atw

import java.io.{FileWriter, PrintWriter}

import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.io.Source

/**
 * @author mark
 */
object MatchCallingToSplices
{
	val outputAllMatches = true
	var out: PrintWriter = _

	def main(args: Array[String])
	{
		//major("atwGroupsExtra1.txt", 5, 6)
		//major("atw8s4e.txt", 5, 6)
		//major("atw8s2b.txt", 5, 6)
		//royal("atw10s.txt")
		maximus("atwGroupsMaximus.txt")
	}

	def major(file: String, bucketNums: Int*)
	{
		val nbells = 8
		val finder = new SpliceFinder(nbells)
		val buckets = bucketNums.map{(n)=> finder.findCourseBuckets(n)}.reduceLeft{(a,b)=> a++b}
		val allSplices = buckets.values.flatten.flatMap(_.allRots.toSet).toList
		processFile(file, allSplices)
		/*
		val coSets = "   0. (53246, 64523, 56423, 23564, 56234, 45623, 42563), 0. (25463, 36245, 23645, 45236, 23456, 62345, 64235), 51. (34562, 24365, 32465, 43265, 54263, 34256, 34625), 4. (53462, 26543, 53624, 25634, 35264, 45362, 62534), 39. (52364, 46253, 54326, 52436, 65243, 64352, 52643)"
		val callings = "   H M W M W H W W W H M M W M W W M M M W H H W W W H M M W W M H W W M M W W H H W W W H M W W W M H H\n   H M W M W H W W W H M M W M H H W W W H W W M M M W M M W W M H W W M M W W H H W W W H M W W W M H H\n   H M H H W W W H W M W H W W W H M M W M W W M M M W H H W W W H M M W W M H W W M M W W M W W W M H H\n   H M H H W W W H W M W H W W W H M M W M H H W W W H W W M M M W M M W W M H W W M M W W M W W W M H H"

		for (calling <- callings.split('\n'))
			doMatch(allSplices, calling, coSets)
		*/
	}

	def royal(file: String)
	{
		val nbells = 10
		val finder = new SpliceFinder(nbells)
		val splices = finder.findCourseBuckets(7)// ++ finder.findCourseBuckets(8)
		val allSplices = splices.values.flatten.flatMap(_.allRots.toSet).toList
		processFile(file, allSplices)
	}

	def maximus(file: String)
	{
		val nbells = 12
		val finder = new SpliceFinder(nbells)
		val splices = finder.findCourseBuckets(10)
		val allSplices = splices.values.flatten.flatMap(_.allRots.toSet).toList
		processFile(file, allSplices)
	}

	def output() { output("") }

	def output(s: String)
	{
		println(s)
		out.println(s)
	}

	def processFile(file: String, splices: List[ProtoSplice])
	{
		out = new PrintWriter(new FileWriter(file.substring(0, file.indexOf("."))+".out"))

		def getCompLine(it: Iterator[String]) =
		{
			if (it.hasNext)
			{
				val line = it.next()
				if (line.startsWith("   "))
					Some(line)
				else
					None
			}
			else
				None
		}

		val it = Source.fromFile(file).getLines()
		//it.next()
		var bestOfAllMatches = 0
		while (it.hasNext)
		{
			val coSets = parseCOString(it.next())
			var comps = List[String]()
			var line = getCompLine(it)
			while (line.isDefined)
			{
				comps = line.get::comps
				line = getCompLine(it)
			}
			if (!comps.isEmpty)
			{
				var bestMatch = 0
				for (comp <- comps)
				{
					val nMatched = doMatch(splices, comp, coSets)
					bestMatch = if (nMatched>bestMatch) nMatched else bestMatch
				}
				if (bestMatch==coSets.size)
				{
					output("Matched: "+bestMatch)
					output()
				}
				bestOfAllMatches = if (bestMatch>bestOfAllMatches) bestMatch else bestOfAllMatches
			}
		}
		output("Best match count: "+bestOfAllMatches)
		out.close()
	}

	def doMatch(splices: List[ProtoSplice], calling: String, parsedCoSets: List[List[CoursingOrder]]): Int =
	{
		val parsedComp = parseCalling(calling)
		val infos = doMatch(splices, parsedComp, parsedCoSets)
		val nMatched = infos.count{_.availableSplices.size>0}
		if (nMatched==parsedCoSets.size || outputAllMatches)
		{
			val (score, bestStartingCO) = GoodCOChecker.checkAllRots(infos.flatMap{_.coSet})
			val rotatedComps = rotateComp(parsedComp, bestStartingCO).map{_.mkString(" ")}
			output("Unrotated calling = "+parsedComp.mkString(" "))
			output("Score: "+score+" (from "+bestStartingCO+") rotated callings "+rotatedComps.mkString(" / "))
			for (info <- infos)
				output(info.toString)
		}
	  nMatched
	}

	def parseCalling(calling: String) = calling.split(' ').filter(_!="").toList

	def parseCOString(cos: String) =
	{
		def parse(s: String) =
		{
			val stripBrackets = s.substring(s.indexOf('(')+1, s.indexOf(')'))
			val split = stripBrackets.filter{_!=' '}.split(',')
			split.map{CoursingOrder(_)}.toList
		}
		val split = cos.split("\\.").tail
		split.map{parse(_)}.toList
	}

	def doMatch(splices: List[ProtoSplice], comp: List[String], coSets: List[List[CoursingOrder]]): List[CourseInfo] =
	{
		val compCalls = callsInComp(comp)
		doMatch(splices, compCalls, coSets)
	}

	def doMatch(splices: List[ProtoSplice], compCalls: Map[CoursingOrder, Set[Int]], coSets: List[List[CoursingOrder]]): List[CourseInfo] =
	{
		def availableSplices(coSet: List[CoursingOrder]) =
		{
			val callsForCOset = coSet.map{compCalls(_)}
			val splicesByLeadNumStart = splices.filter{_.hasCallingPositions(callsForCOset)}.toSet
			splicesByLeadNumStart
		}

		var revInfos = List[CourseInfo]()
		for (coSet <- coSets)
		{
			val info = CourseInfo(coSet, coSet.map{compCalls(_)}, availableSplices(coSet).toList)
			revInfos = info::revInfos
		}
		revInfos.reverse
	}

	/** Returns the calls affecting each course, as a map of coursing order -> set of Ints, 0=H, 1=W, -1=M */
	def callsInComp(composition: List[String]) =
	{
		var callMap = Map[CoursingOrder, Set[Int]]()
		var co = CoursingOrder.Start
		for (call <- composition)
		{
			val callPos = callPositions(call)
			callMap+= co->(callMap.getOrElse(co, Set())+callPos)
			co = co.call(call)
		}
		callMap
	}

	/** Starts the comp from the given coursing order - multiple results if the CO occurs between more than one calling position! */
	def rotateComp(composition: List[String], startCO: CoursingOrder): List[List[String]] =
	{
		var co = CoursingOrder.Start
		var revCompCos = List[CoursingOrder]()
		for (call <- composition)
		{
			revCompCos = co::revCompCos
			co = co.call(call)
		}
		val startOccurrences = revCompCos.reverse.zipWithIndex.filter{_._1==startCO}.map{_._2}
		var rotatedComps = List[List[String]]()
		for (i <- startOccurrences)
			rotatedComps = (composition.drop(i)++composition.take(i))::rotatedComps
		rotatedComps
	}

	val callNames = Map(0->"H", 1->"W", -1->"M")
	val callPositions = callNames.map{_.swap} + ("sH"->0) + ("sW"->1) + ("sM"-> -1)

	case class CourseInfo(coSet: List[CoursingOrder], calls: List[Set[Int]], availableSplices: List[ProtoSplice])
	{
		val callString = calls.map{_.map{callNames(_)}.toList.sorted.mkString.padTo(4, ' ')}.mkString
		/** Sort the splices by alphabetical order, but with shorter ones grouped first */
		def comp(c1: ProtoSplice, c2:ProtoSplice) =
		{
			val byLength = c1.methods.size.compareTo(c2.methods.size)
			if (byLength==0)
				c1.methodString.compareTo(c2.methodString)<0
			else
				byLength<0
		}
		val sortedSplices = availableSplices.sortWith(comp)
		override def toString = callString+" InputCourse(\""+coSet.mkString(",")+"\", \""+sortedSplices.mkString(",")+"\"),"
	}

}