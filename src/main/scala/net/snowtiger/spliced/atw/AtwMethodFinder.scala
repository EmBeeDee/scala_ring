package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{NamedMethod, PN, Perm, Row}
import net.snowtiger.spliced.MethodAssessor
import net.snowtiger.spliced.atw.construct.MethodConstructor
import net.snowtiger.spliced.composition.Stage
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * @author mark
 */
object AtwMethodFinder
{
	val nbells = 8
	val finder = new AtwMethodFinder8(nbells)

	val libraryFile = if (nbells==10) "TDRoyal.csv" else "tdMajor.csv" //"surprise.csv"
	//val libraryFile = "plainMajor.csv"
	val PruneWorstBefore = 0.0	// 0.6
	val KeepBestAfter = 1.0

	/** Returns true if every section of the method produces two each positive and negative rows */
	def evenPlusMinus(method: NamedMethod) =
		method.lead.slice(0,method.lead.size/2).grouped(4).map{_.reverse.tail.reverse}.forall{PN.nNegativeRowsGenerated(_,8)==2}
	def accept(m: NamedMethod) = /*Set("Derwent", "Cray").contains(m.name) || m.lead.startsWith(PN.parse("x56x14",8)) || */m.lead.startsWith(PN.parse("x56x14",8))

	val royalStandardMethods = "London,Cambridge,York,Triton,Sgurr,Horsleydown,Lincoln,Albanian,Anglia,Evesham,Hampstead,Isleworth,King's Norton,Nideggen,Oakham,Pudsey,Prittlewell,Quedgeley,Superlative,Swindon,Vermuyden,Woodspring,Winchester,Zelah".split(",")
	def acceptRoyal(m: NamedMethod) = methodAssessor.isGoodRoyal(m) || royalStandardMethods.exists{(n)=> m.name.startsWith(n)}

	val methodAssessor = MethodAssessor(nbells)
	val rawLibraryMethods = methodAssessor.parseMethods(Source.fromFile(libraryFile)).
			filter{_.leadLength==nbells*4}.filter{_.isSymmetric}//.filter{_.leadheadPN==PN("12")}
	val libraryMethods = if (nbells==10)
		rawLibraryMethods./*filter{!_.lead.slice(0,20).contains("10")}.*/filter(acceptRoyal)
	else
		rawLibraryMethods.filter(methodAssessor.isHalfDecent)//.filter(accept)//.filter(evenPlusMinus)
	val libraryMatchPrefix = 4
	val libraryMethodsFirst2Sections = libraryMethods.map{_.lead.slice(0,libraryMatchPrefix)}.toSet
	val doubleMethods = MethodConstructor.doubleMethods()
	//val searchMethods = doubleMethods.filter{m=> libraryMethodsFirst2Sections.contains(m.lead.slice(0,libraryMatchPrefix))}
	val searchMethods = libraryMethods
	val methodsByName = searchMethods.groupBy(_.name)

	val inputCompReader = new InputCompReader(nbells)

	var rowsAlreadyRung = Set[Row]()

	//val comps = inputCompReader.read("major2s.lst", 5)
	val comps = inputCompReader.read("major4sd.lst", 5)
	//val comps = inputCompReader.read("atw8s4e.out", 5)
	//val comps = inputCompReader.read("royal2s.lst", 7)

	def main(args: Array[String])
	{
		println(searchMethods.size+" methods filtered.")
		val allSplitTenors = genCH7neg

		//val popularMethods = new MethodPopularityReader(libraryMethods)
		//popularMethods.readAll(200)

		var n = 2 //2
		//for (comp <- comps)
		val comp = comps(n)
		if (true)
		{
			//val methodProvider = new PopularMethodProvider(popularMethods, n)
			//val methodProvider = new StandardMethodProvider(searchMethods)
			val methodProvider = new Combined2nds8thsPlaceMethodProvider(searchMethods)

			n+= 1
			println()
			println("************************* COMP "+n+" *****************************")
			println(comp)
			val shortestSpliceComp = comp.preferShortest
			val longestSpliceComp = comp.preferLongest
			val firstShortest = new InputComp(comp, shortestSpliceComp.courses.slice(0,2)++longestSpliceComp.courses.slice(2,4))
			val lastShortest = new InputComp(comp, longestSpliceComp.courses.slice(0,2)++shortestSpliceComp.courses.slice(2,4))
			val searchComp = lastShortest
			//val searchComp = shortestSpliceComp
			finder.findMethods(methodProvider, searchComp)

			/*
			println("Generating rows for previous comp...")
			val avoidRows = allSplitTenors.flatMap{courseFrom(_)}.toSet
			rowsAlreadyRung = avoidRows
			finder.findMethods(methodsByLH, shortestSpliceComp)
			*/
			/*
			val sampleMethods = longestSpliceComp.courses.map{_.spliceList(0).map{(c)=> methodsByLH(""+c.toLower).head.name}.mkString(",")}.mkString("/")
			val lhsToRingList = genComp(longestSpliceComp, "12345678", false, sampleMethods)
			val lhsToRingSet = lhsToRingList.toSet
			for (ch7 <- allSplitTenors)
			{
				val rowsRungList = genComp(shortestSpliceComp, ch7, true, "Northamptonshire,Pontypridd,Regina,Moonshine,Hornsey/Henley,Kingsdown,Chester,Newell,Willesden,Ypres")
				val rowsRungSet = rowsRungList.toSet
				if (MethodNode.quickTruthCheck(lhsToRingSet, rowsRungSet))
				{
					println("True: "+ch7)
					rowsAlreadyRung = rowsRungSet
					finder.findMethods(methodsByLH, longestSpliceComp)
				}
			}
			*/
			/*
			// See what single methods are true to the compositions (whole courses of them)
			var nTrue = 0
			for (methods <- methodsByName.values; method <- methods)
			{

				val rows = genSingleMethodComp(method, shortestSpliceComp.getCompCOs.flatten)
				if (rows.size==6272)
				{
					nTrue+= 1
					println(method.name+"\t"+method.outputPN())
				}
			}
			println(nTrue+" true single methods")
			*/
			/* Attempt to find methods for the first course set, given a set of methods for the last three sets
			//val pnMethods = "-38-14-12 (e), -56-16-12 (b), 38-38.16-56 (e), -56-14.56- (a), -58-16-56 (e), 38.56.38.16-56 (c), 58.34-14-58 (e), -56-14.56- (e), -38-14-58 (d), -58-14.58- (f), -58-14-12 (c), -56-14.56- (e), 56-58.14-58 (a), -56-16-58 (b), -58-14.58- (b), 34.58-14.58- (a), -58-14-1258 (a), -58-14-56 (d), 38-58.14-1258 (d), -58-14-56 (d), 38-38.14-58 (e), -36-14-58 (b), -36-14-12 (b)"
			//val pnsPerCourse = shortestSpliceComp.unflattenByCourse(pnMethods.filter{_!=' '}.split(",").toList)

			val courseSetsToSearch = List(false, false, false, true)
			def toSearch[T](xs: List[T]) = courseSetsToSearch.zip(xs).filter{_._1}.map{_._2}
			def notToSearch[T](xs: List[T]) = courseSetsToSearch.zip(xs).filter{!_._1}.map{_._2}
			val compToFind = InputComp(nbells, comp.score, comp.rotateTo, toSearch(shortestSpliceComp.courses))
			val compRemaining = InputComp(nbells, comp.score, comp.rotateTo, notToSearch(shortestSpliceComp.courses))

			//val rowsRungList = genComp(compRemaining, notToSearch(pnsPerCourse).flatten)
			val rowsRungList = genCompPrefixRows(compRemaining, List(Nil,Nil,Nil))
			rowsAlreadyRung = rowsRungList.toSet
			finder.findMethods(methodsByLH, compToFind)
			*/
		}
	}

	private def genCH7neg = Row.generateAll(5).filter{_.positive}.map{(r)=> padCH7(r.shift(1).toString)}
	private def padCH7(small: String) = "1"+small.substring(0,1)+"7"+small.substring(1,5)+Row.Rounds.substring(7, nbells)
	private def courseFrom(lh: String) = methodsByName("Yorkshire").head.generateFullCourse(Row(lh)).grouped(32).map{_.head}

	def genMethodCourses(setNumber: Int, splice: String, cos: List[CoursingOrder]): List[MethodCourse] =
	{
		val baseCourse = new ProtoSplice(splice, nbells)
		val methodCourses = baseCourse.leadNums.map{(ln)=> MethodCourse(nbells, setNumber, ln, cos)}
		methodCourses
	}

	def genMethodCourses(setNumber: Int, lhGroups: List[String], cos: List[CoursingOrder]): List[MethodCourse] =
	{
		val baseCourse = CourseStructure(nbells, lhGroups)
		val methodCourses = baseCourse.leadNums.map{(ln)=> MethodCourse(nbells, setNumber, ln, cos)}
		methodCourses
	}

	def genSingleMethodComp(method: NamedMethod, cos: List[CoursingOrder]) =
	{
		val buf = ListBuffer[Row]()
		for (co <- cos)
		{
			val ch = co.toCourseHead(nbells)
			buf++= method.generateFullCourse(ch)
		}
		buf.toSet
	}

	/** Uses the given methods to generate all rows of the comp */
	def genComp(comp: InputComp, permTo: String, methodStr: String): List[Row] =
	{
		val methods = methodStr.split('/').toList.map{_.split(',').map(methodsByName(_).head)}
		val flatMethods = methods.flatten
		val cos = comp.getCompCOs
		assert(methods.size==cos.size)
		val splices = methods.map{_.map(_.lhGroup).mkString}
		val methodCourses = splices.zip(cos).zipWithIndex.flatMap{(p)=> genMethodCourses(p._2, p._1._1, p._1._2)}

		val buf = ListBuffer[Row]()
		for ( (method, course) <- flatMethods.zip(methodCourses); lh <- course.leadheads)
			method.generateLead(lh, buf)
		val rows = buf.toList
		val perm = Perm(permTo)
		rows.map{_.permuteBy(perm)}
	}

	def genComp(comp: InputComp, pnMethods: List[String]): List[Row] =
	{
		genCompForPNMethods(comp, pnMethods.map{parsePNMethod})
	}

	def parsePNMethod(raw: String) =
	{
		val i = raw.indexOf('(')
		val j = raw.indexOf(')')
		(raw.substring(i+1,j), PN.parse(raw.substring(0,i), nbells))
	}

	/** Uses the given (place notation, lhgroup) pairs to generate all prefix rows of the comp */
	def genCompForPNMethods(comp: InputComp, pnMethods: List[(String, Seq[PN])]): List[Row] =
	{
		val plainCourseLeads = new Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
		val allLHGroups = pnMethods.map{_._1}.distinct
		val lhPerms = CourseStructure.lhGroupsToLHOrders(nbells, allLHGroups).map{plainCourseLeads(_)}
		val lhPermForGroup = Map[String,Perm]() ++ allLHGroups.zip(lhPerms)
		val pn12 = PN("12")
		val lhGroupsPerCourse = comp.unflattenByCourse(pnMethods.map{_._1}.toList)
		val cos = comp.getCompCOs
		val methodCourses = lhGroupsPerCourse.zip(cos).zipWithIndex.flatMap{(p)=> genMethodCourses(p._2, p._1._1, p._1._2)}
		val buf = ListBuffer[Row]()
		for ( (pnMethod, course) <- pnMethods.zip(methodCourses); lh <- course.leadheads)
		{
			buf+= PN.generateChanges(lh, pnMethod._2, buf)
			val le = lh.apply(lhPermForGroup(pnMethod._1)).apply(pn12)
			buf+= PN.generateChanges(le, pnMethod._2, buf)
		}
		val rows = buf.toList
		rows
	}

	/** Generates the leadheads and leadends of the comp, together with one or more initial PN prefixes.	*/
	def genCompPrefixRows(comp: InputComp, prefixes: List[List[String]]): List[Row] =
	{
		val pn12 = PN("12")
		val cos = comp.getCompCOs
		assert(cos.size==prefixes.size)
		val splices = comp.courses.map{_.randomSplice}
		val methodCourses = splices.zip(cos).zip(prefixes.zipWithIndex).flatMap{
			(p)=> genMethodCourses(p._2._2, p._1._1, p._1._2).map(Tuple2(_, p._2._1))
		}

		val buf = ListBuffer[Row]()
		for ( (course, pnList) <- methodCourses; lh <- course.leadheads)
		{
			// Note this is the leadend of the previous lead, likely to be a different method in the real composition!
			val le = lh.apply(pn12)
			buf+= lh
			buf+= le
			for (pn <- pnList)
			{
				buf++= genExtraRows(lh, pn)
				buf++= genExtraRows(le, pn)
			}
		}
		val rows = buf.toList
		rows
	}

	def genExtraRows(row: Row, pn: String) = PN.generateRows(row, PN.parse(pn))

	trait MethodProvider
	{
		def allMethods: Map[String, List[NamedMethod]]
		def methodsForCourseSet(courseSet: Int): Map[String, List[NamedMethod]]
	}

	class StandardMethodProvider(libraryMethods: List[NamedMethod]) extends MethodProvider
	{
		val methodMap = libraryMethods.groupBy(_.lhGroup)
		override def allMethods = methodMap
		override def methodsForCourseSet(courseSet: Int) = allMethods
	}

	/** Adds the 2nds place method place notations to the equivalent 8ths place method lists. E.g. g includes all b methods. */
	class Combined2nds8thsPlaceMethodProvider(libraryMethods: List[NamedMethod]) extends StandardMethodProvider(libraryMethods)
	{
		def pn(m: NamedMethod) = m.lead.slice(0,m.leadLength-1)
		def combineOneWay(lhg1: String, lhg2: String) =
		{
			val methods1 = methodMap(lhg1)
			val distinctHalfLeads = methods1.map(pn).toSet
			methods1 ++ methodMap(lhg2).filter{(m)=> !distinctHalfLeads.contains(pn(m))}
		}
		def combineTwoWay(lhg1: String, lhg2: String) = List( (lhg1,combineOneWay(lhg1,lhg2)), (lhg2,combineOneWay(lhg2,lhg1)) )
		val combinedMethodMap = MethodConstructor.LHGPairs.flatMap{(p)=> combineTwoWay(p._1,p._2)}.toMap
		override def allMethods = combinedMethodMap
	}

	class PopularMethodProvider(popularity: MethodPopularityReader, comp: Int) extends StandardMethodProvider(popularity.libraryMethods)
	{
		override def methodsForCourseSet(courseSet: Int) = popularity.methodsPerComp(comp)(courseSet).groupBy(_.lhGroup)
	}
}
