package net.snowtiger.spliced.atw

import net.snowtiger.ringing.NamedMethod


/**
 * A method finder which searches specific splices, and uses MethodNodes and their truthtables to optimise
 * search speed, at the expense of potentially slow table-build times. The MethodNodes may be built with
 * partial methods (e.g. just the first section) in order to reduce the number of nodes which must be searched.
 *
 *
 * @author mark
 */
class AtwMethodFinder2(nbells: Int) extends AtwMethodFinderBase
{
	var bestScore = 0
	var highwater = 0
	var count: Long = 0

	def findMethods(libraryMethods: Map[String, List[NamedMethod]], comp: InputComp)
	{
		val nIterations = comp.courses.head.spliceList.size
		for (i <- 0 until nIterations)
		{
			println()
			val randomSplices = comp.courses.map{_.randomSplice}
			println("Splices = "+randomSplices.mkString(" "))
			findMethods(libraryMethods, comp, randomSplices)
		}
	}

	def findMethods(libraryMethods: Map[String, List[NamedMethod]], comp: InputComp, splices: List[String])
	{
		bestScore = 0
		highwater = 0
		count = 0
		//if (nbells==10)
		//	methodsPerLH = methodsPerLH.mapValues{(xs)=> if (xs.size>50) xs.filter((m)=>AtwMethodFinder2.methodAssessor.isGoodRoyal(m)) else xs}

		println("Building method nodes...")
		val allCourses = comp.getCompCOs.zip(splices).zipWithIndex.flatMap{(p)=> AtwMethodFinder.genMethodCourses(p._2, p._1._2, p._1._1)}
		val allLHGroups = splices.flatMap{getLHGroups}
		//val allMethods = libraryMethods.values.flatten
		val searchMethods = libraryMethods.mapValues{_.map(FullSearchMethod(_))}
		var allNodes = allCourses.zip(allLHGroups).map{ (p)=> makeMethodNodes(p._1, searchMethods(p._2)) }
		if (allNodes.exists(_.isEmpty))
			println("One or more courses have no nodes: "+allNodes.map{_.size}.mkString(", "))
		else
		{
			allNodes = buildTruthTables(allNodes)
			println("Searching... "+allNodes.map{_.size}.mkString(" "))
			for (startMethod <- allNodes.head)
			{
				//println("**** "+startMethod)
				find(List(startMethod), allCourses.tail, startMethod.trueTable.toMap)
			}
		}
	}

	def find(revFound: List[TruthTableMethodNode], todo: List[MethodCourse], remaining: Map[MethodCourse, Set[TruthTableMethodNode]])
	{
		if (revFound.size>highwater)
		{
			highwater = revFound.size
			println("Highwater: "+highwater)
		}
		if (todo.isEmpty)
		{
			count+= 1
			val score = revFound.map{_.score}.sum
			if (score>=bestScore)
			{
				bestScore = score
				val comp = revFound.reverse
				println(score+" "+comp.mkString(", "))
			}
			else if (count%10000000==0)
				println("Count: "+count+" - "+score+" "+revFound.reverse.mkString(", "))
		}
		else
		{
			val nextCourse = todo.head
			for (other <- remaining(nextCourse))
			{
				def prune(course: MethodCourse, nodes: Set[TruthTableMethodNode]): Option[(MethodCourse, Set[TruthTableMethodNode])] =
					if (course==nextCourse) None else Some(course->nodes.intersect(other.trueTable(course)))
				val newRemaining = remaining.flatMap{(p)=> prune(p._1, p._2)}
				if (newRemaining.values.forall{!_.isEmpty})
					find(other::revFound, todo.tail, newRemaining)
			}
		}
	}

	def getLHGroups(splice: String): List[String] =
	{
		val baseCourse = new ProtoSplice(splice, nbells)
		val lhGroups = baseCourse.methods.map{_.lhGroup}
		lhGroups
	}
}