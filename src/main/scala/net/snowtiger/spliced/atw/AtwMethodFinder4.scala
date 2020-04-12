package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.util.Random

/**
 * Uses a basic simulated annealing algorithm to search for methods.
 * Allowed to roam over false solutions - energy function = degree of falseness
 * TODO does not respect the splice structure within each course set! I.e. results in unworkable LH groups.
 * TODO have temporarily fixed this with a version that takes the most popular splice in each course set
 *
 * @author mark
 */
class AtwMethodFinder4(nbells: Int) extends AtwMethodFinderBase
{
	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		println("Building splice trees")
		val trees = comp.courses.map{new LHGTreeBuilder(nbells, _)}
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		val searchMethods: Array[Map[String,List[SearchMethod]]] = (0 until comp.courses.size).toArray.map{(n)=> methodProvider.methodsForCourseSet(n).mapValues(makeSearchMethods)}

		println("Building method nodes...")
		//val lhGroups = "f,f,f,f,f,l, f,b,a,a,b,b, f,b,b,b,b, b,d,b,b,b,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,j,d,d,f,b, f,b,a,a,b,b, f,b,b,b,b, b,d,b,b,b,b".split(' ').map{_.split(',').toList}
		val lhGroups = "a,j,d,f,f,a, f,b,a,a,b,b, f,b,b,b,b, b,d,b,b,b,b".split(' ').map{_.split(',').toList}
		val nodeBuilders = trees.zip(lhGroups).zipWithIndex.map{(p)=> new SpliceBuilder(p._2, cos(p._2), searchMethods(p._2), p._1._1, p._1._2)}
		//val nodeBuilders = trees.zipWithIndex.map{(p)=> new NodeBuilder(p._2, cos(p._2), searchMethods(p._2), p._1)}
		var methodNodes = nodeBuilders.flatMap{_.methodNodes}
		//methodNodes = methodNodes.zip(lhGroups).map{(p)=>p._1.filter(_.method.lhGroup==p._2)}

		// May be no methods for a given node if rowsAlreadyFound have been set, and are false with all possibilities.
		if (methodNodes.exists(_.isEmpty))
			println("No methods exist for at least one node - aborting search")
		else
		{
			println("Searching...")
			var t = System.currentTimeMillis()

			//val seedComp = FalseComposition2(methodNodes.map{(xs)=> xs.toArray.apply(Random.nextInt(xs.size))}.toList)
			//val cray = FullSearchMethod(NamedMethod("Cray", 8, "x56x14x56x36x34x58x34x18", "12"))
			//val seedComp = FalseComposition(nodeAlts.map{(xs)=> new MethodNode(xs.head.course, cray)}.toList)
			//val seed = "Glazebury,Ilfracombe,Southleigh,Superlative,Caversham,Zverinogolovskoye,St John's,Chiswick,Jeffry's,Humberside,Hintlesham,Vigo Village,Boveney,Qatar,Notting Hill,Woolacombe,Digbeth,Ealing,Newbury,Aipe,Piglet,Wellington,Andover"
			//val seedComp = getSeed(seed, allMethods)

			// Add in null methods, containing just the leadheads and leadends - for PartialComposition search only!
			val nullMethods = methodNodes.map{(set)=> makeNullMethod(methodProvider, set.head)}
			methodNodes = methodNodes.zip(nullMethods).map{(p)=> p._1+p._2}
			val seedComp = new PartialComposition(nullMethods)

			find(methodNodes, seedComp)
			t = (System.currentTimeMillis()-t)/1000
			println("Search finished in "+t+"s")
		}
	}

	//def makeSearchMethods(methods: List[NamedMethod]) = methods.map{FullSearchMethod(_)}
	def makeSearchMethods(methods: List[NamedMethod]) = methods.map{(m)=> FirstSectionSearchMethod(11, List(m))}

	def makeNullMethod(methodProvider: MethodProvider, node: TruthTableMethodNode) =
	{
		val nm = methodProvider.allMethods(node.method.lhGroup).head
		val sm = FirstSectionSearchMethod(0, List(nm))
		val nullNode = new MethodNodeWithTruth(node.course, sm)
		nullNode
	}

	class NodeBuilder(courseSet: Int, cos: List[CoursingOrder], searchMethods: Map[String,List[SearchMethod]], treeBuilder: LHGTreeBuilder)
	{
		// Currently just using the first leadNumSet for each course.
		// If there are more than one, could iterate over all with multiple CourseSetTreeSearches per course set
		// Here picking the last (2nd) set for COMP 3, which allows a method with no calls which can be replaced by an 8th's place method.
		private val leadNums: List[Int] = treeBuilder.leadNumSets.last
		private val lhgTree = treeBuilder.lhgTrees.last
		private val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		// For each leadNum, a list of all possible method nodes (only those with LH groups possible for the leadNum)
		private def makeNodes(lhgs: Set[String], mc: MethodCourse): Set[TruthTableMethodNode] =
			lhgs.flatMap{(lhg)=> makeMethodNodes(mc, searchMethods.getOrElse(lhg,Nil))}
		// Picks the most popular splice, by number of methods for each LH group
		private var revNodes = List[Set[TruthTableMethodNode]]()
		private var currTree = lhgTree
		for (mc <- methodCourses)
		{
			val popularLHG = currTree.lhGroups.keys.map{(lhg) => (lhg,searchMethods.getOrElse(lhg,Set()).size)}.toList.sortBy((p)=> -p._2)
			val mostPopularLHG = currTree.lhGroups.keySet.maxBy{searchMethods.getOrElse(_,Set()).size}
			revNodes = makeNodes(Set(mostPopularLHG), mc)::revNodes
			currTree = currTree.lhGroups(mostPopularLHG)
		}
		val methodNodes = revNodes.reverse
	}

	/** Builds the method nodes for a given splice (note this must be in leadnum order not the splice order!!) */
	class SpliceBuilder(courseSet: Int, cos: List[CoursingOrder], searchMethods: Map[String,List[SearchMethod]], treeBuilder: LHGTreeBuilder, splice: List[String])
	{
		private val leadNums: List[Int] = treeBuilder.leadNumSets.last
		private val methodCourses: List[MethodCourse] = leadNums.map{MethodCourse(nbells, courseSet, _, cos )}
		private def makeNodes(lhgs: Set[String], mc: MethodCourse): Set[TruthTableMethodNode] =
			lhgs.flatMap{(lhg)=> makeMethodNodes(mc, searchMethods.getOrElse(lhg,Nil))}
		private var revNodes = List[Set[TruthTableMethodNode]]()
		for ( (mc,lhg) <- methodCourses.zip(splice))
			revNodes = makeNodes(Set(lhg), mc)::revNodes
		val methodNodes = revNodes.reverse
	}

	def getSeed(methodStr: String, allMethods: List[Set[TruthTableMethodNode]]) =
	{
		def findMethod(name: String, nodes: Set[TruthTableMethodNode]) = nodes.find{_.method.name==name} match
		{
			case Some(method) => method
			case None => throw new AssertionError("Seed method "+name+" not found in node list")
		}
		val methods = methodStr.split(',')
		FalseComposition(methods.zip(allMethods).map{(p)=> findMethod(p._1, p._2)}.toList)
	}

	def find(allMethods: List[Set[TruthTableMethodNode]], seedComp: Composition)
	{
		val nodeAlts = allMethods.map{_.toArray}.toArray
		//tunnel(seedComp, 2.0, 1.8, 100000, nodeAlts)
		anneal(seedComp, 2000000, nodeAlts)
	}

	def tunnel(comp: Composition, baseK: Double, kMult: Double, maxK: Int, nodeAlts: Array[Array[TruthTableMethodNode]]) =
	{
		var thisK = baseK
		var current = comp
		while (thisK<maxK)
		{
			val intK = thisK.toInt
			val newComp = anneal(current, intK, nodeAlts)
			if (newComp.energy < current.energy)
			{
				println("*** Seeding from new best: "+newComp)
				current = newComp
				thisK = baseK
			}
			else
			{
				thisK*= kMult
			}
		}
	}

	def anneal(comp: Composition, kMax: Int, nodeAlts: Array[Array[TruthTableMethodNode]]) =
	{
		//def temperature(k: Int) = 4*(kMax - k).toDouble/kMax
		def temperature(k: Int) = 40*(kMax - k).toDouble/kMax

		var k = 0
		var state = comp
		var e = state.energy
		var best = comp
		var bestE = e
		println("Seeding from: "+state)
		var time = System.currentTimeMillis()
		var nMoves:Long = 0
		while (k < kMax)
		{
			val t = temperature(k)
			val newState = state.randomMove(nodeAlts)
			val newE = newState.energy
			if (acceptMove(e, newE, t))
			{
				nMoves+= 1
				state = newState
				e = newE
				if (k>0 && k%10000==0)
				{
					val newT = System.currentTimeMillis()
					val duration = newT-time
					time = newT
					val perfString = " ["+(nMoves*1000/duration)+" moves/s]"
					println("New state at k=" + k + ": " + state + perfString)
				}
				if (newE < bestE)
				{
					println("New best: "+state)
					best = newState
					bestE = newE
				}
			}
			k+= 1
		}
		best
	}

	def acceptMove(e: Int, eNew: Int, t: Double) =
	{
		if (eNew<e)
			true
		else
		{
			val p = math.exp( (e-eNew).toDouble/t )
			p > Random.nextDouble()
		}
	}

	trait Composition
	{
		def energy: Int
		def randomMove(nodeAlts: Array[Array[TruthTableMethodNode]]): Composition
	}

	case class FalseComposition(courses: List[TruthTableMethodNode]) extends Composition
	{
		/** Lower is better, < 0 is true! */
		private lazy val nMethods = courses.map{_.method.name}.toSet.size
		private lazy val nFalse = 5152 - courses.foldLeft(Set[Row]()){(rows,method)=> rows++method.rows}.size
		override lazy val energy = nFalse - nMethods*2

		override def randomMove(nodeAlts: Array[Array[TruthTableMethodNode]]) =
		{
			val i = Random.nextInt(courses.size)
			val j = Random.nextInt(nodeAlts(i).size)
			FalseComposition(courses.take(i)++(nodeAlts(i)(j)::courses.drop(i+1)))
		}

		override def toString = nMethods+" "+nFalse+" "+courses.mkString(", ")
	}

	val sectionsTreblePos = Array(Set(1,2), Set(3,4), Set(5,6,7,8))//(1 to nbells).toList.grouped(2).map{_.toSet}.toArray
	def partitionBySection(rows: Set[Row]) = sectionsTreblePos.map{(tp)=> rows.filter{(r)=> tp.contains(r.placeOf(1))}}

	case class FalseComposition2(courses: List[TruthTableMethodNode]) extends Composition
	{
		def findDupRows() =
		{
			var allRows = Set[Row]()
			var dupRows = Set[Row]()
			for (node <- courses; rows = node.rows)
			{
				dupRows++= allRows.intersect(rows)
				allRows++= rows
			}
			dupRows
		}
		private lazy val dupRowsPerSection = partitionBySection(findDupRows())
		private lazy val falseCountsPerSection = dupRowsPerSection.map{(dr)=> courses.count(!_.rows.intersect(dr).isEmpty)}
		private lazy val nMethods = courses.map{_.method.name}.toSet.size
		// First section is scored most highly, then second etc; higher score actually means higher energy = WORSE
		def score() =
		{
			var score = 0
			for (falseCount <- falseCountsPerSection)
				score = score*5 + falseCount
			score
		}
		override lazy val energy = score - nMethods*2000000

		override def randomMove(nodeAlts: Array[Array[TruthTableMethodNode]]) =
		{
			val i = Random.nextInt(courses.size)
			val j = Random.nextInt(nodeAlts(i).size)
			FalseComposition2(courses.take(i)++(nodeAlts(i)(j)::courses.drop(i+1)))
		}

		override def toString = nMethods+" "+falseCountsPerSection.mkString("/")+" "+courses.mkString(", ")
	}

	case class PartialComposition(courses: List[TruthTableMethodNode], rows: Set[Row]) extends Composition
	{
		def this(courses: List[TruthTableMethodNode]) = this(courses, courses.foldLeft(Set[Row]()){(rows,method)=> rows++method.rows})
		private lazy val nMethods = courses.map{_.method.name}.toSet.size
		private lazy val nRowsLeft = 5152 - rows.size
		override lazy val energy = nRowsLeft - nMethods*2

		override def randomMove(nodeAlts: Array[Array[TruthTableMethodNode]]) =
		{
			val i = Random.nextInt(courses.size)
			var n = nodeAlts(i).size
			var foundMove = false
			var nTries = 1+n/10
			var comp = this
			var reducedRows = rows--courses(i).rows
			while (nTries>=0 && !foundMove)
			{
				val j = Random.nextInt(nodeAlts(i).size)
				val nodeAlt = nodeAlts(i)(j)
				val altRows = nodeAlt.rows
				val newRows = reducedRows ++ altRows
				if (newRows.size==reducedRows.size+altRows.size)
				{
					comp = PartialComposition(courses.take(i)++(nodeAlt::courses.drop(i+1)), newRows)
					foundMove = true
				}
			}
			comp
		}

		override def toString = nMethods+" "+courses.mkString(", ")
	}

}