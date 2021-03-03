package net.snowtiger.spliced.atw

import net.snowtiger.spliced.atw.AtwMethodFinder.MethodProvider
import net.snowtiger.spliced.atw.construct._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.util.Random

/**
 * @author mark
 */

/**
 * Uses a simulated annealing algorithm to search for methods, based on a fixed splice (which must be given in leadset order!)
 * Differs from {@link AtwMethodFinder4} in its use of {@link RowBitSet}s to optimise search speed.
 *
 * @author mark
 */
class AtwMethodFinder5(nbells: Int) extends AtwMethodFinderBase2[MethodNodeBitSet](nbells)
{
	assert(nbells==8)

	var completer: CompositionCompleter = _

	def findMethods(methodProvider: MethodProvider, comp: InputComp)
	{
		val searchMethods: Array[Map[String,List[SearchMethod]]] =
			(0 until comp.courses.size).toArray.map{(n)=> methodProvider.methodsForCourseSet(n).mapValues(makeSearchMethods).toMap}

		//val lhGroups = "b,f,b,b,a,a, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,b,b,b, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,b,b,b, a,b,b,f,b,a, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}

		//val lhGroups = "b,d,l,d,b,d, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "f,l,f,f,f,f, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		//val lhGroups = "b,d,b,j,f,d, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}
		val lhGroups = "a,j,f,f,d,a, f,b,b,a,a,b, f,b,b,b,b, b,b,b,b,d,b".split(' ').map{_.split(',').toList}

		//val lhGroups = findMostPopularSplice(comp, searchMethods)

		val outputter = new CompositionOutputter(comp, lhGroups.toList)
		println(outputter.output)

		println("Building method nodes...")
		val cos: List[List[CoursingOrder]] = comp.getCompCOs
		var methodNodes = lhGroups.zipWithIndex.map{(p)=> buildNodes(p._2, cos(p._2), searchMethods(p._2), p._1)}.toList.flatten

		// May be no methods for a given node if rowsAlreadyFound have been set, and are false with all possibilities.
		if (methodNodes.exists(_.isEmpty))
			println("No methods exist for at least one node - aborting search")
		else
		{
			val possibleHalfLeads = methodNodes.map{(xs)=> MethodLeads(xs.head.course, xs.head.method.lhGroup).halfleads}
			val methodCourses = lhGroups.zipWithIndex.map{(p)=> buildMethodCourses(p._2, cos(p._2), p._1)}.toList.flatten
			val lastSections = new LastSectionBuilder(methodCourses, possibleHalfLeads).sections

			completer = new CompositionCompleter(possibleHalfLeads)

			//assessNodes(methodNodes.toArray)
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

	def makeNullMethod(methodProvider: MethodProvider, node: MethodNodeBitSet) =
	{
		val nm = methodProvider.allMethods(node.method.lhGroup).head
		val sm = FirstSectionSearchMethod(0, List(nm))
		val nullNode = new MethodNodeBitSet(node.course, sm)
		nullNode
	}

	override protected def makeNode(mc: MethodCourse, sm: SearchMethod) =
	{
		val mn = MethodNodeBitSet(mc, sm)
		if (mn.isTrue) Some(mn) else None
	}

	def find(allMethods: List[Set[MethodNodeBitSet]], seedComp: AtwComposition)
	{
		val nodeAlts = allMethods.map{_.toArray}.toArray
		//tunnel(seedComp, 2.0, 1.8, 100000, nodeAlts)
		val best = anneal(seedComp, 2000000, nodeAlts)
		println("Best = "+best)
	}

	def anneal(comp: AtwComposition, kMax: Int, nodeAlts: Array[Array[MethodNodeBitSet]]) =
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

	def assessNodes(allMethods: Array[Set[MethodNodeBitSet]])
	{
		def assessNode(method: MethodNodeBitSet, courseSet: Int): Int =
		{
			var nFalse = 0
			for (i <- 0 until allMethods.size)
				if (i!=courseSet)
					for (other <- allMethods(i))
						if (!method.bitset.trueWith(other.bitset))
							nFalse+= 1
			nFalse
		}
		def assessCourseSet(courseSet: Int) =
			allMethods(courseSet).map{(m)=> (m,assessNode(m, courseSet))}.toList.sortBy{_._2}
		val assessed = (0 until allMethods.size).toList.map{assessCourseSet}
		println("Best = "+assessed.map{_.head}.mkString(","))
	}

	val CompleteBelow = 900

	case class PartialComposition(courses: List[PartialMethodNodeBitSet], rows: MultiBitSet) extends CourseAtwComposition
	{
		def this(courses: List[MethodNodeBitSet]) = this(courses.map{PartialMethodNodeBitSet(_,4)}, MultiBitSet.addAll(courses.map{_.bitset}))
		private lazy val nMethods = courses.map{_.node.method.name}.toSet.size
		private lazy val nSections = courses.map{_.effectiveSections}
		def countSections(at: Int) = nSections.count{_>=at}
		//private lazy val score = 10*countSections(1) + 1*countSections(2) + countSections(3)
		private lazy val score = if (nRowsLeft<CompleteBelow) completion else nRowsLeft
		override lazy val energy = score - branchiness*100 - nMethods*30
		private lazy val nRowsLeft = 5152 - rows.size
		//override lazy val energy = nRowsLeft - nMethods*2
		private lazy val branchiness = completer.findBranchiness(this)
		private lazy val completion = if (nRowsLeft<CompleteBelow) completer.complete(this) else 0

		override def randomMove(nodeAlts: Array[Array[MethodNodeBitSet]]) =
		{
			val i = Random.nextInt(courses.size)
			var n = nodeAlts(i).size
			var foundMove = false
			var nTries = 1+n/10
			var comp = this
			var reducedRows = rows-courses(i).bitset
			while (nTries>=0 && !foundMove)
			{
				val j = if (nTries==0) 0 else Random.nextInt(nodeAlts(i).size)
				val nodeAlt = nodeAlts(i)(j)
				val trueSections = nodeAlt.trueUpTo(reducedRows)
				if (trueSections>0)
				{
					val newNode = PartialMethodNodeBitSet(nodeAlt, trueSections)
					val newSet = reducedRows+newNode.bitset
					val newCourses = courses.take(i) ++ (newNode::courses.drop(i+1))
					comp = PartialComposition(newCourses, newSet).maximise
					foundMove = true
				}
				else
				{
					nTries-= 1
				}
			}
			comp
		}

		private def maximise =
		{
			var newRows = rows
			val newCourses = courses.map{(node)=>
				val (maximisedNode, maximisedRows) = node.maximize(newRows)
				newRows = maximisedRows
				maximisedNode
			}
			PartialComposition(newCourses, newRows)
		}

		override def toString = score+"/"+nMethods+"/"+nRowsLeft+"/"+branchiness+" "+courses.mkString(", ")
	}

}

trait AtwComposition
{
	def energy: Int
	def randomMove(nodeAlts: Array[Array[MethodNodeBitSet]]): AtwComposition
}

abstract class CourseAtwComposition extends AtwComposition
{
	def courses: List[PartialMethodNodeBitSet]
	def rows: MultiBitSet
}


