package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.atw.construct.TreblePath

import scala.collection.mutable

/**
 * @author mark
 */

class AtwMethodFinderBase extends TreblePath
{
	def makeMethodNodes(course: MethodCourse, methods: Iterable[SearchMethod]): Set[TruthTableMethodNode] =
		methods.map{(m)=> new MethodNodeWithTruth(course,m)}.filter(_.isTrue).toSet

	def makeNumberedMethodNodes(course: MethodCourse, methods: Iterable[SearchMethod]): Set[NumberedMethodNode] =
		methods.map{(m)=> new NumberedMethodNode(course,m)}.filter(_.isTrue).toSet

	/** The search methods ought to have the full lead - i.e. be instances of FullSearchMethod */
	def makeMethodNodesWithPrefixTruth(course: MethodCourse, methods: Iterable[SearchMethod]): Set[TruthTableMethodNode] =
	{
		val methodList = methods.toList
		if (methodList.isEmpty)
			Set()
		else if (methodList.head.isInstanceOf[FullSearchMethod])
			methods.map{(m)=> new MethodNodeWithPrefixTruth(course,m)}.filter(_.isTrue).toSet
		else
			makeMethodNodes(course, methods)
	}

	def buildTruthTables[M <: TruthTableMethodNode](nodes: List[Set[M]]): List[Set[M]] =
	{
		print("Building true-node tables...")
		val t = System.currentTimeMillis()
		val courses = nodes.map{_.head.course}.toArray
		val sizes = nodes.map{_.size}.toArray
		if (nodes.head.head.isInstanceOf[MethodNodeWithPrefixTruth])
			prefixOptimisationBuild(nodes.asInstanceOf[List[Set[MethodNodeWithPrefixTruth]]], courses, sizes)
		else
			standardBuild(nodes.asInstanceOf[List[Set[TruthTableMethodNode]]], courses, sizes)
		println(" in "+((System.currentTimeMillis()-t)/1000)+"s")
		print("Pruning method tables...")
		var nPruned = 1
		var revPruned = nodes.reverse
		while (nPruned>0)
		{
			nPruned = 0
			// Prune nodes which do not have any true links to one or more other courses
			revPruned = revPruned.head::revPruned.tail.map{(methods)=>
			{
				val (pruned, remaining) = methods.partition{_.isPrunable}
				nPruned+= pruned.size
				for (prunedNode <- pruned; pointsTo <- prunedNode.revTruth)
					pointsTo.removePruned(prunedNode)
				remaining
			} }
			print(" "+nPruned)
		}
		println()
		revPruned.reverse
	}

	def standardBuild(nodes: List[Set[TruthTableMethodNode]], courses: Array[MethodCourse], sizes: Array[Int])
	{
		val nodeArray = nodes.toArray
		for (i <- 0 until nodes.size)
		{
			val theseMethods = nodeArray(i)
			for (j <- i+1 until nodes.size)
			{
				theseMethods.foreach{_.buildTable(courses(j), nodeArray(j))}
			}
			print(" "+sizes(i))
		}
	}

	def prefixOptimisationBuild(nodes: List[Set[MethodNodeWithPrefixTruth]], courses: Array[MethodCourse], sizes: Array[Int])
	{
		val prefixes = nodes.map{_.groupBy(_.prefix)}.toArray
		for (i <- 0 until nodes.size)
		{
			for ( (thisPrefix, theseMethods ) <- prefixes(i))
				for (j <- i+1 until nodes.size)
				{
					val otherPrefixes = prefixes(j).filterKeys{(p)=> TruthTableMethodNode.quickTruthCheck(thisPrefix, p)}
					val candidateMethods = otherPrefixes.values.flatten
					theseMethods.foreach{_.buildTable(courses(j), candidateMethods.toSet)}
				}
			print(" "+sizes(i))
		}
	}

	def removePoorlyLinkedNodes(nodes: List[Set[TruthTableMethodNode]], proportionToKeep: Double) =
	{
		def nLinks(method: TruthTableMethodNode) = method.trueTable.values.map(_.size).sum + method.revTruth.size
		def removeWorst(methods: Set[TruthTableMethodNode]) =
		{
			val sorted = methods.toList.sortBy{-nLinks(_)}
			var (keep, lose) = sorted.splitAt((proportionToKeep*sorted.size).toInt)
			for (prunedNode <- lose; pointsTo <- prunedNode.revTruth)
				pointsTo.removePruned(prunedNode)
			keep.toSet
		}
		if (proportionToKeep<1.0)
			nodes.map(removeWorst)
		else
			nodes
	}

	def singleMethodFalseness(method: SearchMethod, courseSet: List[MethodCourse]) =
	{
		def generateLead(lh: Row) =
		{
			val builder = mutable.Buffer[Row]()
			method.generateLead(lh, builder)
			builder.toSet
		}
		val allLeads = courseSet.flatMap{_.leadheads.map(generateLead)}.toArray
		var nTrue = 0
		for (i <- 0 until allLeads.size)
			for (j <- i+1 until allLeads.size)
				if (TruthTableMethodNode.quickTruthCheck(allLeads(i), allLeads(j)))
					nTrue+= 1
		nTrue
	}

}