package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.tables.Node.{falseLeadBitSet, leadBitSet}
import net.snowtiger.spliced.tables.{Node, Tables}

import scala.collection.{BitSet, mutable}

/**
 * @author mark
 */

abstract class ImmutableCompositionBase(val nodes: List[Node], val tables: Tables) extends Composition
{
	def makeNew(nodes: List[Node], tables: Tables): ImmutableCompositionBase

	def toImmutableComp = this

	def replaceNode(i: Int, newNode: Node): ImmutableCompositionBase

	def undoLastReplace() = {} // No-op

	override def leadBitSetWithoutNode(i: Int): BitSet =
	{
		val (pre, post) = nodes.splitAt(i)
		leadBitSet(pre, leadBitSet(post.tail))
	}

	override def falseLeadBitSetWithoutNode(i: Int): BitSet =
	{
		val (pre, post) = nodes.splitAt(i)
		falseLeadBitSet(pre, falseLeadBitSet(post.tail))
	}

	/** Includes the length of any excluded leads - it is assumed these will be manually added to the result compositions */
	lazy val length = (nodes.map{_.length}.sum * compPlan.nparts) + compPlan.excludedLength

	lazy val music = MusicCount.sumAll(compPlan.excludedMusic, nodes.map{_.music})

	/** Includes excluded leads; not massively efficient since it regens and re-interns the (included) leads */
	override lazy val allLeads = super.allLeads

	override lazy val leadsWithoutMusic = super.leadsWithoutMusic

	/** Doesn't include any COM caused by post-search addition of the excluded leads - too difficult to determine */
	override lazy val com = super.com

	override lazy val longestNoComRun = super.longestNoComRun

	/** Includes methods used in excluded leads */
	lazy val methodsUsed =
	{
		val counts = mutable.Set[NamedMethod]()
		for (node <- nodes)
			counts++= node.methodsUsed
		for (lead <- compPlan.excludedLeads)
			counts+= lead.method
		counts.toSet
	}

	lazy val sortedMethodsUsed = methodsUsed.toList.sortWith{ (a,b)=> a.toString < b.toString }

	/** Includes methods in excluded leads */
	def methodCounts(methods: List[NamedMethod]): List[Int] =
	{
		val counts = mutable.Map[NamedMethod, Int]()
		for (node <- nodes; m <- node.methods)
		{
			val c = counts.getOrElseUpdate(m, 0)
			counts+= m -> (c+compPlan.nparts)
		}
		for (lead <- compPlan.excludedLeads)
		{
			val m = lead.method
			val c = counts.getOrElseUpdate(m, 0)
			counts+= m -> (c+1)
		}
		methods.map{counts.getOrElse(_, 0)}
	}

	override lazy val leadsToLastMethod = super.leadsToLastMethod
	override lazy val longestAbsence = super.longestAbsence

	/** Includes methods in excluded leads */
	lazy val atwTable = nodes.foldLeft(compPlan.baseAtw){ (atw,node) => atw+node.atw }

	override lazy val levenshteinPartDistance = super.levenshteinPartDistance

	override lazy val score = super.score
}