package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.tables.{Node, Tables}

import scala.collection.BitSet
import scala.collection.immutable.TreeMap

/**
 * Mustn't be a case class - equals and hashcode do not depend on originalNodes, since these may have changed
 *
 * @author mark
 */
class FastComp(originalNodes: List[Node], val tables: Tables) extends Composition
{
	def this(comp: Composition) = this(comp.nodes, comp.tables)

	val nodeArray = originalNodes.toArray
	var immutableComp: Option[ImmutableComposition] = None

	private var replacementListeners = List[ReplacementListener]()
	def addReplacementListener(listener: ReplacementListener) {replacementListeners = listener::replacementListeners}

	var lastReplaceIndex = -1
	var lastReplacedEl:Node = _

	var nodes = originalNodes

	def toImmutableComp =
	{
		if (immutableComp.isEmpty)
			immutableComp = Some(ImmutableComposition(nodes, tables))
		immutableComp.get
	}

	def makeNew(nodes: List[Node], tables: Tables) = new FastComp(nodes, tables)

	def leadBitSetWithoutNode(i: Int) =
	{
		var leadBitSet = BitSet()
		for (j<-0 until nodeArray.size)
			if (i!=j)
				leadBitSet = leadBitSet|nodeArray(j).leads
		leadBitSet
	}

	def falseLeadBitSetWithoutNode(i: Int) = ???

	def replaceNode(i: Int, newNode: Node) =
	{
		lastReplaceIndex = i
		lastReplacedEl = nodeArray(i)
		doReplaceNode(i, newNode)
	}

	def undoLastReplace()
	{
		if (lastReplaceIndex>=0)
		{
			doReplaceNode(lastReplaceIndex, lastReplacedEl)
			lastReplaceIndex = -1
		}
	}

	private def doReplaceNode(i: Int, newNode: Node): FastComp =
	{
		nodeArray(i) = newNode
		nodes = nodeArray.toList
		for (listener <- replacementListeners)
			listener.replace(i)
		immutableComp = None
		this
	}

	val musicArray = new LazyFastArray[MusicCount](this, (i)=>nodeArray(i).music)
	def music = musicArray.getTotal + compPlan.excludedMusic

	case class IntProperty(i: Int) extends Subtractable[IntProperty]
	{
		def +(other: IntProperty) = IntProperty(i+other.i)
		def -(other: IntProperty) = IntProperty(i-other.i)
	}
	val lengthArray = new LazyFastArray[IntProperty](this, (i)=>IntProperty(nodeArray(i).length))
	def length = (lengthArray.getTotal.i * compPlan.nparts) + compPlan.excludedLength

	def comSuper = super.com
	val comProperty = new LazyCompositionProperty[Int](this) {protected def recalc() = comSuper}
	override def com = comProperty.getTotal

	def longestNoComSuper = super.longestNoComRun
	val longestNoComProperty = new LazyCompositionProperty[Int](this){protected def recalc() = longestNoComSuper}
	override def longestNoComRun = longestNoComProperty.getTotal

	case class MethodsUsed(methods: TreeMap[NamedMethod, Int]) extends Subtractable[MethodsUsed]
	{
		def this(countPairs: Set[Tuple2[NamedMethod,Int]]) = this(TreeMap[NamedMethod,Int]()++countPairs.toMap)
		def this(splice: List[NamedMethod]) = this(splice.toSet.map{ (m:NamedMethod)=> (m, splice.count(_==m)) } )

		def +(other: MethodsUsed) = MethodsUsed(addToMap(other.methods))
		def -(other: MethodsUsed) = MethodsUsed(subtractFromMap(other.methods))
		def addToMap(other: TreeMap[NamedMethod, Int]) =
		{
			var newMap = methods
			for (m <- other.keys)
				newMap+= m -> (newMap.getOrElse(m, 0)+other(m))
			newMap
		}
		def subtractFromMap(other: TreeMap[NamedMethod, Int]) =
		{
			var newMap = methods
			for (m <- other.keys)
				newMap+= m -> (newMap.getOrElse(m, 0)-other(m))
			newMap
		}
	}
	val excludedMethods = new MethodsUsed(compPlan.excludedLeads.toList.map{_.method})
	val methodsUsedArray = new LazyFastArray[MethodsUsed](this, (i)=> new MethodsUsed(nodeArray(i).methods))
	def methodCounts = methodsUsedArray.getTotal+excludedMethods
	def methodsUsed = methodCounts.methods.keySet
	def sortedMethodsUsed = methodsUsed.toList

	def methodCounts(methods: List[NamedMethod]) = methods.map{methodCounts.methods(_)}

	val atwArray = new LazyPyramidArray[MultiAtw](this, (i)=> nodeArray(i).atw)
	def atwTable = atwArray.getTotal + compPlan.baseAtw

	def scoreSuper = super.score
	val scoreProperty = new LazyCompositionProperty[Int](this) {protected def recalc() = scoreSuper}
	override def score = scoreProperty.getTotal

	// Make sure score listener is last!
	replacementListeners = replacementListeners.reverse

	override def hashCode() = nodes.hashCode()

	override def equals(obj: scala.Any) =
	{
		obj match
		{
			case that: Composition => nodes==that.nodes
			case _ => false
		}
	}
}