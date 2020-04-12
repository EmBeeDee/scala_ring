package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.tables.{Node, Tables}

/**
 * @author mark
 */

case class ImmutableFalseTrackerComposition(override val falseScore: Int, nodes1: List[Node], methodUseTracker: MethodUseTracker, tables1: Tables)
		extends ImmutableCompositionBase(nodes1, tables1)
{
	override def makeNew(nodes: List[Node], tables: Tables) =
		ImmutableFalseTrackerComposition(Composition.falseScore(nodes), nodes, MethodUseTracker(nodes), tables)

	override lazy val methodsUsed: Set[NamedMethod] = methodUseTracker.methodUse.keySet

	override def methodCounts(methods: List[NamedMethod]) = methods.map(methodUseTracker.methodUse)

	override def atwScore = methodUseTracker.atwScore

	override def isAtw = methodUseTracker.isAtw

	override def balanceString = super.balanceString

	override def replaceNode(i: Int, newNode: Node) =
	{
		val (pre, post) = nodes.splitAt(i)
		val oldNode = post.head
		val postTail = post.tail
		val newNodes = pre++(newNode::postTail)
		val oldFalse = doubleFalseScore(oldNode, pre)+doubleFalseScore(oldNode, postTail)
		val newFalse = doubleFalseScore(newNode, pre)+doubleFalseScore(newNode, postTail)
		val newScore = falseScore+newFalse-oldFalse
		val newMethodTracker = methodUseTracker + newNode.mut - oldNode.mut
		ImmutableFalseTrackerComposition(newScore, newNodes, newMethodTracker, tables)
	}

	private def doubleFalseScore(node: Node, nodes: List[Node]): Int =
		nodes.map{ (other)=> Composition.falseScore(node, other)+Composition.falseScore(other, node)}.sum

}

