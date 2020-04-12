package net.snowtiger.spliced.composition

import net.snowtiger.spliced.tables.{Node, Tables}

/**
 * @author mark
 */

case class ImmutableComposition(nodes1: List[Node], tables1: Tables) extends ImmutableCompositionBase(nodes1, tables1)
{
	override def makeNew(nodes: List[Node], tables: Tables) = ImmutableComposition(nodes, tables)

	override def replaceNode(i: Int, newNode: Node): ImmutableCompositionBase =
	{
		val (pre, post) = nodes.splitAt(i)
		ImmutableComposition(pre++(newNode::post.tail), tables)
	}

	override lazy val falseScore = super.falseScore

}