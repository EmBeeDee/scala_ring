package net.snowtiger.unprinciple

import net.snowtiger.ringing.{PN, Row}

/**
 * @author Mark
 */

class Table(val nbells: Int, val pn: List[PN])
{

	import scala.collection.mutable

	val nodes: Map[Row, Node] = buildNodeMap()

	private def buildNodeMap(): Map[Row, Node] =
	{
		val map = mutable.Map[Row, Node]()
		for (r <- Row.generateAll(nbells))
			map(r) = new Node(r, this)
		println("Table size: " + map.size)
		// Make immutable
		Map() ++ map
	}

	def getNodeForRow(row: Row) =
	{
		nodes.get(row) match
		{
			case Some(n) => n
			case None =>
			{
				println("Row not found in table: " + row); null
			}
		}
	}

}

class Node(val row: Row, table: Table)
{
	lazy val nextNodes = buildNextNodes()
	var finishingSequences = List[List[Int]]()

	def hasGoodFinish = finishingSequences.size > 0

	var visited = false

	def buildNextNodes(): Seq[Node] =
	{
		table.pn.map
		{
			(pn: PN) => table.getNodeForRow(row.apply(pn))
		}
	}

	def markVisitedAndGetNext(pnIndex: Int): Node =
	{
		visited = true
		nextNodes(pnIndex)
	}

	override def toString = row.toString
}