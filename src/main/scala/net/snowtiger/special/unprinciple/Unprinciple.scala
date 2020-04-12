package net.snowtiger.unprinciple

import net.snowtiger.ringing.{Method, PN, Row}


/**
 * @author Mark
 */

class Unprinciple(nbells: Int, lead: List[PN]) extends Method(nbells, lead)
{
	def this(nbells: Int, pn: String) = this(nbells, PN.parseToList(pn))

	def isSubPrinciple = firstLeadHead.isSubPrincipleLeadhead

	def isRepeatedPN = isRotationallySimilar(lead.size / 2, isDivisorOfLead, lead, lead)

	def allPathsIdentical =
	{
		val path1 = getPath(1)
		var cyclesChecked = Set(firstLeadHead.cyclePattern(0))
		def pathCheck(i: Int) =
		{
			val cycleNumber = firstLeadHead.cyclePattern(i - 1)
			if (cyclesChecked.contains(cycleNumber))
				true
			else
			{
				cyclesChecked += cycleNumber
				pathsMatch(path1, getPath(i))
			}
		}
		def recursivePathCheck(i: Int): Boolean = i >= nbells || (pathCheck(i) && recursivePathCheck(i + 1))
		recursivePathCheck(2)
	}

	def isDivisorOfLead(n: Int) = n > 0 && n % lead.size == 0

	def slideInterval = courseLength / nbells

	def isSlideInterval(n: Int) = n % slideInterval == 0

	def getPath(bell: Int) = fullCourse.map(_.placeOf(bell))

	def pathsMatch(path1: List[Int], path2: List[Int]) = isRotationallySimilar(courseLength - 1, (i: Int) => true, path1, path2)

	private def isRotationallySimilar[T](nRots: Int, checkThisRot: (Int) => Boolean, original: Seq[T], compare: Seq[T]) =
	{
		val xs = original.toIndexedSeq
		val ys = compare.toIndexedSeq
		def recursiveRotCheck(rot: Int): Boolean =
		{
			def valuesEqual(pos: Int) = xs(pos) == ys((pos + rot) % ys.size)
			def isRotEqual(pos: Int): Boolean = pos >= xs.size || (valuesEqual(pos) && isRotEqual(pos + 1))
			if (checkThisRot(rot) && isRotEqual(0))
				true
			else if (rot >= nRots)
				false
			else
				recursiveRotCheck(rot + 1)
		}
		recursiveRotCheck(1)
	}

	override def toString() =
	{
		super.toString() +
			{
				if (isSubPrinciple) " subprinciple"
			} +
			{
				if (allPathsIdentical) " unprinciple!"
			} +
			{
				if (isTrue) " true" else " false"
			}
	}


}

object Unprinciple
{

	def main(args: Array[String])
	{
		if (false)
		{
			test()
		}
		else
		{
			val allPn = PN.generateAll(8)
			//val allPn = List(PN("18"), PN("16"), PN("38"), PN("12"), PN("34"), PN("14"), PN("58"), PN("36"))
			//val allPn = List(PN("18"), PN("16"), PN("38"), PN("36"))
			//val allPn = List(PN("18"), PN("1678"))
			findUnprinciples(8, 8, allPn.toList, false)

			//val allPn = List(PN("x"), PN("16"), PN("14"), PN("36"), PN("34"))
			//val allPn = List(PN("16"), PN("14"), PN("36"), PN("12"), PN("34"), PN("56") )
			//findUnprinciples(6, 24, allPn, false)
		}
	}

	def test()
	{
		//val m = new Unprinciple(6, PN.parse("34-16-16-36-16-16-16-16-16-14-16-16-"))
		//val m = new Unprinciple(6, "16-16-16-16-16-14-16-16-34-16-16-36-")
		val m = new Unprinciple(6, "18-18-1678-1678-")
		println(m)
	}

	def findUnprinciples(nbells: Int, leadLen: Int, allPn: List[PN], rightPlace: Boolean)
	{
		def isGoodLeadhead(row: Row) = row.isSubPrincipleLeadhead
		def isGoodMethod(m: Unprinciple) = m.allPathsIdentical
		val finder = new TableMethodFinder(nbells, leadLen, allPn, isGoodLeadhead(_), isGoodMethod(_), rightPlace)
		finder.generateAll()
		println("Finished: found " + finder.methodsFound + " methods. Visited " + finder.nodesVisited + " nodes in " + finder.timeTaken.toFloat / 1000 + " seconds")
	}
}

case class TableMethodFinder(nbells: Int, leadLen: Int, pns: List[PN], isGoodLeadhead: (Row) => Boolean, isGoodMethod: (Unprinciple) => Boolean, rightPlace: Boolean)
{
	val finishDepth = 1

	val allPn = if (rightPlace) PN("x") :: pns else pns
	val max = allPn.size
	val copyStart = if (rightPlace) 1 else 0
	val searchDepth = leadLen - finishDepth

	var nodesVisited = 0L
	var methodsFound = 0
	var timeTaken = 0L

	val (table, tableBuildTime) = buildTable()
	val startNode = table.getNodeForRow(Row(nbells))

	def buildTable() =
	{
		println("Building tables for nbells=" + nbells + ", pn=" + allPn)
		var t = System.currentTimeMillis()
		val table = new Table(nbells, allPn)
		table.nodes.values.foreach
		{
			finishBuilder(_)
		}
		t = System.currentTimeMillis() - t
		println("Table build time: " + (t / 1000))
		(table, t)
	}

	def finishBuilder(node: Node)
	{
		def recurseFinish(node: Node, pns: List[Int])
		{
			node.visited = true
			for (c <- 0 until max)
			{
				val nextNode = node.nextNodes(c)
				if (!nextNode.visited)
				{
					val newPns = c :: pns
					nextNode.finishingSequences = newPns :: nextNode.finishingSequences
					if (newPns.size < finishDepth)
						recurseFinish(nextNode, newPns)
				}
			}
			node.visited = false
		}
		if (isGoodLeadhead(node.row))
			recurseFinish(node, Nil)
	}

	def isGoodFinish(pns: IndexedSeq[Int], copyFrom: Int, finishPns: List[Int]) =
	{
		val i = pns.size
		val combinedPns = pns ++ finishPns
		def recurseIsGoodFinish(i: Int, copyFrom: Int): Boolean =
		{
			if (i >= combinedPns.size)
				copyFrom == copyStart
			else
			{
				val x = combinedPns(i)
				val y = combinedPns(copyFrom)
				if (x < y)
					false
				else
					recurseIsGoodFinish(i + 1, if (x == y) copyFrom + 1 else 0)
			}
		}
		recurseIsGoodFinish(i, copyFrom)
	}

	def checkMethod(pn: Seq[Int])
	{
		val m = new Unprinciple(nbells, pn.toList.map
		{
			allPn(_)
		})
		if (isGoodMethod(m))
		{
			methodsFound += 1;
			println(m)
		}
	}

	def checkFinish(node: Node, pns: IndexedSeq[Int], copyFrom: Int)
	{
		node.finishingSequences.foreach
		{
			(finish: List[Int]) => if (isGoodFinish(pns, copyFrom, finish)) checkMethod(pns ++ finish)
		}
	}

	def generateAll()
	{
		val t = System.currentTimeMillis
		generatePns(0)
		timeTaken = System.currentTimeMillis - t
	}

	def generatePns(i: Int): List[List[PN]] =
	{
		type GenFn = (IndexedSeq[Int], Int, Int, Int, Node) => Unit
		def generateCross(pns: IndexedSeq[Int], pn: Int, copyFrom: Int, nodesLeft: Int, node: Node)
		{
			val crossNode = node.markVisitedAndGetNext(0)
			if (!crossNode.visited)
				generateWithRedirect(pns :+ 0, pn, copyFrom + 1, nodesLeft - 1, crossNode, generateCross)
			node.visited = false
		}
		def generate(pns: IndexedSeq[Int], pn: Int, copyFrom: Int, nodesLeft: Int, node: Node)
		{
			generateWithRedirect(pns, pn, copyFrom, nodesLeft, node, generate)
		}
		def generateWithRedirect(pns: IndexedSeq[Int], pn: Int, copyFrom: Int, nodesLeft: Int, node: Node, gen: GenFn)
		{
			nodesVisited += 1
			val newPns = pns :+ pn
			val nextNode = node.markVisitedAndGetNext(pn)
			if (!nextNode.visited)
			{
				//println("Gen: "+newPns+" = "+nextNode+" c? "+copyFrom+" left="+nodesLeft)
				if (copyFrom == copyStart && isGoodLeadhead(nextNode.row))
					checkMethod(newPns)
				if (nodesLeft == 0)
					checkFinish(nextNode, newPns, copyFrom)
				else
				{
					val copy = newPns(copyFrom)
					if (nodesLeft > 1)
					{
						gen(newPns, copy, copyFrom + 1, nodesLeft - 1, nextNode)
					}
					List.range(copy + 1, max).foreach((c: Int) => gen(newPns, c, 0, nodesLeft - 1, nextNode))
				}
			}
			node.visited = false
		}

		if (i >= max)
			Nil
		else
		{
			if (rightPlace)
			{
				if (i > 0)
					generateCross(IndexedSeq.empty[Int], i, 0, searchDepth - 1, startNode)
			}
			else
			{
				generate(IndexedSeq.empty[Int], i, 0, searchDepth - 1, startNode)
			}
			generatePns(i + 1)
		}
	}
}