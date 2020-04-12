package net.snowtiger.triples

import java.io.PrintWriter

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Perm, Row}

import scala.collection.mutable

object HelixFinder
{
	val NBells = 7
	val HalfLeadLength = 210
	val PN1 = PN("1")
	val PN7 = PN("7")
	val PN147 = PN("147")
	val AllPN = List(PN1, PN7, PN("3"), PN("5"))
	//val AllPN = List(PN1, PN("3"), PN7)
	//val AllPN = List(PN1, PN("3"), PN("5"), PN7, PN147)
	//val AllPN = List(PN1, PN("3"), PN("5"), PN7, PN("125"), PN147)

	val QuarterLeadPn = PN147
	val StartRow = Row(NBells)
	val LHPerm = Perm("2316475")

	//val QLPerm = Perm("2154376") // Also need to reverse row, of course
	//val Pairs = Set( Set(1,2), Set(3,5), Set(7,6) ) // Must match QLPerm
	//val HalfLeadPn = PN("3")

	val QLPerm = Perm("2176543")
	// Also need to reverse row, of course
	val Pairs = Set(Set(1, 2), Set(3, 7), Set(4, 6))
	// Must match QLPerm
	val HalfLeadPn = PN("1")

	val HLPerm = Perm("2135476")

	val Rot3Perm = Perm("2314567")

	val Table = new CosetTable(LHPerm, HLPerm, QLPerm, PN("147")::PN("125")::AllPN)

	var c = 0
	var nFound = 0

	def maskFrontThree(row: Row) = Row(for (c <- row.toString) yield if (c == '1' || c == '2' || c == '3') 'X' else c)

	def maskBackFour(row: Row) = Row(for (c <- row.toString) yield if (c == '1' || c == '2' || c == '3' || c == 'X') c else '.')

	def maskAll(row: Row) = Row(for (c <- row.toString) yield if (c == '1' || c == '2' || c == '3' || c == 'X') 'X' else '.')

	def main(args: Array[String])
	{
		/*
				 val out = if (args.length==1) Some(new PrintWriter(args(0))) else None
				 findMethods(out)
				 if (out.isDefined)
					 out.get.close()
				 */
		findMethods(null)
	}


	def findMethods(out: Option[PrintWriter])
	{

		def outputMethod(pnList: List[PN], out: Option[PrintWriter])
		{
			val row = PN.generateLastRow(StartRow, pnList)
			val method = new Method(7, (HalfLeadPn :: (pnList.reverse)).reverse, PN147)
			val log = method.isTrue + " " + method.toString()
			println(log)
			val checker = new HelixChecker(method)
			checker.check()
			nFound += 1
		}

		def progressCheck(revPnList: List[PN])
		{
			if (revPnList.size > c)
			{
				c = revPnList.size
				println("" + c + " \"" + PN.output(revPnList.reverse) + "\"")
			}
		}

		def standardHelixSearch(row: Row, revPn: List[PN], rowsVisited: Set[Row])
		{
			progressCheck(revPn)
			if (revPn.size==35-1)
			{
				val halfLeadPn = if (revPn.head==PN7) PN("5") else PN7
				//val method = new Method(7, (halfLeadPn::revPn).reverse, PN147)
				val method = new Method(7, revPn.reverse)
				val log = method.toString()+", 2nd halflead = "+PN.output(revPn)
				println(log)
				nFound += 1
			}
			else
			{
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn<-AllPN)
					if (GoodPn.isAllowableConsecutivePn(pn, revPn))
					{
						val next = row.apply(pn)
						if (!pnResultsForThisChange.contains(next) && !rowsVisited.contains(next))
						{
							standardHelixSearch(next, pn::revPn, rowsVisited+next)
							pnResultsForThisChange+= next
						}
					}
			}
		}

		/**
		 * nodesVisited expected to contain row.
		 */
		def normalSearch(node: Node, revPn: List[PN], nodesVisited: Set[Node])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength - 1)
			{
				outputMethod(revPn.reverse, out)
			}
			else
			{
				def tryPN(pn: PN, next: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(next))
							normalSearch(next, pn :: revPn, nodesVisited + next)
				}
				node.foreach(tryPN)
			}
		}

		/**
		 * nodesVisited expected to contain row.
		 */
		def randomSearch(node: Node, revPn: List[PN], nodesVisited: Set[Node])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength - 1)
			{
				outputMethod(revPn.reverse, out)
			}
			else
			{
				var searches = List[(PN,Node)]()
				def tryPN(pn: PN, next: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(next))
						searches = (pn,next)::searches
				}
				node.foreach(tryPN)
				if (!searches.isEmpty)
				{
					if (searches.size==1)
						randomSearch(searches.head._2, searches.head._1 :: revPn, nodesVisited + searches.head._2)
					else
					{
						val probs = (0 until searches.size).map{(i)=>Math.random<0.25}.toList
						if (probs.exists{ (b)=>b })
						{
							probs.zip(searches).foreach( (x) => if (x._1) randomSearch(x._2._2, x._2._1::revPn, nodesVisited+x._2._2))
						}
						else
						{
							val search = searches.drop( (Math.random*searches.size).toInt ).head
							randomSearch(search._2, search._1 :: revPn, nodesVisited + search._2)
						}
					}
				}
			}
		}

		/**
		 * nodesVisited expected to contain row.
		 */
		def restrictedNormalSearch(row: Row, node: Node, revPn: List[PN], nodesVisited: Set[Node], maskedRowsVisited: Set[Row])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength - 1)
			{
				outputMethod(revPn.reverse, out)
			}
			else
			{
				def tryPN(pn: PN, next: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(next))
					{
						val newRow = row.apply(pn)
						val masked = maskBackFour(newRow)
						if (!maskedRowsVisited.contains(masked))
							restrictedNormalSearch(newRow, next, pn :: revPn, nodesVisited + next, maskedRowsVisited + masked)
					}
				}
				node.foreach(tryPN)
			}
		}

		def quarterLeadPerm(row: Row) = row.reverse.permuteBy(QLPerm)

		def goodQuarterLead(row: Row) =
		{
			val pair1 = Set(row.bellAt(1), row.bellAt(7))
			val pair2 = Set(row.bellAt(2), row.bellAt(5))
			val pair3 = Set(row.bellAt(3), row.bellAt(6))
			Pairs == Set(pair1, pair2, pair3)
		}

		/**
		 * rowMap expected to contain row.
		 */
		def doubleSearch(row: Row, node: Node, revPn: List[PN], nodesVisited: Set[Node])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength / 2 - 1)
			{
				if (goodQuarterLead(row))
				{
					val remainingPn = QuarterLeadPn :: revPn.map(_.reverse(NBells))
					outputMethod(revPn.reverse ++ remainingPn, out)
				}
			}
			else
			{
				def tryPN(pn: PN, nextNode: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(nextNode))
					{
						var nextRow = row.apply(pn)
						val qlRow = quarterLeadPerm(nextRow)
						val qlNode = Table.get(qlRow)
						if (nextNode != qlNode && !nodesVisited.contains(qlNode))
							doubleSearch(nextRow, nextNode, pn :: revPn, nodesVisited + nextNode + qlNode)
					}
				}
				node.foreach(tryPN)
			}
		}

		/**
		 * rowsVisited expected to contain node
		 */
		def restrictedDoubleSearch(row: Row, node: Node, revPn: List[PN], nodesVisited: Set[Node], maskedRowsVisited: Set[Row])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength / 2 - 1)
			{
				if (goodQuarterLead(row))
				{
					val remainingPn = QuarterLeadPn :: revPn.map(_.reverse(NBells))
					outputMethod(revPn.reverse ++ remainingPn, out)
				}
			}
			else
			{
				def tryPN(pn: PN, nextNode: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(nextNode))
					{
						val nextRow = row.apply(pn)
						val qlRow = quarterLeadPerm(nextRow)
						val qlNode = Table.get(qlRow)
						if (nextNode != qlNode && !nodesVisited.contains(qlNode))
						{
							val masked1 = maskBackFour(nextRow)
							val masked2 = maskBackFour(qlRow)
							if (!maskedRowsVisited.contains(masked1) && !maskedRowsVisited.contains(masked2))
							{
								restrictedDoubleSearch(nextRow, nextNode, pn :: revPn, nodesVisited + nextNode + qlNode, maskedRowsVisited + masked1 + masked2)
							}
						}
					}
				}
				node.foreach(tryPN)
			}
		}

		val TailSize = 30
		val qlRow = Row("2765341")
		assert(quarterLeadPerm(qlRow) == qlRow.apply(PN147))
		val tailPN = PN.parseToList("1.7.1.7.1.7.3.1.7.1.7.1.3.1.7.1.7.1.7.1.7.1.7.1.7.1.5.1.7.3")	// Reverse of what we need

		/**
		 * rowMap expected to contain row.
		 */
		def doubleMidSearch(row: Row, node: Node, revPn: List[PN], nodesVisited: Set[Node])
		{
			progressCheck(revPn)
			if (revPn.size == HalfLeadLength / 2 - 1 - TailSize)
			{
				if (row==qlRow)
				{
					val pn1 = tailPN++revPn
					val pn2 = QuarterLeadPn :: pn1.map(_.reverse(NBells))
					outputMethod(pn1.reverse++pn2, out)
				}
			}
			else
			{
				def tryPN(pn: PN, nextNode: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(nextNode))
					{
						var nextRow = row.apply(pn)
						val qlRow = quarterLeadPerm(nextRow)
						val qlNode = Table.get(qlRow)
						if (nextNode != qlNode && !nodesVisited.contains(qlNode))
							doubleMidSearch(nextRow, nextNode, pn :: revPn, nodesVisited + nextNode + qlNode)
					}
				}
				node.foreach(tryPN)
			}
		}

		/**
		 * rowMap expected to contain row.
		 */
		def doubleTailSearch(row: Row, node: Node, revPn: List[PN], nodesVisited: Set[Node])
		{
			progressCheck(revPn)
			if (revPn.size == TailSize)
			{
				val pnList = revPn.reverse
				val row = PN.generateLastRow(qlRow, pnList)
				println(row + " " + PN.output(pnList))
				nFound += 1
			}
			else
			{
				def tryPN(pn: PN, nextNode: Node)
				{
					if (GoodPn.isAllowableConsecutivePn(pn, revPn) && !nodesVisited.contains(nextNode))
					{
						var nextRow = row.apply(pn)
						val qlRow = quarterLeadPerm(nextRow)
						val qlNode = Table.get(qlRow)
						if (nextNode != qlNode && !nodesVisited.contains(qlNode))
							doubleTailSearch(nextRow, nextNode, pn :: revPn, nodesVisited + nextNode + qlNode)
					}
				}
				node.foreach(tryPN)
			}
		}

		val startNode = Table.get(StartRow)

		if (false)
		{
			val maskedStart = maskAll(StartRow)
			standardHelixSearch(maskedStart, Nil, Set(maskedStart))
		}

		if (true)
		{
			//val startPn = PN.parse("5.1.3.5.1.7.5.1.3.5.7.5.3.1.7.1.5.3.147.3.147.3.5.3.5.3.147.5.1.7.3.5.7.3.125.3.7.5.3.7.1.5.147.3.5.3.5.3.147.3.147.3.5.1.7.1.3.5.7.5.3.1.5.7.1.5.3.1.5.147.5.1.3.5.1.7.5.1.3.5.7.5.3.1.7.1.5.3.147.3.147.3.5.3.5.3.147.5.1.7.3.5.7.3.5.3.7.5.3.7.1.5.147.3.5.3.5.3.147.3.147.3.5.1.7.1.3.5.7.5.3.1.5.7.1.5.3.1.5.147")
			val startPn = PN.parseToList("7.1.7.1.3.7.1.7.1.3.7.1.7.1.5.7.3.7.3.1.7.5.3.7.3.7.5.1.3.7.1.7.5.1.147.1.5.7.1.7.3.1.5.7.3.7.3.5.7.1.3.7.3.7.5.1.7.1.7.3.1.7.1.7.3.1.7.1.7.1. 7.1.7.1.3.7.1.7.1.3.7.1.7.1.5.7.3.7.3.1.7.5.3.7.3.7.5.1.3.7.1.7.5.1.5.1.5.7.1.7.3.1.5.7.3.7.3.5.7.1.3.7.3.7.5.1.7.1.7.3.1.7.1.7.3.1.7.1.7.147.")
			//val startPn = List[PN]()
			println("Normal search starting from "+PN.output(startPn))
			var node = startNode
			var set = Set(startNode)
			var c  = 1
			for (pn <- startPn)
			{
				node = node.nextFromPn(pn)._2
				set = set + node
				c+= 1
				println("PN "+c+", set size = "+set.size+" node = "+node)
			}
			assert(set.size==1+startPn.size)
			normalSearch(node, startPn.reverse, set)
		}

		if (false)
		{
			println("Random normal search")
			randomSearch(startNode, List(), Set(startNode))
		}

		if (false)
		{
			println("Restricted normal search")
			restrictedNormalSearch(StartRow, startNode, List(), Set(startNode), Set(maskBackFour(StartRow)))
		}

		if (false)
		{
			println("Double search for QL perm " + QLPerm)
			doubleSearch(StartRow, startNode, List(), Set(startNode, Table.get(quarterLeadPerm(StartRow))))
		}

		if (false)
		{
			println("Restricted Double search for QL perm " + QLPerm)
			restrictedDoubleSearch(StartRow, startNode, List(), Set(startNode, Table.get(quarterLeadPerm(StartRow))), Set(maskBackFour(StartRow)))
		}

		if (false)
		{
			println("Double tail search from QL row " + qlRow)
			doubleTailSearch(qlRow, startNode, List(), Set(Table.get(qlRow), Table.get(quarterLeadPerm(qlRow))))
		}

		if (false)
		{
			println("Double search for QL perm " + QLPerm + " for tail "+PN.output(tailPN))
			var set = Set(startNode, Table.get(quarterLeadPerm(StartRow)))
			var row = qlRow
			for (pn <- tailPN)
			{
				set = set + Table.get(row) + Table.get(quarterLeadPerm(row))
				row = row.apply(pn)
			}
			doubleSearch(StartRow, startNode, List(), set)
		}

		println("Found " + nFound + " methods.")
	}
}
