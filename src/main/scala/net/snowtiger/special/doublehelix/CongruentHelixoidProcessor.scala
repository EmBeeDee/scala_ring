package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Row}

import scala.collection.{immutable, mutable}

/**
 * @author Mark
 */

object CongruentHelixoidProcessor extends HelixoidHelper(GoodPn(8).oneConsec)
{
	//val file = "double_x_18_14_58_36.lst"
	val file = "double.lst"
	//val file = "test.lst"
	val helixoids = streamReadHelixoidFile(file)
	//val helixoids = List("-16-14-14-18-18-14-14-18-16-16-14-18-58-36-14-16-58-58-16-16-58-16-58-58-18-14-16-")
	val justPB = true

	var c = 0

	def main(args: Array[String])
	{
		println("Finding Plain congruent lead-sets from "+file)
		for (pn <- helixoids)
			// Only mask out 1-3, since here we only want to expand TVs with different configurations of the front 3
			// Later on, when remixing course splices, we will want to expand out TVs of the back 5
			genDoubleTVsFor123Expansion(PN.parseToList(pn), processTV)
		println("Processed "+c+" methods.")
	}

	def processTV(pn: List[PN])
	{
		genValidHalfleadVariants(pn, LHPN).foreach{ search(_) }
	}

	val treblePathPNs =
	{
		val arr = new Array[List[PN]](7)
		val pnList = if (justPB) List(PN("-"), PN("18")) else allPN
		for (i <- 1 to 7)
			arr(i-1) = pnList.filter( Row(8).apply(_).bellAt(i+1)==i)
		arr
	}

	def search(helixoid: Method)
	{
		c+= 1
		var foundSome = false
		val treblePositions = partitionTreblePositions(helixoid)

		def search(n: Int, revPnList: List[PN])
		{
			if (n==8)
			{
				if (justPB)
				{
					if (false)
					{
						val hls = partitionRowsBySign(treblePositions(7))
						val lhs = partitionRowsBySign(treblePositions(0))
						var links = " hls: "
						if (linkPlainHunts(hls, List(PN("18"))))
							links+= "18 "
						if (linkPlainHunts(hls, List(PN("78"))))
							links+= "78 "
						links+= "lhs: "
						if (linkPlainHunts(lhs, List(PN("18"))))
							links+= "18 "
						if (linkPlainHunts(lhs, List(PN("12"))))
							links+= "12 "
						if (links==" hls: lhs: ")
							links = " not linked "
						println(helixoid+" ["+links+"] "+treblePositions(0).filter(_._2).map(_._1).hashCode())
					}
					else
					{
						val lhs = partitionRowsBySign(treblePositions(0))
						if (linkPlainHunts(lhs, List(PN("12"))))
						{
							val (positive, negative) = lhs
							var links = extractCourseHeads(positive, 2, 2).mkString(", ")
							println(helixoid+" [+ve chs: "+links+"] "+treblePositions(0).filter(_._2).map(_._1).hashCode())
						}
					}
				}
				else
				{
					if (!foundSome)
					{
						println
						println("Plain methods congruent with "+helixoid)
						foundSome = true
					}
					println(" "+PN.output(revPnList.reverse))
				}
			}
			else
			{
				for (pn <- treblePathPNs(n-1))
				{
					val usedRows = mutable.Set[(Row,Boolean)]()
					def badRow(pair: (Row, Boolean), pn: PN) =
					{
						val newRow = pair._1.apply(pn)
						val newSign = if (pn.swapsSign(8)) !pair._2 else pair._2
						val newPair = (newRow, newSign)
						if (treblePositions(n).contains(newPair) && !usedRows.contains(newPair))
						{
							usedRows+= newPair
							false
						}
						else
						{
							true
						}
					}

					if (!treblePositions(n-1).exists{badRow(_, pn)})
					{
						search(n+1, pn::revPnList)
					}
				}
			}
		}

		search(1, Nil)
	}

	def partitionTreblePositions(helixoid: Method): Array[mutable.Set[ (Row,Boolean)]] =
	{
		val threeLeads = helixoid.fullCourse.slice(0, HalfLeadLength*6)
		val treblePositions = new Array[mutable.Set[ (Row,Boolean)]](8)
		for (i <- 1 to 8)
			treblePositions(i-1) = mutable.Set[(Row,Boolean)]()
		for (row <- threeLeads)
		{
			val i = row.placeOf(1)
			treblePositions(i-1)+= Tuple2(maskBackFive(row), row.positive)
		}
		treblePositions
	}

	def partitionRowsBySign(rowSigns: mutable.Set[(Row,Boolean)]) =
	{
		val (positive, negative) = rowSigns.toSet.partition(_._2)
		(positive.map(_._1), negative.map(_._1))
	}

	def linkPlainHunts(partitioned: (Set[Row], Set[Row]), allowedPn: List[PN]): Boolean =
		linkPlainHunts(partitioned._1, partitioned._2, allowedPn)

	def linkPlainHunts(positive: Set[Row], negative: Set[Row], allowedPn: List[PN]): Boolean =
	{
		val plusToMinus = mutable.Map[Row, immutable.Set[Row]]()

		def notLinked(plusRow: Row) =
		{
			var minusSet = immutable.Set[Row]()
			for (pn <- allowedPn)
			{
				val minusRow = plusRow.apply(pn)
				if (negative.contains(minusRow))
					minusSet+= minusRow
			}
			plusToMinus.put(plusRow, minusSet)
			minusSet.isEmpty
		}

		!positive.exists(notLinked(_))
	}

	def extractCourseHeads(positive: Set[Row], bellHome: Int, homePos: Int) =
	{
		positive.filter( _.bellAt(homePos)==bellHome )
	}
}