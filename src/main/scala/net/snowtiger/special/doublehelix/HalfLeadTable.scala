package net.snowtiger.doublehelix

import net.snowtiger.ringing.{PN, Row}

import scala.collection.mutable

/**
 * Table of pre-halfleads (handstroke half-lead) which are able to produce a valid (3,5)-differential leadhead.
 * Valid pre-halfleads are mapped to the set of PNs which produce valid backstroke halfleads.
 *
 * @author Mark
 */

class HalfLeadTable
{
	import Row.Rounds

	val halfLeadTable = buildHalfLeadTable()

	/** Returns Option[Map halfLeadPn -> List(leadHeadPn)]  */
	def getHalfleadPnMap(preHalfLead: Row) = halfLeadTable.get(preHalfLead)

	/**
	 * Table build
	 * @return
	 */
	private def buildHalfLeadTable(): Map[Row, Map[PN, List[PN]]] =
	{
		def groupBellPositions(earlierPosition: Int, groups: List[List[Int]]): List[List[Int]] =
		{
			val Later = earlierPosition+1
			groups match
			{
				case (Later::tail)::otherGroups => (earlierPosition::Later::tail)::otherGroups
				case _ => List(earlierPosition)::groups
			}
		}

		def isOdd(n: Int) = (n != 2*(n/2))

		def countOddGroups(group: List[Int], count: Int) = if (isOdd(group.size)) count+1 else count

		def createPnGenerators(group: List[Int]): Seq[Int] =
		{
			if (isOdd(group.size))
				for (i <- group(0) until group(0)+group.size by 2 ) yield  i
			else
				List()
		}

		def generateUnsortedPns(generatorList: List[Seq[Int]]): List[List[Int]] =
		{
			def addPrefix(prefix: Int, list: List[List[Int]]) = if (list.isEmpty) List(List(prefix)) else list.map( prefix :: _ )

			generatorList match
			{
				case head::tail =>
				{
					val tailGen = generateUnsortedPns(tail)
					if (head.isEmpty)
						tailGen
					else
					{
						val ret = head.toList.flatMap( addPrefix(_, tailGen) )
						ret
					}
				}
				case _ =>
					Nil
			}
		}

		def createSortedPn(unsorted: List[Int]): PN =
		{
			val chars = unsorted.sorted.map(Row.Rounds(_))
			PN(chars.mkString)
		}

		def findHalfLeadPnSetForRow(row: Row): List[PN] =
		{
			val indexed = row.toString.zipWithIndex
			val frontBells = indexed.filter( _._1 <= '3').map(_._2)
			val backBells = indexed.filter( _._1 > '3').map(_._2)
			val emptyGroups: List[List[Int]] = Nil
			val frontGroups = frontBells.foldRight(emptyGroups)(groupBellPositions)
			if (frontGroups.size<=2)
			{
				val backGroups = backBells.foldRight(emptyGroups)(groupBellPositions)
				val nOddBackGroups = backGroups.foldRight(0)(countOddGroups)
				if (nOddBackGroups<=1)
				{
					val pnGeneratorList = frontGroups.map(createPnGenerators)++backGroups.map(createPnGenerators)
					val unsortedPns = generateUnsortedPns(pnGeneratorList)
					val sortedPn = unsortedPns.map( createSortedPn(_) )
					return sortedPn.toList
				}
			}
			Nil
		}

		/**
		 * Return all leadhead PNs which can generate the (3,5)-cycles.
		 * Bells 1,2,3 must be at the front for a valid Helix method; if not we return None.
		 * It is possible we may return Some[Nil] if, despite the front three being at the front and the back three
		 * at the back, there are no PNs which give a 5-cycle: this can happen if the handstroke lead is xxx54687.
		 */
		def checkLeadHead(preHalfLead: Row, halfLeadPn: PN): Option[List[PN]] =
		{
			def backPns(back5: String): List[PN] =
			{
				def downBy3(c: Char) = Rounds(Rounds.indexOf(c)-3)
				val row = Row( back5.map(downBy3(_)).mkString )
				val trialPns = for (i <-1 to 5 by 2) yield PN(""+i)
				val validPns = trialPns.filter( row.apply(_).cycleSizes==List(5) )
				validPns.toList.map( _.shiftUp(3) )
			}

			def prependPN(c: Char, pn: PN) = PN(c+pn.toString)

			Row(8).reflectAroundPivot(preHalfLead, halfLeadPn) match
			{
				case Some(leadend) =>
				{
					val front3 = leadend.toString.substring(0, 3)
					val back5 = leadend.toString.substring(3, 8)
					front3 match
					{
						case "132" => Some(backPns(back5).map(prependPN('3',_)))
						case "213" => Some(backPns(back5).map(prependPN('1',_)))
						case "321" => Some(backPns(back5).map(prependPN('1',_)) ++ backPns(back5).map(prependPN('3',_)))
						case _ => None
					}
				}
				case None => assert(false, "Generated illegal LH! "+preHalfLead+"/"+halfLeadPn); None
			}
		}

		println("Generating halflead table...")
		val halfleadMap = mutable.Map[Row, Map[PN, List[PN]]]()
		for (row <- Row.generateAll(8))
		{
			val leadheadMap = mutable.Map[PN, List[PN]]()
			for (halfleadPn <- findHalfLeadPnSetForRow(row))
			{
				val leadheadPns = checkLeadHead(row, halfleadPn)
				if (leadheadPns.isDefined)
					leadheadMap(halfleadPn) = leadheadPns.get
			}
			if (!leadheadMap.isEmpty)
				halfleadMap(row) = leadheadMap.toMap
		}
		println("Done: "+halfleadMap.size+" halfleads")
		halfleadMap.toMap
	}
}