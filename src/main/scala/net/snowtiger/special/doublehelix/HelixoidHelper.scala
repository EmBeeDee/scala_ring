package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Row}

import scala.collection.mutable
import scala.io.Source

/**
 * @author Mark
 */

class HelixoidHelper(val allPN: List[PN])
{
	val NBells = 8
	val HalfLeadLength = 56
	val FullyMaskedStartRow = Row("XXX.....")

	val LHPN = PN("14")

	lazy val halfLeadTable = new HalfLeadTable()
	lazy val quarterLeadTable = new QuarterLeadTable(halfLeadTable)


	def maskFrontThree(row: Row) = Row( for (c <- row.toString) yield if (c=='1' || c=='2' || c=='3') 'X' else c )

	def maskBackFive(row: Row) = Row( for (c <- row.toString) yield if (c=='1' || c=='2' || c=='3' || c=='X') c else '.' )

	// Could be done with Perm("23145678")!
	def rotateFrontThree(row: Row) = Row( for (c <- row.toString) yield if (c=='1') '2' else if (c=='2') '3' else if (c=='3') '1' else '.' )

	def genValidHalfleadVariants(firstHalf: List[PN], lh: PN) =
	{
		List("56","36","16","58","38","18").map{ (hl)=> makeHelixoid(firstHalf++List(PN(hl)), lh) }.filter{_.firstLeadEnd.cycleSizes==List(3,5)}
	}

	def makeHelixoid(hlPN: String, lh: PN) = new Method(8, PN.parse(hlPN), lh)
	def makeHelixoid(hlPN: List[PN], lh: PN) = new Method(8, hlPN, lh)

	def streamReadHelixoidFile(file: String): Iterator[String] =
	{
		println("Reading helixoid file "+file)
		Source.fromInputStream(getClass.getResourceAsStream("/"+file)).getLines().flatMap(parseLine(_).iterator)
	}

	def readHelixoidFileWithDeDup(file: String): Iterator[String] =
	{
		println("Reading helixoid file "+file)
		val uniqueMethods = mutable.Set[String]()
		for (line <- Source.fromInputStream(getClass.getResourceAsStream("/"+file)).getLines(); parsed <- parseLine(line))
			uniqueMethods+= parsed
		uniqueMethods.iterator
	}

	private def parseLine(line: String): Option[String] =
	{
		if (!line.trim.isEmpty && !line.contains("Time taken") && !line.contains("Helixoids"))
		{
			if (line.startsWith("\""))
			{
				Some(line.stripPrefix("\"").stripSuffix("\","))
			}
			else
			{
				val split = line.split(' ')
				if (split.size==3)
					Some(split(1))
				else if (split.size>3)
				{
					// NB unlike the other file formats, this one includes the half-lead PN!
					val pn = split(1)
					if (pn.endsWith(","))
						Some(pn.substring(0, pn.length-1))
					else
						Some(pn)
				}
				else
					None
			}
		}
		else
		{
			None
		}
	}

	def isDoubleHelix(halfleadPns: List[PN]) =
	{
		def outputHelix(halfLeadPns: List[PN], leadHeadPnsForFirstHalflead: List[PN])
		{
			// Finally create a sample method using one half-lead PN and one lead-head PN
			val halfLeadPn = halfLeadPns.head
			val leadHeadPn = leadHeadPnsForFirstHalflead.head
			val m = new Method(8, halfleadPns++List(halfLeadPn)++halfleadPns.reverse++List(leadHeadPn))
			assert(m.courseLength==3*5*56*2, "Method turned out not to be double-helix! "+m.courseLength+" "+m.firstLeadEnd)
			if (!m.isTrue)
				print("FALSE ")
		}

		// First check first half-lead contains all possible combinations of places for (123)
		val rowSet = mutable.Set[Row]()
		var row = FullyMaskedStartRow
		rowSet+= row
		for (pn <- halfleadPns)
		{
			row = row.apply(pn)
			rowSet+= row
		}
		if (rowSet.size!=HalfLeadLength)
			false
		else
		{
			// Next check half-lead is good
			val preHalflead = PN.generateLastRow(Row(NBells), halfleadPns)
			halfLeadTable.getHalfleadPnMap(preHalflead) match
			{
				case None => false
				case Some(map) => map.keys.filterNot( !map(_).isEmpty ).toList match
				{
					case Nil => false
					case head::tail => outputHelix(head::tail, map(head)); true
				}
			}
		}
	}

	/** Gen TVs of the pn of the first halflead, not including the halflead itself.
	 *  Because there are so many of these, we can't generate them all, so instead require a function f which can be
	 *  called on each */
	def genTrivialVariations(pn: List[PN], f: (List[PN])=>Unit )
	{
		def genTrivialVariations(row: Row, revEarlierPns: List[PN], laterPns: List[PN])
		{
			def isAcceptablePn(pn: PN, revEarlierPns: List[PN]) = GoodPn.isAllowableConsecutivePn(pn, revEarlierPns)

			laterPns match
			{
				case Nil => f(revEarlierPns.reverse)
				case pn::tail =>
				{
					val nextRow = row.apply(pn)
					val acceptablePns = allPN.filter(isAcceptablePn(_, revEarlierPns))
					val matchingPns = acceptablePns.filter( row.apply(_)==nextRow )
					matchingPns.foreach( (varPn: PN)=> genTrivialVariations(nextRow, varPn::revEarlierPns, tail) )
				}
			}
		}

		genTrivialVariations(FullyMaskedStartRow, Nil, pn)
	}

	// Params are row, nonTvPN, nonTvNextRow, initiallyAcceptablePNlist
	type PnFilter = (Row, PN, Row, List[PN]) => List[PN]

	def genDoubleTVsFor123Expansion(pn: List[PN], f: (List[PN])=>Unit )
	{
		def filterPN(row: Row, nonTvPN: PN, nextRow: Row, pns: List[PN]) =
		{
			val uniquePns = mutable.Set[(Row,Boolean)]()
			val fullymasked = maskFrontThree(nextRow)
			def checkPN(pn: PN) =
			{
				val trialNext = row.apply(pn)
				if (fullymasked!=maskFrontThree(trialNext))
					false
				else
				{
					val pair = (trialNext, pn.swapsSign(8))
					if (uniquePns.contains(pair))
						false
					else
					{
						uniquePns.add(pair)
						true
					}
				}
			}
			pns.filter(checkPN(_))
		}
		genDoubleTrivialVariations(Row("123....."), pn.slice(0, HalfLeadLength/2-1), pn(HalfLeadLength/2-1), filterPN, f)
	}

	def genDoubleTVsFor45678Expansion(pn: List[PN], f: (List[PN])=>Unit )
	{
		def filterPN(row: Row, nonTvPN: PN, nextRow: Row, pns: List[PN]) =
		{
			def checkPN(pn: PN) =
			{
				pn==nonTvPN || (row.apply(pn)==nextRow && pn.swapsSign(8)==nonTvPN.swapsSign(8))
			}
			pns.filter(checkPN(_))
		}
		genDoubleTrivialVariations(Row("123....."), pn.slice(0, HalfLeadLength/2-1), pn(HalfLeadLength/2-1), filterPN, f)
	}

	/** Gen TVs of the pn of the first quarter-lead, not including the quarter-lead itself.
	 *  Because there are so many of these, we can't generate them all, so instead require a function f which can be
	 *  called on each.
	 *  Must pass in the start row, which determines what kind of TVs are expanded, e.g.:
	 *    XXX45678 will expand TVs differing in the positions of 1-3
	 *    123..... will expand TVs differing in the positions of 4-8
	 *    XXX..... would expand all TVs.
	 *  As an added complication, normally a change of row sign is permitted for a XXX45678 expansion, but is not
	 *  for the 123.... expansion. Hence, we also accept a "pnFilter" function controlling which PNs are allowed
	 *  for the given expansion. To match the start row type, the rules should be:
	 *    XXX45678 - any PN leaving 45678 in exactly the same places is allowed; also ONE pn is allowed which swaps
	 *    	the row sign and alters the position of 45678, whilst leaving the fully-masked row pattern XXX..... the same.
	 *    123..... - only PNs leaving 123 in exactly the same places are allowed, and then only if the row sign remains
	 *      the same.
	 */
	private def genDoubleTrivialVariations(startRow: Row, pn: List[PN], ql: PN, pnFilter: PnFilter, f: (List[PN])=>Unit )
	{
		val x = Iterator[String]("abc")
		def genDoubleTrivialVariations(row: Row, revEarlierPns: List[PN], laterPns: List[PN])
		{
			def isAcceptablePn(pn: PN, revEarlierPns: List[PN]) = GoodPn.isAllowableConsecutivePn(pn, revEarlierPns)

			laterPns match
			{
				case Nil => f( (ql::revEarlierPns).reverse ++ revEarlierPns.map{_.reverse(NBells)} )
				case pn::tail =>
				{
					val nextRow = row.apply(pn)
					val acceptablePns = allPN.filter(isAcceptablePn(_, revEarlierPns))
					val matchingPns = pnFilter(row, pn, nextRow, acceptablePns)
					matchingPns.foreach( (varPn: PN)=> genDoubleTrivialVariations(nextRow, varPn::revEarlierPns, tail) )
				}
			}
		}

		genDoubleTrivialVariations(startRow, Nil, pn)
	}

	def checkPreHalfLead(pnList: List[PN]): Option[Row] =
	{
		val preHalfLead = PN.generateLastRow(Row(NBells), pnList)
		if (halfLeadTable.getHalfleadPnMap(preHalfLead).isDefined)
			Some(preHalfLead)
		else
			None
	}

}