package net.snowtiger.doublehelix

import java.io.PrintWriter

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{PN, Row}

import scala.Predef._
import scala.collection.mutable

/**
 * @author Mark
 */

object HelixoidFinder extends HelixoidHelper(GoodPn(8).noConsec)
{
	var count = 0

	def main(args: Array[String])
	{
		/*
		val out = if (args.length==1) Some(new PrintWriter(args(0))) else None
		findMethods(out)
		if (out.isDefined)
			out.get.close()
		*/
		findCongruentMethods(new DoubleB(), false)
	}

	def outputMethods(row: Row, revPnList: List[PN], out: Option[PrintWriter])
	{
		if (row.toString.contains("XX"))
		{
			val pn = revPnList.reverse
			checkPreHalfLead(pn) match
			{
				case Some(preHalfLead) =>
				{
					//val log = preHalfLead+" "+PN.output(pn)+" ("+halfLeadTable.getHalfleadPnMap(preHalfLead).get.keySet.mkString(",")+")"
					val log = "\""+PN.output(pn)+"\","
					println(log)
					if (out.isDefined)
						out.get.println(log)
					count+= 1
				}
				case _ =>
			}
		}
	}

	/**
	 * Search
	 * @param out
	 */
	def findMethods(out: Option[PrintWriter])
	{
		/**
		 * rowMap expected to contain row.
		 */
		def rightPlaceSearch(row: Row, revPnList: List[PN], rowSet: Set[Row])
		{
			if (revPnList.size==HalfLeadLength-1)
				outputMethods(row, revPnList, out)
			else
			{
				// Keep track of the changes produced by each PN - ignore PNs that produce the same result pattern
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn <- allPN)
					if (revPnList.isEmpty || GoodPn.isRightPlaceConsecutivePn(pn, revPnList.head))
					{
						val newRow = row.apply(pn)
						if ((pn.isCross || !pnResultsForThisChange.contains(newRow)) && !rowSet.contains(newRow))
						{
							rightPlaceSearch(newRow, pn::revPnList, rowSet+newRow)
							if (!pn.isCross)
								pnResultsForThisChange+= newRow
						}
					}
			}
		}

		/**
		 * rowMap expected to contain row.
		 */
		def wrongPlaceSearch(row: Row, revPnList: List[PN], rowSet: Set[Row])
		{
			if (revPnList.size==HalfLeadLength-1)
				outputMethods(row, revPnList, out)
			else
			{
				// Keep track of the changes produced by each PN - ignore PNs that produce the same result pattern
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn <- allPN)
					if (GoodPn.isAllowableConsecutivePn(pn, revPnList))
					{
						val newRow = row.apply(pn)
						if (!pnResultsForThisChange.contains(newRow) && !rowSet.contains(newRow))
						{
							wrongPlaceSearch(newRow, pn::revPnList, rowSet+newRow)
							pnResultsForThisChange+= newRow
						}
					}
			}
		}

		def outputDoubleMethods(revPnList: List[PN])
		{
			val pnList = revPnList.reverse
			val preQuarterLead = PN.generateLastRow(Row(NBells), pnList)
			if (quarterLeadTable.isValid(preQuarterLead))
			{
				val validQuarterLeads = quarterLeadTable.getValidQuarterLeads(preQuarterLead).filter( (pnAndRow)=>GoodPn.isAllowableConsecutivePn(revPnList.head, pnAndRow._1) )
				for ((quarterLeadPn, preHalfLead) <- validQuarterLeads)
				{
					val pn = (quarterLeadPn::revPnList).reverse ++ revPnList.map( _.reverse(NBells) )
					if (halfLeadTable.getHalfleadPnMap(preHalfLead).isDefined)
						if (isDoubleHelix(pn))
						{
							val log = preHalfLead+" "+PN.output(pn)+" ("+halfLeadTable.getHalfleadPnMap(preHalfLead).get.keySet.mkString(",")+")"
							println(log)
							if (out.isDefined)
								out.get.println(log)
							count+= 1
						}
				}
			}
		}

		def doubleMethodSearch(row: Row, revPnList: List[PN], rowSet: Set[Row])
		{
			if (revPnList.size==HalfLeadLength/2-1)
				outputDoubleMethods(revPnList)
			else
			{
				// Keep track of the changes produced by each PN - ignore PNs that produce the same result pattern
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn <- allPN)
					if (GoodPn.isAllowableConsecutivePn(pn, revPnList))
					{
						val newRow = row.apply(pn)
						if (!pnResultsForThisChange.contains(newRow) && !rowSet.contains(newRow))
						{
							doubleMethodSearch(newRow, pn::revPnList, rowSet+newRow+newRow.reverse)
							pnResultsForThisChange+= newRow
						}
					}
			}
		}
	}

	abstract class BaseMethod
	{
		val name: String
		val plainLead: String
		val bobLead: String
		val singleLead: String
		val touches: List[String]
	}

	class PB1278Singles extends BaseMethod
	{
		val name = "PB (1278 singles)"
		val plainLead = MethodRemixer.pbPL
		val bobLead = MethodRemixer.pbBL
		val singleLead = MethodRemixer.pbXL
		val touches = List(
			"PP-PSSPPPPP-SPPPPPSPP",
			"PP--SSPPPPP-SPPPPPSPP",
			"PP--SSPPPPP-SPPPP-SPP",
			"PPSPPPPPS-PPPPPSSP-PP",
			"PPSPPPPPS-PPPPPSS--PP",
			"PPS-PPPPS-PPPPPSS--PP",
			"PSPPSPPPPSPPPPPPS-SSP",
			"PSPPSPPPPS-PPPPPSPSSP",
			"PSPPSPPPPS-PPPPPS-SSP",
			"PSPPSSPPPPPPSPPPP-SSP",
			"PSP-SPPPPSPPPPPPSPSSP",
			"PSP-SPPPPSPPPPPPS-SSP",
			"PSP-SPPPPS-PPPPPSPSSP",
			"PSP-SSPPPPPPSPPPPPSSP",
			"PSP-SSPPPPPPSPPPP-SSP",
			"PSP-SSPPPPP-SPPPPPSSP",
			"PS-PSPPPPSPPPPPPS-SSP",
			"PS-PSPPPPS-PPPPPSPSSP",
			"PS-PSPPPPS-PPPPPS-SSP",
			"PS-PSSPPPPPPSPPPP-SSP",
			"PS--SPPPPSPPPPPPSPSSP",
			"PS--SPPPPSPPPPPPS-SSP",
			"PS--SPPPPS-PPPPPSPSSP",
			"PS--SSPPPPPPSPPPPPSSP",
			"PS--SSPPPPPPSPPPP-SSP",
			"PS--SSPPPPP-SPPPPPSSP",
			"PSSPPPPPSPPPPPPSS-PSP",
			"PSSPPPPPSPPPPPPSS--SP",
			"PSSPPPPPS-PPPPPSS-PSP",
			"PSSPPPPPS-PPPPPSS--SP",
			"PSSPSPPPPPPSPPPPS-PSP",
			"PSSPSPPPPPPSPPPPS--SP",
			"PSSPSPPPPP-SPPPPSPPSP",
			"PSSPSPPPPP-SPPPPSP-SP",
			"PSSPSPPPPP-SPPPPS-PSP",
			"PSSPSPPPPP-SPPPPS--SP",
			"PSS-PPPPSPPPPPPSSPPSP",
			"PSS-PPPPSPPPPPPSSP-SP",
			"PSS-PPPPSPPPPPPSS-PSP",
			"PSS-PPPPSPPPPPPSS--SP",
			"PSS-SPPPPPPSPPPPSPPSP",
			"PSS-SPPPPPPSPPPPSP-SP",
			"PSS-SPPPPPPSPPPPS-PSP",
			"PSS-SPPPPPPSPPPPS--SP",
			"PSS-SPPPPP-SPPPPSPPSP",
			"PSS-SPPPPP-SPPPPSP-SP"	)
	}

	class DNCB extends BaseMethod
	{
		val name = "DNCB, standard 6th's place calls"
		val plainLead = "-14-36-58-18-58-36-14-18"
		val bobLead = "-14-36-58-18-58-36-14-16"
		val singleLead = "-14-36-58-18-58-36-14-1678"
		val touches = List()
	}

	class DoubleB extends BaseMethod
	{
		val name = "Double Bob (1278 singles)"
		val plainLead = "-18-18-18-78-18-18-18-12"
		val bobLead = "-18-18-18-78-18-18-18-14"
		val singleLead = "-18-18-18-78-18-18-18-1278"
		val touches = List(
			"PPPP-SPPPSPP-PPPSPPSP",
			"PPPP-SPP-SPP-PPPSPPSP",
			"PPPPSPPSPPP-PPSPPPS-P",
			"PPPPSPPSPPP-PPS-PPS-P",
			"P-PP-SPP-SPP-PPPSPPSP",
			"P-PPSPPSPPP-PPS-PPS-P",
			"P-PPSSPP-PPPSPP-PPSPP")
	}

	def findCongruentMethods(base: BaseMethod, wrongPlace: Boolean)
	{
		/** Map of masked changes in PB (e.g. 123XXXXX) to their nature (+ve/-ve) */
		val rowSigns = mutable.Map[Row, Boolean]()

		/** Map of rows -> Map of PN -> (subsequent row, masked subsequent row) */
		val nodes = mutable.Map[Row, mutable.Map[PN, (Row,Row)]]()
		def makeSubsequentPair(row: Row, pn: PN) =
		{
			val nextRow = row.apply(pn)
			(nextRow, maskFrontThree(nextRow))
		}

		/**
		 * rowSet expected to contain row, which must have back five masked with . but front three NOT masked!
		 */
		def rightPlaceCongruentSearch(row: Row, nature: Boolean, revPnList: List[PN], rowSet: Set[Row])
		{
			if (revPnList.size==HalfLeadLength-1)
				outputMethods(maskFrontThree(row), revPnList, None)
			else
			{
				// Keep track of the changes produced by each PN - ignore PNs that produce the same result pattern
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn <- allPN)
					if (revPnList.isEmpty || GoodPn.isRightPlaceConsecutivePn(pn, revPnList.head))
					{
						val pnMap = nodes.getOrElseUpdate(row, mutable.Map[PN,(Row,Row)]() )
						val (newRow, newMaskedRow) = pnMap.getOrElseUpdate(pn, makeSubsequentPair(row, pn))
						if (!pnResultsForThisChange.contains(newRow) && !rowSet.contains(newMaskedRow))
						{
							val newNature = if (pn.swapsSign(8)) !nature else nature
							if (newNature==rowSigns(newRow))
							{
								rightPlaceCongruentSearch(newRow, newNature, pn::revPnList, rowSet+newMaskedRow)
								if (!pn.isCross)
									pnResultsForThisChange+= newRow
							}
						}
					}
			}
		}

		/**
		 * rowSet expected to contain row, which must have back five masked with . but front three NOT masked!
		 */
		def doubleWrongPlaceCongruentSearch(row: Row, nature: Boolean, revPnList: List[PN], rowSet: Set[Row])
		{
			def allowedPnList(rowNumber: Int) =
			{
				if (rowNumber==1)
					allPN
				else if (rowNumber < HalfLeadLength/2)
					allPN.filter{ GoodPn.isAllowableConsecutivePn(_, revPnList.head) }
				else if (rowNumber == HalfLeadLength/2)
					allPN.filter{ (pn)=> pn==pn.reverse(8) }
				else
					List(revPnList(rowNumber*2-HalfLeadLength-1))
			}

			if (revPnList.size==HalfLeadLength-1)
				outputMethods(maskFrontThree(row), revPnList, None)
			else
			{
				// Keep track of the changes produced by each PN - ignore PNs that produce the same result pattern
				val pnResultsForThisChange = mutable.Set[Row]()
				for (pn <- allowedPnList(revPnList.size+1))
				{
					val pnMap = nodes.getOrElseUpdate(row, mutable.Map[PN,(Row,Row)]() )
					val (newRow, newMaskedRow) = pnMap.getOrElseUpdate(pn, makeSubsequentPair(row, pn))
					if (!pnResultsForThisChange.contains(newRow) && !rowSet.contains(newMaskedRow))
					{
						val newNature = if (pn.swapsSign(8)) !nature else nature
						if (newNature==rowSigns(newRow))
						{
							doubleWrongPlaceCongruentSearch(newRow, newNature, pn::revPnList, rowSet+newMaskedRow)
							pnResultsForThisChange+= newRow
						}
					}
				}
			}
		}

		println("Finding Helixoids congruent with "+base.name)

		for (touchStr <- base.touches)
		{
			val touchPn = touchStr.flatMap( _ match {
				case 'P' => base.plainLead
				case '-' => base.bobLead
				case 'S' => base.singleLead
				case u => throw new Exception("Unknown touch char "+u)
			} )
			val touch = PN.generateRows(Row(8), PN.parse(touchPn))
			rowSigns.clear()

			try
			{
				if (false)
				{
					// Fast way
					rowSigns++= touch.map{(r:Row)=>(maskBackFive(r), r.positive)}
				}
				else
				{
					// Slow way - add all rows in touch to the rowSigns map, checking consistency as we go
					var rowN = 0
					for (r <- touch)
					{
						rowN+= 1
						val masked = maskBackFive(r)
						if (rowSigns.contains(masked))
						{
							throw new RuntimeException("Touch is not congruent: row no."+rowN+" "+r+" in masked form "+masked+" occurs twice!")
						}
						else
						{
							rowSigns.put(masked, r.positive)
							val rot1 = rotateFrontThree(masked)
							val rot2 = rotateFrontThree(rot1)
							if (rowSigns.contains(rot1) && rowSigns(rot1)!=r.positive)
								throw new RuntimeException("Touch is not congruent: row no."+rowN+" "+r+" has "+r.signChar+" sign, but the touch also has row "+rot1+" of opposite sign.")
							if (rowSigns.contains(rot2) && rowSigns(rot2)!=r.positive)
								throw new RuntimeException("Touch is not congruent: row no."+rowN+" "+r+" has "+r.signChar+" sign, but the touch also has row "+rot2+" of opposite sign.")
						}
					}
				}
				val t = System.currentTimeMillis()
				println()
				if (wrongPlace)
				{
					println("Double wrong-place Helixoids congruent against touch "+touchStr)
					doubleWrongPlaceCongruentSearch(Row("123....."), true, List(), Set(FullyMaskedStartRow))
				}
				else
				{
					println("Right-place Helixoids congruent against touch "+touchStr)
					rightPlaceCongruentSearch(Row("123....."), true, List(), Set(FullyMaskedStartRow))
				}
				println("Time taken = "+(System.currentTimeMillis()-t))
			}
			catch
			{
				case e: RuntimeException => println("Bad touch: "+e)
			}
		}

		//val realDoubleHelix = List("x","38","16","x","36","14","38","x","14","38","14","58","36","x","38","14","38","x","16","38","x","16","38","16","38","14","x").map(PN(_))
		//outputDoubleMethods(realDoubleHelix.reverse)

		//rightPlaceSearch(StartRow, List(), Set(StartRow))
		//doubleMethodSearch(StartRow, List(), Set(StartRow, StartRow.reverse))

		println("All done - "+count+" found.")
	}
}
