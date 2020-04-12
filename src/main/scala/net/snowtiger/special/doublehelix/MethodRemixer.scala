package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Perm, Row}


/**
 * @author Mark
 */

object MethodRemixer extends HelixoidHelper(GoodPn(8).oneConsec)
{
	val pbPL = "-18-18-18-18-18-18-18-12"
	val pbBL = "-18-18-18-18-18-18-18-14"
	val pbSL = "-18-18-18-18-18-18-18-1234"
	val pbXL = "-18-18-18-18-18-18-18-1278"
	val pb8WeirdThree = new Method(8, pbPL*2+pbXL+ pbPL+pbXL+pbPL*6+pbXL+pbPL*4+ pbXL+pbPL*4)
	val pb8ThreeHomes = new Method(8, pbPL*6+pbBL)
	val pb8ThreeMiddles = new Method(8, pbPL*5+pbBL+pbPL)
	val pb8PlainCourse = new Method(8, pbPL)

	val dncbPlainCourse = new Method(8, "-14-36-58-18-58-36-14-18")
	val dncbThreeV = new Method(8, "-14-36-58-18-58-36-14-18"*3+"-14-36-58-18-58-36-14-16"+"-14-36-58-18-58-36-14-18"*3)

	val helixoids = streamReadHelixoidFile("14Congruent_Helixoids_double.txt")
	//val helixoids = streamReadHelixoidFile("14Congruent_Helixoids_double_x_18_14_58_36.txt")

	// Number of changes in half lead.
	val HelixoidSize = 56

	abstract class SpliceRemix
	{
		val name: String
		def remixRows(helixoid: Method): Set[Row]
	}

	class LeadSplice extends SpliceRemix
	{
		val name = "Lead splice"
		def remixRows(helixoid: Method) = PN.generateRows(Row(8), helixoid.lead).toSet
	}

	class CourseSplice extends SpliceRemix
	{
		val name = "Course splice"
		def remixRows(helixoid: Method) = helixoid.fullCourse.toSet
	}

	def genAllFrontThreeRotations(rows: Set[Row]) =
	{
		val rot1 = rows.map(rotateFrontThree(_))
		rows++rot1++rot1.map(rotateFrontThree(_))
	}

	class Klein4Splice extends SpliceRemix
	{
		val doublepairswap1 = Perm("12367458")
		val doublepairswap2 = Perm("12376548")
		val doublepairswap3 = Perm("12354768")

		val name = "Klein-Four splice: "+doublepairswap1+" "+doublepairswap2+" "+doublepairswap3
		def remixRows(helixoid: Method) =
		{
			val lead1 = helixoid.fullCourse.slice(0, HelixoidSize*6).toSet
			val lead2 = lead1.map( _.permuteBy(doublepairswap1) )
			val lead3 = lead1.map( _.permuteBy(doublepairswap2) )
			val lead4 = lead1.map( _.permuteBy(doublepairswap3) )
			val remixRowsLead = lead1++lead2++lead3++lead4
			remixRowsLead
			//genAllFrontThreeRotations(remixRowsLead)
		}
	}

	class Cycle3Splice extends SpliceRemix
	{
		val cycle3 = Perm("12356478")

		val name = "3-cycle splice: "+cycle3
		def remixRows(helixoid: Method) =
		{
			val lead1 = helixoid.fullCourse.slice(0, HelixoidSize*2).toSet
			val lead2 = lead1.map( _.permuteBy(cycle3) )
			val lead3 = lead2.map( _.permuteBy(cycle3) )
			val remixRowsLead = lead1++lead2++lead3
			genAllFrontThreeRotations(remixRowsLead)
		}
	}

	var max = 0;

	def main(args: Array[String])
	{
		val splice = new CourseSplice
		println("Remixes using "+splice.name)

		if (false)
		{
			// Deal with input files which includes half-lead PN as well as those who don't
			def genH(pn: List[PN]): List[Method] =
			{
				if (pn.size==HalfLeadLength)
					List(makeHelixoid(pn, LHPN))
				else
					genValidHalfleadVariants(pn, LHPN)
			}
			helixoids.flatMap{ (pn)=>genH(PN.parseToList(pn)).iterator }.foreach{remixHelixoid(_, splice)}
		}
		else
		{
			// Special search for Double Helixoids
			println("(Special Double-Helixoid remix)")
			def processTV(pn: List[PN])
			{
				genValidHalfleadVariants(pn, LHPN).foreach{ remixHelixoid(_, splice) }
			}
			for (pn <- helixoids)
			// Only mask out 4-8, since the 1-3 TVs have already been expanded in the congruency search
				genDoubleTVsFor45678Expansion(PN.parseToList(pn), processTV)
		}
		//println("Finished: max course length was "+max)
	}

	def remix(row: Row, allRows: Set[Row], revPns: List[PN])
	{
		val remixRows = pb8PlainCourse.fullCourse.toSet

		def output(finalRow: Row, pns: List[PN])
		{
			println(finalRow+" "+PN.output(pns))
		}

		def acceptablePn(pn: PN, nextRow: Row) =
			remixRows.contains(nextRow) && !allRows.contains(nextRow) && (GoodPn.isAllowableConsecutivePn(pn, revPns))

		if (revPns.size+1>max)
		{
			max = revPns.size+1
			println("New max: "+max)
		}
		if (revPns.size+1==remixRows.size)
			for (pn <- allPN; nextRow=row.apply(pn); if (acceptablePn(pn, nextRow)))
				output(nextRow, (pn::revPns).reverse)
		else
			for (pn <- allPN; nextRow=row.apply(pn); if (!nextRow.isRounds && acceptablePn(pn, nextRow)))
				remix(nextRow, allRows+nextRow, pn::revPns )
	}


	def remixHelixoid(helixoid: Method, splice: SpliceRemix)
	{
		var titleOutputted = false
		val remixRows = splice.remixRows(helixoid)

		val allowedBack5LeadHeads = for (i <- 0 to HelixoidSize*8 by HelixoidSize*2) yield HelixoidFinder.maskFrontThree(helixoid.fullCourse(i))

		def methodDiff(pns: List[PN]) =
		{
			val originalPns = helixoid.lead.slice(0, HalfLeadLength-1)
			pns.zip(originalPns).filter{ (pair)=>pair._1!=pair._2 }.size
		}

		def outputHelixoid(finalRow: Row, pns: List[PN])
		{
			val validMethods = genValidHalfleadVariants(pns, LHPN).filter{ (m)=>allowedBack5LeadHeads.contains(HelixoidFinder.maskFrontThree(m.firstLeadEnd)) }
			for (m<-validMethods)
			{
				val wrongPlace = m.wrongPlaceFactor
				val basicOutput = m+" "+wrongPlace.formatted("%.2f")
				val starredOutput = basicOutput+(if (wrongPlace>0.3) " ***" else "")
				val diff = methodDiff(pns)
				if (diff>10)
				{
					val diffOutput = starredOutput+" diff = "+diff+(if (diff > 2*(HalfLeadLength/4)) " !!!" else "")
					if (!titleOutputted)
					{
						println("Remixing "+helixoid)
						titleOutputted = true
					}
					println(diffOutput)
				}
			}
		}

		remixHelixoid(Row(8), Nil, Set(FullyMaskedStartRow))
		if (titleOutputted)
			println()

		def remixHelixoid(row: Row, revPns: List[PN], maskedRowSet: Set[Row])
		{
			def acceptablePn(pn: PN, nextRow: Row) =
				remixRows.contains(nextRow) && GoodPn.isAllowableConsecutivePn(pn, revPns)

			if (revPns.size+1==HelixoidSize)
				outputHelixoid(row, revPns.reverse)
			else
			{
				for (pn <- allPN; nextRow=row.apply(pn); if (acceptablePn(pn, nextRow)))
				{
					val maskedRow = HelixoidFinder.maskBackFive(HelixoidFinder.maskFrontThree(nextRow))
					if (!maskedRowSet.contains(maskedRow))
						remixHelixoid(nextRow, pn::revPns, maskedRowSet+maskedRow )

				}
			}
		}
	}

}