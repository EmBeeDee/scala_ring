package net.snowtiger.doublehelix

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Method, PN, Perm, Row}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * @author Mark
 */

object CongruentPlainComposer extends HelixoidHelper(GoodPn(8).oneConsec)
{
	abstract class MethodAndCalls
	{
		val name: String;
		val methodPn: List[PN]
		val calls: List[(String,PN)]
	}

	class PB1278 extends MethodAndCalls
	{
		val name = "PB with 1278 singles"
		val methodPn = PN.parseToList("-18-18-18-18-18-18-18-")
		val calls = List( "P"->PN("12"), "-"->PN("14"), "S"->PN("1278"))
	}

	class PB extends MethodAndCalls
	{
		val name = "PB with standard calls"
		val methodPn = PN.parseToList("-18-18-18-18-18-18-18-")
		val calls = List( "P"->PN("12"), "-"->PN("14"), "s"->PN("1234"))
	}

	class PBSpecial extends MethodAndCalls
	{
		val name = "PB with 123456 single"
		val methodPn = PN.parseToList("-18-18-18-18-18-18-18-")
		val calls = List( "P"->PN("12"), "x"->PN("123456"), "-"->PN("14"))
	}

	class RevB extends MethodAndCalls
	{
		val name = "Reverse Bob with standard 6th's place calls"
		val methodPn = PN.parseToList("-18-18-18-78-18-18-18-")
		val calls = List( "P"->PN("18"), "-"->PN("16"), "S"->PN("1678"))
	}

	class DB1278 extends RevB
	{
		override val name = "Double Bob with 1278 single"
		override val calls = List( "P"->PN("12"), "-"->PN("14"), "S"->PN("1278"))
	}

	class DB123456 extends RevB
	{
		override val name = "Double Bob with 123456 bingle"
		//override val calls = List( "P"->PN("12"), "x"->PN("123456"), "s"->PN("1256"))
		override val calls = List( "P"->PN("12"), "-"->PN("14"), "s"->PN("1256"))
	}

	class DNCB extends MethodAndCalls
	{
		val name = "DNCB with standard 6th's place calls"
		val methodPn = PN.parseToList("-14-36-58-18-58-36-14-")
		val calls = List( "P"->PN("18"), "-"->PN("16"), "S"->PN("1234"))
	}

	val method = new PB()

	def main(args: Array[String])
	{
		/*
		checkSigns(MethodRemixer.pb8WeirdThree.fullCourse)
		checkSigns(MethodRemixer.pb8ThreeMiddles.fullCourse)
		*/

		println("Searching for Helixoid-congruent touches of "+method.name)
		search(Row(8), Map(), Nil)

		/*
		val testHelix = new Method(8, PN.parse("-16.58-58-18-58-18-18-14-18-58-18-58-14.58-36-14.58-14-18-14-18-58-18-18-14-18-14-14.38-36"), PN("16"))
		checkHelixoidIsCongruentWithPB(testHelix)
		 */

		//println("Linking courses with "+method.name)
		//linkThreeWholeCourses(List("123.....", "12...3..", "12....3."))
	}

	def linkThreeWholeCourses(positiveCHs: List[String])
	{
		assert(positiveCHs.size==3)
		assert(positiveCHs.forall( _.startsWith("12")))

		val plainPN = method.calls.filter(_._1=="P").map(_._2).head
		val handstrokeLead = new Method(8, method.methodPn).firstLeadEnd
		val handstrokePerm = Perm(handstrokeLead.toString)
		val plainPerm = Perm(handstrokeLead.apply(plainPN).toString)

		def genLeads(nLeft: Int, lhsGenned: List[Row]): List[Row] =
		{
			if (nLeft==0)
				lhsGenned
			else
				genLeads(nLeft-1, lhsGenned.head.apply(plainPerm) :: lhsGenned )
		}
		val desiredPositiveLHs = positiveCHs.flatMap( (r)=> genLeads(6, List(Row(r))))

		def search(lh: Row, used: Set[Row], touch: List[String])
		{
			if (touch.size==3*7)
			{
				if (lh.cycleSizes==List(1,1,1,5))
					println(lh+" "+touch.reverse.mkString)
			}
			else
			{
				val masked = maskBackFive(lh)
				val positiveMasked = if (lh.positive) masked else masked.apply(handstrokePerm)
				if (desiredPositiveLHs.contains(positiveMasked) && !used.contains(positiveMasked))
				{
					val newUsed = used+positiveMasked
					for (call <- method.calls)
						search(lh.apply(handstrokePerm).apply(call._2), newUsed, call._1::touch)
				}
			}
		}

		search(Row(8), Set(), Nil)

	}

	def checkHelixoidIsCongruentWithPB(helixoid: Method)
	{
		println("Checking congruent plain comp for "+helixoid)
		val treblePositions = CongruentHelixoidProcessor.partitionTreblePositions(helixoid)
		val lhs = CongruentHelixoidProcessor.partitionRowsBySign(treblePositions(0))
		if (CongruentHelixoidProcessor.linkPlainHunts(lhs, List(PN("18"))))
		{
			val (positive, negative) = CongruentHelixoidProcessor.partitionRowsBySign(treblePositions(0))
			for (ch <- CongruentHelixoidProcessor.extractCourseHeads(positive, 2, 2))
				println(" "+ch)
		}
		else
		{
			println("NOT CONGRUENT!")
		}
	}

	def search(row: Row, signMap: Map[Row, Set[(Row,Boolean)]], touch: List[String])
	{
		if (touch.size==3*7)
		{
			if (row.isRounds || row.cycleSizes==List(1,1,1,5))
				println(row+" "+touch.reverse.mkString)
		}
		else
		{
			val buf = ListBuffer[Row]()
			val leadend = PN.generateChanges(row, method.methodPn, buf)
			buf += leadend
			if (!buf.exists{ badRow(_, signMap) })
			{
				val signMapBuilder = mutable.Map()++signMap
				for (r <- buf)
				{
					val maskedBack5 = maskBackFive(r)
					val maskedRow = maskFrontThree(maskedBack5)
					val matchSet = signMapBuilder.getOrElse(maskedRow, Set[(Row,Boolean)]())
					signMapBuilder.put(maskedRow, matchSet + Tuple2(maskedBack5,r.positive))
				}
				val newSignMap = signMapBuilder.toMap
				for (call <- method.calls)
					search(leadend.apply(call._2), newSignMap, call._1::touch)
			}
		}
	}

	def badRow(row: Row, signMap: Map[Row, Set[(Row,Boolean)]]) =
	{
		val maskedBack5 = maskBackFive(row)
		val maskedRow = maskFrontThree(maskedBack5)
		val matchSet = signMap.getOrElse(maskedRow, Set[(Row,Boolean)]())

		def badSign(prev: (Row, Boolean)) =
		{
			// Row is bad if we've had it already, or...
			val ret = maskedBack5==prev._1 ||
					{
						// It is a (123)-rotation and it has a different sign, or...
						if (maskedBack5.bellsInSamePlace(prev._1).isEmpty)
							row.positive!=prev._2
						// If it is a (123) pair-swap and has the same sign.
						else
							row.positive==prev._2
					}
			ret
		}

		matchSet.exists(badSign)
	}

	def checkSigns(rows: List[Row])
	{
		/*
		for (r <- rows)
		{
			val masked = maskBackFive(r)
			val rot1 = rotateFrontThree(masked)
			val rot2 = rotateFrontThree(rot1)
			//println(masked+" "+r.signChar+" "+rot1+" "+rot2)
			println(masked+" "+r.signChar+" "+r)
		}
		*/

		// Map of completely masked rows XXX..... -> set of unmasked rows found with this mask
		val signMap = mutable.Map[Row, Set[Row]]()
		var c = 0;
		for (row <- rows)
		{
			val maskedBack5 = maskBackFive(row)
			val maskedRow = maskFrontThree(maskedBack5)
			val matchSet = signMap.getOrElse(maskedRow, Set[Row]())
			if (matchSet.map(maskBackFive(_)).contains(maskedBack5))
			{
				println("Duplicate row: "+row)
			}
			else
			{
				for (prevRow <- matchSet)
				{
					val prevMasked = maskBackFive(prevRow)
					if (maskedBack5.bellsInSamePlace(prevMasked).size==0)
					{
						if (prevRow.positive != row.positive)
						{
							c+= 1
							println(" "+maskedBack5+row.signChar+" "+prevMasked+prevRow.signChar)
						}
					}
				}
				signMap.put(maskedRow, matchSet+row)
			}
		}
		println("Found "+c+" incompatible rows out of "+rows.size+" total.")
	}
}