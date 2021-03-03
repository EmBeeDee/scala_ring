package net.snowtiger.special

import net.snowtiger.ringing.{Perm, Row}

import scala.collection.mutable

/**
 * @author mark
 */

object LittleGrandsire
{
	val perms = Map('p'-> (Perm("162483957"),Perm("132547698")), '-'->(Perm("164325879"),Perm("132547698")), 's'->(Perm("164325879"),Perm("123547698")))

	val coPerm = Perm("123468975")

	val goodLBs = Set("2453", "3542", "4235", "5324", "3564", "4653", "5346", "6435", "6457", "7546")
	val allowedCallingPos9 = Set(3,5,7)

	val musicMap = mutable.Map[Row,Int]()

	val MAX = 80

	// Tittums search
	/*
	val start = Row("146728395")
	//val start = Row("126748395")
	//val end = Row("123456978")
	val end = Row("124365978")
	val avoid = Set()
	val startLeadNum = 5
	*/

	// Handstroke Home search
	///*
	val start = Row("128976543")
	//val start = Row("128975634")
	//val start = Row("148975623")
	val end = Row("136492857")	// Rounds at hand in this row (converted to Tittums)
	val avoid = Set(Row("123456798"), Row("134256798"), Row("124356798"), Row("143256798"), Row("132456798"),
		Row("157849362"), Row("124537689"), Row("129683745"), Row("136948275"), Row("149683725"))
	val startLeadNum = 3
	//*/

	val goodFactor = 1.20
	val keepFactor = 1.20

	def main(args: Array[String]): Unit =
	{
		search(start, Nil, Set(start)++avoid, 0, 0, startLeadNum)
	}

	val debug = false

	// Note we must always search in the Tittums position!
	def search(lh: Row, revComp: List[Char], allLeads: Set[Row], music: Int, length: Int, leadNum: Int): Unit =
	{
		def searchWith(call: Char, newLeadNum: Int): Unit =
		{
			val lelhPerms = perms(call)
			val newLE = lh(lelhPerms._1)
			val newLH = newLE(lelhPerms._2)
			val newLeads = allLeads+newLE+newLH
			if (newLeads.size == allLeads.size+2)
			{
				if (debug)
					println("   "+call+" "+newLeadNum+" "+newLH)
				val m = musicMap.getOrElseUpdate(newLH, calcMusic(newLH))
				val newComp = call::(if (newLeadNum==1) ' '::revComp else revComp)
				search(newLH, newComp, newLeads, music+m, length+1, newLeadNum)
			}
			else if (debug)
				println("False")
		}

		if (leadNum<9 && music+2>length*goodFactor)
		{
			if (lh==end)
				output(lh, revComp, music, length)
			else if (length < MAX)
			{
				val b9 = lh.placeOf(9)
				val newLeadNum = if (b9==7) 1 else leadNum+1
				if (allowedCallingPos9(b9))
				{
					searchWith('-', newLeadNum)
					searchWith('s', newLeadNum)
				}
				searchWith('p', newLeadNum)
			}
		}
		else if (debug)
			println("Backtrack")
	}

	def output(lh: Row, revComp: List[Char], music: Int, length: Int): Unit =
	{
		if (music>length*keepFactor)
		{
			val musicPerLead = music.toDouble/length
			val comp = revComp.reverse.mkString
			println(f"$music%d $length%d $musicPerLead%.3f $lh%s $comp%s")
		}
	}

	def calcMusic(lh: Row): Int =
	{
		var score = 0
		val co = lh(coPerm).toString.tail
		val back3 = co.indexOf("789")
		if (back3>=0)
		{
			score+= 1
			if (back3>0 && back3<5)
			{
				val b56 = ""+co(back3-1)+co(back3+3)
				if (b56=="56" || b56=="65")
					score+= 1
			}
		}
		val lbs = Set(co.slice(0,4), co.slice(3,7), co.slice(4,8))
		if (lbs.exists(goodLBs))
			score+= 1
		/*
		if (score>1)
			println("** "+score+" "+lh+" "+co)
		*/
		score
	}
}