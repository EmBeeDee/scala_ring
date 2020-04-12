package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.composition.Stage

/**
 * @author mark
 */

object AtwBlockFinder
{
	def main(args: Array[String])
	{
		//val finder = new AtwBlockFinder(8, "cbabf")
		val finder = new AtwBlockFinder(8, "abbb")
		finder.find()
	}

}

class AtwBlockFinder(nbells: Int, lhgs: String)
{
	val stage = new Stage(nbells)
	val plainCoursePerms = stage.StageLeadheads.map{_.toPerm}.toArray
	val bobPerm12 = stage.rounds.apply(PN("12")).apply(PN("14")).toPerm
	val singlePerm12 = stage.rounds.apply(PN("12")).apply(PN("1234")).toPerm
	val bobPerm18 = stage.rounds.apply(PN("18")).apply(PN("14")).toPerm
	val n = nbells-1
	val lhOrders = lhgs.map{(lhg)=> (CourseStructure.LHOrders(lhg.toString)+n)%n}
	def makeProtoMethod(lhOrder: Int, index: Int) = ProtoMethod(('A'+index).toChar, plainCoursePerms(lhOrder))
	val methods = lhOrders.zipWithIndex.map{(p)=> makeProtoMethod(p._1, p._2)}
	val targetScore = methods.size*n*n

	def find(): Unit =
	{
		val row = stage.rounds
		val startTime = System.currentTimeMillis()
		find(new Atw(), row, "", List('X','X'), Set())
		val duration =System.currentTimeMillis()-startTime
		println("Search completed in "+duration/1000+"s")
	}

	var hiwater = 0

	def find(atw: Atw, lh: Row, touch: String, revMethods: List[Char], rows: Set[Row]): Unit =
	{
		def methodAllowed(m: ProtoMethod) = m.abbrev!=revMethods.head || m.abbrev!=revMethods.tail.head

		val score = atw.score
		if (score==targetScore)
			println("Solution: "+touch+" = "+lh)
		else
		{
			val newRows = rows+lh
			if (newRows.size==rows.size+1)
			{
				if (score>hiwater)
				{
					hiwater = score
					println("Hiwater " + hiwater+": "+touch+" = "+lh)
				}
				for (method <- methods; if methodAllowed(method))
				{
					val newAtw = atw.add(method, lh)
					if (newAtw.score-atw.score==n)
					{
						val abbrev = method.abbrev
						val plainLead = method.applyTo(lh)
						find(newAtw, plainLead, touch+" "+abbrev, abbrev::revMethods, newRows)
						val bobbedLead = plainLead.apply(bobPerm12)
						val tenorPos = bobbedLead.placeOf(nbells)
						// Allow B,M,W,H
						if (tenorPos==3 || tenorPos>=nbells-2)
							find(newAtw, bobbedLead, touch+" "+abbrev+"-", '-'::abbrev::revMethods, newRows)
						if (true)
						{
							val singledLead = plainLead.apply(singlePerm12)
							// Allow M,W,H
							if (singledLead.placeOf(nbells)>=nbells-2)
								find(newAtw, singledLead, touch+" "+abbrev+"s", 's'::abbrev::revMethods, newRows)
						}
						if (true)
						{
							val bobbedLead2 = plainLead.apply(bobPerm18)
							val tenorPos2 = bobbedLead2.placeOf(nbells)
							// Allow B,M,W,H
							if (tenorPos2==3 || tenorPos2>=nbells-2)
								find(newAtw, bobbedLead2, touch+" "+abbrev+"_", '_'::abbrev::revMethods, newRows)
						}
					}
				}
			}
		}
	}

}