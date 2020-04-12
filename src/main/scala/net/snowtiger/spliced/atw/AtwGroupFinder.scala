package net.snowtiger.spliced.atw

import net.snowtiger.ringing.{PN, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * Currently set up to investigate whether an odd call within one course of an ATW set can still generate ATW
 * with the front bells. Answer appears to be NO.
 * @author mark
 */

object AtwGroupFinder extends AtwHelper
{
	def main(args: Array[String])
	{
		findATW(8)
	}

	var coGroupsFound: Set[Set[Set[CoursingOrder]]] = Set()

	def findATW(nbells: Int)
	{
		//val courseheads = genAllCourseHeads(nbells).toSet
		val courseheads = genPositiveCourseHeads(nbells).toSet
		val stage = new Stage(nbells)
		val plainCoursePerms = stage.StageLeadheads.map{_.toPerm}.toArray
		val permAGroup = plainCoursePerms(1)
		val permBob = stage.callPerm(PN("14"))

		var cosFound: Set[Set[CoursingOrder]] = Set()
		val protoMethods = ('A' to 'G').map{new ProtoMethod(_, permAGroup)}

		def find(atw: Atw, rotation: Int, rotStarts: List[Row], chsLeft: Set[Row])
		{
			if (rotation>=plainCoursePerms.size)
				outputAtwCourses(rotStarts.reverse)
			else
			{
				// number of working bells * number of leads in course * number of courses done
				val requiredAtwScore = (nbells-1) * (nbells-1) * (rotation+1)
				for (ch <- chsLeft)
				{
					var startLH = ch.apply(plainCoursePerms(rotation))
					val lhs = for (i <- 1 until nbells) yield {val lh=startLH; startLH=startLH.apply(permAGroup); lh}
					var newAtw = atw
					for (leadAtw <- protoMethods.zip(lhs))
						newAtw = newAtw.add(leadAtw._1, leadAtw._2)
					if (newAtw.score==requiredAtwScore)
						find(newAtw, rotation+1, startLH::rotStarts, chsLeft-ch)
				}
			}
		}

		/*
		def outputAllLeads(rotStarts: List[Row])
		{
			for (startLH <- rotStarts)
			{
				for ( (row, method) <- course.genLHsFrom(startLH).zip(course.methods) )
					println(row+" "+method)
				println()
			}
		}
		*/

		def outputAtwCourses(rotStarts: List[Row])
		{
			val coSet = rotStarts.map{(lh)=> CoursingOrder(lh.coursingOrder(permAGroup))}.toSet
			if (!cosFound.contains(coSet))
			{
				// outputAllLeads()
				var groupedCos = relatedCOSets(coSet)
				val rels = groupedCos.map{ relationship(_) }
				// Output courses where the groups of coursing orders are all related within themselves by W, M or H, with one lone CO allowed
				if (true) //rels.count(_=="x")==1 && rels.count(_=="-")<=1)
				{
					// Rotate so the lone CO is always the plain course
					val targetSingle = CoursingOrder.Start
					val perm = groupedCos.last.head.coPerm(targetSingle)
					groupedCos = groupedCos.map{_.map(_.permCO(perm))}
					val extras = groupedCos.map{ groupExtra(_) }
					val setsFound = groupedCos.toSet
					val outputCos = groupedCos.map{_.map{ (co) => co+co.sign }.mkString("(",",",")") }
					if (!coGroupsFound.contains(setsFound))
					{
						println( groupedCos.size+"  "+rotStarts.mkString(",")+"  "+rels.mkString(" ")+"  "+outputCos.mkString(" ")+"  "+extras.mkString(" ") )
						coGroupsFound+= setsFound
					}
				}
				cosFound+= coSet
			}
		}

		val rounds = Row(nbells)
		var startLH = rounds.apply(permBob)
		//var startLH = rounds.apply(permAGroup)
		val lhs = for (i <- 2 until nbells) yield {val lh=startLH; startLH=startLH.apply(permAGroup); lh}
		var newAtw = new Atw()
		for (leadAtw <- protoMethods.zip(rounds::lhs.toList))
			newAtw = newAtw.add(leadAtw._1, leadAtw._2)
		find(newAtw, 1, List(rounds), courseheads-rounds-startLH)
	}
}
