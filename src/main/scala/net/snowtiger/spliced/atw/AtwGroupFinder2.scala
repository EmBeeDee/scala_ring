package net.snowtiger.spliced.atw

import java.io.{FileWriter, PrintWriter}

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition.Stage
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

import scala.collection.mutable

/**
 * @author mark
 */

object AtwGroupFinder2 extends AtwHelper
{
	val nbells = 12
	val nSets = 1
	//val extraSet: Option[Set[CoursingOrder]] = Some(Set("53246", "65324", "24653", "53624", "24536", "62453").map{CoursingOrder(_)})
	val extraSet: Option[Set[CoursingOrder]] = None //Some("25463 35264 56234 63542 53246 54632 26354 54263 63254".split(" ").toSet.map{(c:String)=>CoursingOrder(c)})

	//val out = new PrintWriter(new FileWriter("atwGroupsExtra2.txt"))
	val out = new PrintWriter(new FileWriter("atwGroupsMaximus2.txt"))

	val GenMixedSignCourses = true
	val MaxNegativePerCourse = 10
	val MaxGroupSize = 1
	val BestSetsOnly = false
	val GenAllTouches = true
	val RequireQSets = false

	val courseheads = (if (GenMixedSignCourses) genAllCourseHeads(nbells) else genPositiveCourseHeads(nbells)).toSet

	val plainCourseLeads = new Stage(nbells).StageLeadheads.map{_.toPerm}.toArray
	val allCOs = Map[String,CoursingOrder]() ++
			courseheads.map{(lh) => CoursingOrder(lh.coursingOrder(plainCourseLeads(1)))}.map{(c)=> (c.raw, c)}
	allCOs.foreach{(p)=> p._2.populateLinks(if (GenMixedSignCourses) List("M","W","H","sM","sW","sH") else List("M","W","H"), allCOs)}
	def lookupCO(co: CoursingOrder): CoursingOrder = allCOs(co.raw)

	var hookCOs = CoursingOrder.positive.map(lookupCO)

	def main(args: Array[String])
	{
		// First of all find the 7-course ATW sets: these are groups of 7 coursing orders which, if rung to courses started
		// at each tenor position, will generate ATW for the tenors and the working bells. We number them and output.
		val coSetsFound = findATW(nbells)
		emit("Found "+coSetsFound.size+" sets")
		// Now try to combine the ATW sets in larger groups, to see if we can get a combined set of coursing orders
		// which fall into QSets and can be linked into a composition.
		combineCOSets(coSetsFound, nSets, extraSet)
	}

	def emit(obj: Any)
	{
		val line = obj.toString
		println(line)
		out.println(line)
	}

	def emit()
	{
		println()
		out.println()
		out.flush()
	}

	def combineCOSets(coLists: Array[List[CoursingOrder]], nSets: Int, extra: Option[Set[CoursingOrder]]) =
	{
		//val positiveCoLists = coLists.filter{_.forall(_.positive)}

		def output(groupedCos: List[Set[CoursingOrder]], inOrderFound: List[(Int,List[CoursingOrder])])
		{
			//val rels = groupedCos.map{ relationship(_) }
			//val outputCos = groupedCos.map{_.map{ (co) => co+co.sign }.mkString("(",", ",")") }
			if (groupedCos.size==1)
			{
				val comps = QSetCompLinker.searchByCOLinks(allCOs("53246"), groupedCos.head, GenAllTouches)
				if (!comps.isEmpty)
				{
					//output(groupedCos.size+"  "+rels.mkString(" ")+"  "+outputCos.mkString(" ") )
					emit(inOrderFound.map{(p)=> p._1+p._2.mkString(". (",", ",")")}.mkString(", "))
					for (comp <- comps)
						emit("   "+comp)
					emit()
				}
			}
		}

		/** Only look at set numbers >= the highest set used so far; cheap rotational sort */
		def tryCombinations(coSet: Set[CoursingOrder], foundSoFar: List[(Int,List[CoursingOrder])], highestSetUsed: Int)
		{
			if (foundSoFar.size==nSets)
			{
				val leftOverCOs = if (RequireQSets) cosNotInQSets(coSet) else Set[CoursingOrder]()
				if (leftOverCOs.size<=0)
				{
					val qSetCOs = coSet--leftOverCOs
					val groupedCos = relatedCOSets(qSetCOs)
					output(groupedCos, foundSoFar.reverse)
				}
			}
			else
			{
				for (i <- highestSetUsed until coLists.size)
				//val i = highestSetUsed
				//if (true)
				{
					val other = coLists(i)
					for (hookCO <- hookCOs)
					{
						val perm = other.head.coPerm(hookCO)
						val permuted = other.map{(c)=> lookupCO(c.permCO(perm))}
						val combinedSet = permuted.toSet++coSet
						if (combinedSet.size==permuted.size+coSet.size)
							tryCombinations(combinedSet, (i,permuted)::foundSoFar, i.max(highestSetUsed))
					}
				}
			}
		}

		extra match
		{
			case None =>
				for ( i <- 0 until coLists.size)
					tryCombinations(coLists(i).toSet, List( (i, coLists(i)) ), i)
			case Some(extras) =>
				tryCombinations(extras, List( (-1, extras.toList)), 0)
		}
	}


	def findATW(nbells: Int): Array[(List[CoursingOrder])] =
	{
		// Set of the sets of all rotations of each set of COs found by the search.
		// By "rotations", we mean take each CO from the inner set, and rotate to 53246.
		val coSetsFound = mutable.Set[Set[Set[CoursingOrder]]]()
		// List of each list of COs found by the search; the COs are in the right order for the splice rotation
		var revFound = List[List[CoursingOrder]]()

		def find(atw: SingleMethodAtw, rotation: Int, rotStarts: List[Row], chsLeft: Set[Row])
		{
			if (rotStarts.count{!_.positive}<=MaxNegativePerCourse)
			if (rotation>=plainCourseLeads.size)
				foundAtwCourses(rotStarts.reverse)
			else
			{
				// number of working bells * number of leads in course * number of courses done
				val requiredAtwScore = (nbells-1) * 1 * (rotation+1)
				for (ch <- chsLeft)
				{
					val startLH = ch.apply(plainCourseLeads(rotation))
					val newAtw = atw+startLH
					if (newAtw.score==requiredAtwScore)
						find(newAtw, rotation+1, startLH::rotStarts, chsLeft-ch)
				}
			}
		}

		def foundAtwCourses(rotStarts: List[Row])
		{
			val coList = rotStarts.map{(lh) => lookupCO(CoursingOrder(lh.coursingOrder(plainCourseLeads(1))))}
			val coSet = coList.toSet

			def rotateCOs(coToBecomePC: CoursingOrder) =
			{
				// Rotate all COS by the perm that takes the given CO to the plain course
				val perm = coToBecomePC.coPerm(CoursingOrder.Start)
				coSet.map{(co)=> lookupCO(co.permCO(perm))}
			}

			val groupedCos = relatedCOSets(coSet)
			val rels = groupedCos.map(relationship)
			if (groupedCos.size<=MaxGroupSize)
			// Output courses where the groups of coursing orders are all related within themselves by W, M or H, with one lone CO allowed
			if (!BestSetsOnly || (groupedCos.size<=3 && rels.count(_=="x")<=1 && rels.count(_=="-")<=1))
			{
				val coSetRotations = coSet.map(rotateCOs)
				if (!coSetsFound.contains(coSetRotations))
				{
					val extras = groupedCos.map(groupExtra)
					val outputCos = groupedCos.map{_.map{ (co) => co+(new Row(co.raw).signChar) }.mkString("(",",",")") }
					val number = revFound.size
					emit( number+".   "+groupedCos.size+"  "+rotStarts.mkString(",")+"  "+rels.mkString(" ")+"  "+outputCos.mkString(" ")+"  "+extras.mkString(" ") )
					revFound = coList::revFound
					coSetsFound+= coSetRotations
				}
			}
		}

		val rounds = Row(nbells)
		val protoMethod = new ProtoMethod('A', rounds.toPerm)
		val startAtw = new SingleMethodAtw(protoMethod)+rounds
		find(startAtw, 1, List(rounds), courseheads-rounds)
		revFound.reverse.toArray
	}

}