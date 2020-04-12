package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.composition._

/**
 * @author mark
 */
object SpliceFinder
{
	def main(args: Array[String])
	{
		val nbells = 8
		val finder = new SpliceFinder(nbells)
		finder.findCourseBuckets(4)
	}
}

class SpliceFinder(nbells: Int)
{
	val stage = new Stage(nbells)
	val lhOrders = stage.StageLeadheads.tail
	var protoMethods = lhOrders.zipWithIndex.map{ (p)=> ProtoMethod(('A'+p._2).toChar, p._1.toPerm) }
	val bestMaximusLHs = "ABCDEFHIJ".toSet
	protoMethods = protoMethods.filter{(pm)=> bestMaximusLHs(pm.abbrev)}

	/** Groups courses by the LH groups they contain. E.g. AABC and BCAA are in the same bucket - same LH groups. */
	def findCourseBuckets(courseLength: Int): Map[String, List[ProtoSplice]] =
	{
		val allCourses = findCourses(courseLength)
		// Remove rotations but creating a set with the canonical rotations (alphabetically lowest)
		val canonicalCourses = allCourses.map{_.canonicalForm}.toSet
		val buckets = canonicalCourses.toList.groupBy{ _.bucket }
		for (bucket <- buckets.keys)
			println(bucket+", "+buckets(bucket).size+", "+buckets(bucket).mkString(", "))
		buckets
	}

	/** Finds all course structures of the given length, using any LH orders (2nd's place is sort of assumed) */
	def findCourses(courseLength: Int): List[ProtoSplice] =
	{
		var allCourses: List[ProtoSplice] = Nil
		var nFound = 0

		def findCourses(revMethodsSoFar: List[ProtoMethod], leadsSoFar: Set[Row], lastLead: Row)
		{
			for (m <- protoMethods) //.filter{!revMethodsSoFar.contains(_)})
			{
				if (revMethodsSoFar.size<1 || revMethodsSoFar.head!=m)
				//if (revMethodsSoFar.size<2 || revMethodsSoFar.head!=m || revMethodsSoFar.tail.head!=m)
				{
					val nextLead = lastLead.apply(m.perm)
					val revMethods = m::revMethodsSoFar
					if (nextLead.isRounds)
					{
						if (revMethods.size>=courseLength)
						{
							nFound+= 1
							//outputCourse(revMethods.reverse)
							allCourses = ProtoSplice(revMethods.reverse)::allCourses
						}
					}
					else if (!leadsSoFar.contains(nextLead))
					{
						if (revMethods.size<courseLength)
							findCourses(revMethods, leadsSoFar+lastLead, nextLead)
					}
				}
			}
		}

		findCourses(List(), Set(), Row(nbells))
		println("Found "+nFound)
		allCourses
	}

	def outputCourse(methods: List[ProtoMethod])
	{
		println( methods.map{_.abbrev}.mkString )
	}

}

