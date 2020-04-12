package net.snowtiger.spliced.atw

import net.snowtiger.ringing.Row
import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */

class AtwHelper
{
	def genAllCourseHeads(nbells: Int) = Row.genTenorsTogetherCourseHeads(nbells)
	def genPositiveCourseHeads(nbells: Int) = Row.genPositiveTenorsTogetherCourseHeads(nbells)

	protected def hookCOs(cos: Set[CoursingOrder]) =
	{
		def expandAllQSets(co: CoursingOrder) =
		{
			val home = co.home
			val wrong = co.wrong
			val middle = co.middle
			Set(home, home.home, wrong, wrong.wrong, middle, middle.middle)
		}
		cos.flatMap{ expandAllQSets(_).filter(!cos.contains(_)) }
	}

	protected def hookCOsSingles(cos: Set[CoursingOrder]) =
	{
		def expandAllQSetsWithSingles(co: CoursingOrder) = Set(co.singleHome, co.singleWrong, co.singleMiddle)
		cos.flatMap{ expandAllQSetsWithSingles(_).filter(!cos.contains(_)) }
	}

	/** Returns all coursing orders which do not have a complete QSet (of either M, W or H) in the set
		* WARNING: does NOT work if a mix of positive and negative coursing orders are present! */
	protected def cosNotInQSets(cos: Set[CoursingOrder]) =
	{
		def formsQSet(co: CoursingOrder) =
			cos.count(_.callBetween(co)=="H")==2 || cos.count(_.callBetween(co)=="M")==2 || cos.count(_.callBetween(co)=="W")==2
		cos.filter(!formsQSet(_))
	}

	/** Form sets of COs, where the COs in each set are related by standard calls */
	protected def relatedCOSets(cos: Set[CoursingOrder]) =
	{
		var sets: List[Set[CoursingOrder]] = Nil
		var remainingCos = cos

		/** Recursively extract all COs from remainingCos which are related to those in the initial set, however distantly */
		def extractRelated(relatedCOs: Set[CoursingOrder]): Set[CoursingOrder] =
		{
			val newRelated = remainingCos.filter{ (co)=> relatedCOs.exists(relatedByCall(_, co)) }
			remainingCos = remainingCos--newRelated
			if (newRelated.isEmpty)
				relatedCOs
			else
				extractRelated(relatedCOs++newRelated)
		}

		while (!remainingCos.isEmpty)
		{
			val co = remainingCos.head
			remainingCos-= co
			sets = extractRelated(Set(co))::sets
		}
		sets.sortBy{-_.size}
	}

	protected def relatedCOSet(co: CoursingOrder, others: Set[CoursingOrder]) = others.filter(relatedByCall(_, co))

	protected def relatedByCall(co1: CoursingOrder, co2: CoursingOrder) =
		if (co1.linksPopulated) co1.relations.contains(co2) else "HWM".contains(co1.callBetween(co2))

	protected def relationship(cos: Set[CoursingOrder]): String =
	{
		if (cos.size<2)
			"-"
		else
		{
			val coList = cos.toList
			val relationships = coList.zip(coList.tail).map{(p)=> p._1.callBetween(p._2)}
			val firstRelationship = relationships.head
			if (relationships.tail.forall{_==firstRelationship})
				firstRelationship
			else
				"x"
		}
	}

	protected def groupExtra(cos: Set[CoursingOrder]) =
	{
		cos.toList match
		{
			case List(a) => "lone"
			case List(a,b) => b.permCO(a.coPerm(b))
			case _ => "-"
		}
	}


}