package net.snowtiger.methodmaker

import net.snowtiger.ringing.{PN, Row}

import scala.collection.mutable

/**
 * @author Mark
 */

class GoodPn(nBells: Int)
{
	import Row.Rounds;

	def filterOneConsec(pn: PN) =
	{
		val finalPlace = PN(List(nBells-2, nBells-1))
		val consecs = pn.consecutives
		consecs.isEmpty || (consecs.tail.isEmpty && consecs.head.nPlaces==2 && finalPlace!=consecs.head )
	}

	def filterNoUltimatePlace(pn: PN) = !pn.isPlace(nBells-1)

	lazy val allPN = PN.generateAll(nBells).toList
	lazy val noUltimatePlace = allPN.filter(filterNoUltimatePlace)
	lazy val oneConsec = noUltimatePlace.filter(filterOneConsec)
	lazy val noConsec = oneConsec.filter{_.consecutives.isEmpty}

	def hlPN(treblePos: Int) = allPN.filter(_.nPlaces<4).filter(_.isPlace(treblePos))
	lazy val lhPN = if (nBells%2==0) List( PN("12"), PN("1"+Rounds(nBells-1)) ) else List( PN("1"), PN("12"+Rounds(nBells-1)) )

	def callChangePN(n: Int) = {
		val allFixed = Row(nBells).toString
		PN(allFixed.take(n-1)+allFixed.drop(n+1), nBells)
	}
	lazy val callChangePNs = (1 until nBells).map(callChangePN).toList

}

object GoodPn
{
	val cache = mutable.Map[Int,GoodPn]()

	def apply(nbells: Int) = cache.getOrElseUpdate(nbells, new GoodPn(nbells))

	def isAllowableConsecutivePn(pn: PN, revPN: List[PN]):Boolean =
		revPN.isEmpty || isAllowableConsecutivePn(pn, revPN.head)

	def isAllowableConsecutivePn(pn1: PN, pn2: PN) = pn1!=pn2 && !pn1.hasConsecutivePlacesWith(pn2)

	def isRightPlaceConsecutivePn(pn: PN, revPN: List[PN]):Boolean =
		(revPN.isEmpty && pn.isCross) || isRightPlaceConsecutivePn(pn, revPN.head)

	def isRightPlaceConsecutivePn(pn1: PN, pn2: PN) = pn1!=pn2 && pn1.isRightPlaceAgainst(pn2)

}