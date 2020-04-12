package net.snowtiger.spliced.atw

import net.snowtiger.spliced.search.coursingorder.CoursingOrder

/**
 * @author mark
 */

object GoodCOChecker
{
	val goodCOs = Set("53246", "64235", "35642", "24653", "53462", "64352", "46532", "23564", "25346", "26435",
			"24536", "35426", "42356", "62453", "65324", "63542", "54632", "34256", "23456", "65432",
			"52436", "54326", "63425", "62345", "54236", "52346", "53426").map{CoursingOrder(_)}
	val Start = CoursingOrder.Start

	def check(cos: List[CoursingOrder]) = cos.count(goodCOs.contains(_))

	def checkAllRots(cos: List[CoursingOrder]): (Int, CoursingOrder) =
	{
		var bestScore = -1
		var bestStartingCO: CoursingOrder = null
		for (co <- cos)
		{
			val perm = co.coPerm(Start)
			val permuted = cos.map{ _.permCO(perm)}
			val score = check(permuted)
			if (score>bestScore || (score==bestScore && co==Start))
			{
				bestScore = score
				bestStartingCO = co
			}
		}
		(bestScore, bestStartingCO)
	}
}