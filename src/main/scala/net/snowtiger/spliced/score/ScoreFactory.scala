package net.snowtiger.spliced.score

import net.snowtiger.spliced.composition.Composition

/**
 * @author mark
 */
case class ScoreFactory(comp: Composition)
{
	val length = comp.length
	val methodsUsed = comp.methodsUsed
	lazy val longestNoComRun = comp.longestNoComRun
	lazy val longestAbsence = comp.longestAbsence
	lazy val isAtw = comp.isAtw
	lazy val atwScore = comp.atwScore
	lazy val music = comp.music
	lazy val counts = comp.methodCounts(comp.methodsUsed.toList)
	lazy val balance = comp.mostPopularMethodCount(counts)-comp.leastPopularMethodCount(counts)
	// Any balance difference of less than the number of methods is perfection.
	lazy val balanceScore = 200 - (if (balance<10-comp.methodsUsed.size) 0 else balance)
	//val av = methodCounts(methodsUsed.toList).sum/methodsUsed.size
	//val balance = -Math.sqrt( methodCounts(methodsUsed.toList).map{ (x)=> (x-av)*(x-av) }.sum )

	lazy val atwBoost = if (isAtw) 200 else 0

	def strictLenScore = ScoreFactory.strictLenScore(comp)
	def normalLenScore = ScoreFactory.normalLenScore(comp)
	def laxLenScore = ScoreFactory.laxLenScore(comp)

	//def levenshteinPartAtwFinder = methodsUsed.size*1000000 + atwScore - longestNoComRun*10 + (if (isAtw) strictLenScore+1000-comp.levenshteinPartDistance(2) else 0)
	def levenshteinPartAtwFinder = methodsUsed.size*1000000 + 2*(4*atwScore + atwBoost) + balanceScore +
			strictLenScore-0*(longestNoComRun^2)-5*comp.levenshteinPartDistance
	def levenshteinPartMusicFinder = methodsUsed.size*1000000 + 8*atwScore + atwBoost +
			strictLenScore-0*(longestNoComRun^2)-10*comp.levenshteinPartDistance+5*balanceScore+music(0)+music(1)

	def unlimitedLengthAtwFinder = methodsUsed.size*1000000 + atwScore + (if (isAtw) laxLenScore+1000-longestNoComRun else 0)
	def strictAtwFinder = methodsUsed.size*1000000 + atwScore*40 + balanceScore + 2*strictLenScore - 0*longestNoComRun + (if (isAtw) 1000+music(0) else 0)
	//def strictAtwFinder = methodsUsed.size*1000000 + 10*(atwScore*4 + strictLenScore - longestNoComRun*2 + (if (isAtw) 1000 else 0)) + (1*music(0)+2*music(1)+2*(balanceScore))/10
	def atwFinder = methodsUsed.size*1000000 + 5*(balanceScore)+10*(atwScore) + (2*music(0)+2*music(1)+0*music(2)) - 5*longestNoComRun + strictLenScore + atwBoost

	def balanceFinder = methodsUsed.size*1000000 + 16*(balanceScore)+10*(atwScore) + (2*music(0)+4*music(1)+0*music(3)) - 10*longestNoComRun - 3*longestAbsence + 4*strictLenScore + 10*atwBoost

	// Normal scoring for good music with reasonable balance and no longruns, also trying to preserve ATW if achieved.
	def musicFinder =
		methodsUsed.size*1000000 -
				60*comp.falseScore +
				4*(balanceScore)+
				10*(atwScore) +
				(2*music(0)+3*music(1)+music(2)) -
				10*longestNoComRun -
				5*longestAbsence +
				strictLenScore

	def musicFinder2 =
		methodsUsed.size*1000000 +
		strictLenScore + atwBoost -
		60*comp.falseScore +
		3*(balanceScore)+1*(atwScore) +
		(4*music(0)+10*music(1)+2*music(2)) -
		5*longestNoComRun -
		/* (if (comp.com>12) 25*(comp.com-12) else 0) - */
		0*(comp.longestAbsence)-0*comp.leadsToLastMethod -
		1*comp.leadsWithoutMusic

	// For quarter peals
	def qpFinder = methodsUsed.size*1000000 + 3*(balanceScore)+0*(atwScore) +
			(2*music(0)+3*music(1)+music(2)) - 12*longestNoComRun - 5*longestAbsence +
			ScoreFactory.qpLenScore(comp) - 10*comp.falseScore

	def qpFinderSnap = methodsUsed.size*1000000 + 2*(balanceScore)+0*(atwScore) +
			(2*music(0)+3*music(1)+music(2)) - 12*longestNoComRun - 5*longestAbsence +
			ScoreFactory.qpLenScoreSnap(comp)
}

object ScoreFactory
{
	val maxLen = 5100

	def levenshteinPartAtwFinder(comp: Composition) = ScoreFactory(comp).levenshteinPartAtwFinder

	def levenshteinPartMusicFinder(comp: Composition) = ScoreFactory(comp).levenshteinPartMusicFinder

	def unlimitedLengthAtwFinder(comp: Composition) = ScoreFactory(comp).unlimitedLengthAtwFinder

	def strictAtwFinder(comp: Composition) = ScoreFactory(comp).strictAtwFinder

	def atwFinder(comp: Composition) = ScoreFactory(comp).atwFinder

	def balanceFinder(comp: Composition) = ScoreFactory(comp).balanceFinder

	def musicFinder(comp: Composition) = ScoreFactory(comp).musicFinder

	def musicFinder2(comp: Composition) = ScoreFactory(comp).musicFinder2


	def methodCounts(comp: Composition) = comp.methodCounts(comp.methodsUsed.toList)

	def balance(comp: Composition) =
	{
		val counts = methodCounts(comp)
		comp.mostPopularMethodCount(counts)-comp.leastPopularMethodCount(counts)
	}

	def leastPopularMethodCount(comp: Composition) = comp.leastPopularMethodCount(methodCounts(comp))

	// Any balance difference of less than the number of methods is perfection. (Eh - why??!)
	def balanceScore(comp: Composition) =
	{
		val bal = balance(comp)
		200 - (if (bal<comp.methodsUsed.size) 0 else bal-comp.methodsUsed.size+1)
	}
	def balanceScore2(comp: Composition) =
	{
		val bal = balance(comp)-1
		if (bal<=0) 0 else -bal
	}

	def laxLenScore(comp: Composition) =
	{
		val length = comp.length
		if (length<5000)
			(length-5100)/10
		else if (length>maxLen)
			(maxLen-length)/100
		else
			0
	}
	def normalLenScore(comp: Composition) =
	{
		val length = comp.length
		if (length<5000)
			(length-5100)/5
		else if (length>maxLen)
			(maxLen-length)/20
		else
			0
	}
	def strictLenScore(comp: Composition): Int =
	{
		val length = comp.length
		if (length<5000)
			(length-5100)/2
		else if (length>maxLen)
			(maxLen-length)/2
		else
			0
	}
	def strictLenScore(comp: Composition, scorePerRow: Double): Int =
	{
		val length = comp.length
		if (length<5000)
			(length-5100)/2
		else
		{
			var score = ((5000-comp.length)*scorePerRow).toInt
			if (length>maxLen)
				score+= (maxLen-length)/2
			score
		}
	}

	def qpLenScore(comp: Composition) =
	{
		val max = 1280
		val length = comp.length
		if (length<1250)
			(length-max)
		else if (length>max)
			(max-length)/2
		else
			0
	}

	def qpLenScoreSnap(comp: Composition) =
	{
		val max = 1280
		val length = comp.length
		if (length<1248)
			(length-max)
		else if (length>max)
			(max-length)/2
		else
			0
	}

}

