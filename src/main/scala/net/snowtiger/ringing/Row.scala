package net.snowtiger.ringing

import net.snowtiger.ringing.Row.{MaskChar, MaxStage, Rounds, Stages}
import net.snowtiger.spliced.score.SpecialRows

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * @author Mark
 */
case class Row(bells: Seq[Char])
{
	def this(row: String)
	{
		this(Row.concreteType(row))
	}

	def this(nbells: Int)
	{
		this(Rounds.substring(0, nbells))
	}

	lazy val nbells = bells.size
	lazy val isRounds = this.equals(rounds)
	def rounds = Row(nbells)
	lazy val positive = isPositive
	lazy val signChar = if (isPositive) "+" else "-"
	lazy val order = toPerm.order
	lazy val (cycleSizes, cyclePattern) = determineCycles
	lazy val anyBellHome = bells.zip(Rounds).exists(sameChar(_))
	lazy val isSubPrincipleLeadhead = cycleSizes.size > 1 && cycleSizes.size < nbells && cycleSizes.forall(_ == cycleSizes(0))

	def stage = Stages(nbells-1)

	def toPerm = new Perm(bells)

	def reverse = Row(bells.reverse)

	def apply(pn: PN) = Row(pn.permute(bells))

	/** permute by the <em>places</em> specified in the perm, e.g. 4231.apply(2314) = 2341 */
	def apply(perm: Perm) = Row(perm.permuteBy(toPerm))

	/** permute by the <em>bells</em> specified in the perm, e.g. 4231.apply(2314) = 4312 */
	def permuteBy(perm: Perm) = Row(toPerm.permuteBy(perm))

	def placeOf(bell: Int) = bells.indexOf(Rounds(bell - 1)) + 1

	def bellAt(pos: Int) = getBellNoFor(bells(pos-1))
	def asBellNumbers = (1 to nbells).map(bellAt).toList

	def isMasked(pos: Int) = bells(pos-1)==MaskChar

	def bellsInSamePlace(other: Row): Seq[Int] =
	{
		for ((b1, b2) <- bells.zip(other.bells); i = Rounds.indexOf(b1); if (i>=0 && b1==b2))
			yield i+1
	}

	/** Total distance of bells in this row to the other row. Will be zero if both rows the same, 2 if
		* only one adjacent pair swapped, etc. */
	def distance(other: Row): Int =
	{
		bells.zipWithIndex.map((x)=> Math.abs(x._2+1-other.placeOf(getBellNoFor(x._1)))).sum
	}

	/** Moves bells numbers up (+ve delta) or down (-ve). Row("2134").shift(1) = Row("3254"). */
	def shift(delta: Int) = Row(bells.map{shiftBell(_, delta)}.mkString)
	private def shiftBell(bellChar: Char, delta: Int) =
	{
		val bellNum = getBellNoFor(bellChar) + delta
		assert(bellNum>0 && bellNum<=MaxStage)
		Rounds(bellNum-1)
	}

	lazy val biggestBell: Int = bells.map{getBellNoFor(_)}.max
	lazy val smallestBell: Int = bells.map{getBellNoFor(_)}.min

	/** Surrounds current row with bells at rounds; e.g. Row("65432").extendTo(8) = Row("16543278") */
	def extendTo(nbells: Int) = Row( Rounds.substring(0, smallestBell-1) + this + Rounds.substring(biggestBell, nbells) )

	/** Extracts the middle of a row, between start and end (both INCLUSIVE, and in place numbers, i.e. start at 1). */
	def extract(start: Int, end: Int) = Row(bells.slice(start-1, end))

	/** Returns None if the given perm cannot bring the bell home */
	def rotateUntilBellHome(bell: Int, perm: Perm) = generateGroupByPlaces(perm).find{_.bellAt(bell)==bell}

	/** Applies the perm to the current row until we get back to it; return the resulting list, with this row first.
		* Note the permutation is applied by the places, not the bells! */
	def generateGroupByPlaces(perm: Perm) =
	{
		val rows = ListBuffer(this)
		var nextRow = this.apply(perm)
		while (nextRow!=this)
		{
			rows+= nextRow
			nextRow = nextRow.apply(perm)
		}
		rows.toList
	}

	def highestBellNotHome: Int =
	{
		if (bellAt(nbells)!=nbells)
			nbells
		else if (nbells==1)
			0
		else
			Row(toString.substring(0, nbells-1)).highestBellNotHome
	}

	/** Returns true if the row occurs in the plain course of Plain Bob at the same stage */
	def isPlainBob =
	{
		// Consecutive pairs of bells from odd then reversed even positions must be coursing pairs, ignoring the treble
		oddsThenEvens.sliding(2).forall{(pair)=> isCoursingPairOrTreble(pair.head, pair.tail.head)}
	}

	def countRunPositions(runSize: Int): Int =
	{
		val front = asBellNumbers.slice(0, runSize).toArray
		val back = asBellNumbers.slice(nbells-runSize, nbells).toArray
		var count = 0
		if (Row.isRunPosition(front, nbells))
			count+= 1
		if (Row.isRunPosition(back, nbells))
			count+= 1
		count
	}

	def countPotentialRuns(runSize: Int, swapsAllowed: Boolean): Int =
	{
		val front = asBellNumbers.slice(0, runSize).toArray
		val back = asBellNumbers.slice(nbells-runSize, nbells).toArray
		var count = 0
		if (Row.isPotentialRun(front, nbells, swapsAllowed))
			count+= 1
		if (Row.isPotentialRun(back, nbells, swapsAllowed))
			count+= 1
		count
	}

	/** Returns true if the row is a cyclic permutation of the given row */
	def isCyclicPermOf(row: Row) =
		(0 until nbells).map{ (i)=> Row(bells.slice(i,nbells)++bells.slice(0,i)) }.contains(row)

	/** Gives the bells in all odd places ascending followed by those in all even places descending */
	private def oddsThenEvens = (1 to nbells by 2).toList.map{bellAt(_)} ++ (2 to nbells by 2).toList.map{bellAt(_)}.reverse

	/** For backstroke leads of PB methods only! */
	def coursingOrder(plainLeadPerm: Perm): String =
	{
		assert(bellAt(1)==1)
		val home = rotateUntilBellHome(nbells, plainLeadPerm)
		home.get.coursingOrder()
	}

	/** For backstroke course heads of PB methods only! */
	def coursingOrder(): String =
	{
		assert(bellAt(1)==1 && bellAt(nbells)==nbells)
		val tenorsTogether = bells.slice(6, nbells).zip(Rounds.substring(6, nbells)).forall(sameChar(_))
		var CO = List(bells(4),bells(2),bells(1),bells(3),bells(5)).mkString("")
		if (!tenorsTogether)
			for (n <- 7 until nbells by 2)
				CO = bells(n-1).toString + CO + bells(n).toString
		CO
	}

	/** For treble-dominated, PB-leadhead methods only */
	def isCoursingPair(b1: Int, b2: Int) = Row.isCoursingPair(b1, b2, nbells)
	def isCoursingPairOrTreble(b1: Int, b2: Int) = b1==1 || b2==1 || isCoursingPair(b1, b2)
	def allCoursing(bells: List[Int]) = bells.sliding(2).forall{(pair)=> isCoursingPair(pair.head, pair.tail.head)}

	/** For treble-dominated, PB-leadhead methods only: works out the number of n-tuples which are in course */
	def coursingScore(size: Int) =
		(oddsThenEvens++oddsThenEvens.slice(0, size-1)).sliding(size).map{ (bells)=> if (allCoursing(bells)) 1 else 0}.sum

	def pnBetween(other: Row): Option[PN] =
	{
		val placesList = for (((b1, b2), i) <- bells.zip(other.bells).zipWithIndex; if (b1==b2)) yield i
		val candidatePn = PN(placesList)
		if (apply(candidatePn)==other)
			Some(candidatePn)
		else
			None
	}

	/** Replaces bells 1..n inclusive with a * character */
	def maskFrontBells(n: Int) = Row(bells.map{ (c)=> if (getBellNoFor(c)<=n) MaskChar else c })

	/** Replaces bells 2..n inclusive with a * character */
	def maskFrontBellsNotTreble(n: Int) = Row(bells.map{ (c)=> val b=getBellNoFor(c); if (b>1 && b<=n) MaskChar else c })

	/** Replaces bells n..tenor inclusive with a * character */
	def maskBackBells(n: Int) = Row(bells.map{ (c)=> if (getBellNoFor(c)>=n) MaskChar else c })

	private def getBellNoFor(c: Char) = Rounds.indexOf(c)+1

	def inSameCycle(bell1: Int, bell2: Int) = cyclePattern(bell1) == cyclePattern(bell2)

	/** Early row is this, later row is other.
		* The result is: this.permuteBy( this.permutationBetween(other) ) == other  */
	def permutationBetween(other: Row) = toPerm.inverse.permuteBy(other.toPerm)
	// Incorrect version, apparently "corrected" from the above, despite successful use of the original in "reflectAroundPivot" method below...
	//def permutationBetween2(other: Row) = other.toPerm.permuteBy(toPerm.inverse)

	def reflectAroundPivot(prePivotRow: Row, pivotPN: PN):Option[Row] = reflectAroundPivot(prePivotRow, prePivotRow.apply(pivotPN))

	def reflectAroundPivot(prePivotRow: Row, postPivotRow: Row):Option[Row] =
	{
		val permBetween = prePivotRow.permutationBetween(postPivotRow)
		if (permBetween.isSelfInverse)
			Some(this.apply(permBetween))
		else
			None
	}

	private def sameChar(pair: Tuple2[Char, Char]) = pair._1 == pair._2

	private def isPositive: Boolean =
	{
		if (nbells<=1)
			true
		else
		{
			val tenorPlace = placeOf(nbells)
			assert(tenorPlace>0, "Cannot identify nature of non-standard row: "+this)
			if (tenorPlace==nbells)
				Row(toString().substring(0, nbells-1)).positive
			else
			{
				val arr = bells.toArray
				arr(tenorPlace-1) = arr(nbells-1)
				!(Row(new String(arr).substring(0, nbells-1)).positive)
			}
		}
	}

	/** Only works if bells are numbered 1..n, e.g. a row "54867" will crash this */
	private def determineCycles =
	{
		val pattern = mutable.IndexedSeq.fill(nbells)(-1)
		var sizes = List[Int]()
		for (i <- 0 until nbells)
		{
			var j = i
			var c = 0
			while (pattern(j) < 0)
			{
				pattern(j) = i
				j = Rounds.indexOf(bells(j))
				c += 1
			}
			if (c > 0)
				sizes = c :: sizes
		}
		(sizes.reverse, pattern.map(Rounds(_)).toList)
	}

	override def toString = bells.mkString("")
}

object Row
{
	def concreteType[T](xs: Seq[T]) = xs.toList

	val Stages = List("One", "Two", "Singles", "Minimus", "Doubles", "Minor", "Triples", "Major", "Caters", "Royal",
		"Cinques", "Maximus", "Sextuples", "Fourteen", "Septuples", "Sixteen", "Octuples", "Eighteen", "Nonuples", "Twenty",
		"Decuples", "Twenty-Two", "Undecuples", "Twenty-Four")
	val Rounds = "1234567890ETABCDFGHJKLMN"
	val MaxStage = Rounds.size
	val MaskChar = '*'

	def apply(row: String) = new Row(row)
	def apply(row: Perm) = new Row(row.perm)
	def apply(nbells: Int) = new Row(nbells)
	def apply(bells: List[Int]) = new Row(concreteType(bells.map{bellChar}))

	def bellNumber(c: Char) = Rounds.indexOf(c) + 1
	def bellChar(b: Int) = Rounds(b-1)

	/** For treble-dominated, PB-leadhead methods only */
	def isCoursingPair(b1: Int, b2: Int, nbells: Int): Boolean =
	{
		if (b1>b2)
			isCoursingPair(b2, b1, nbells)
		else
			b1>1 && (b2-b1==2 || (b1==2 && b2==3) || (b1==nbells-1 && b2==nbells))
	}

	/** Returns true for e.g. 24 or 53, but false for 42 or 35. */
	def isCoursingPairRightHunting(b1: Int, b2: Int, nbells: Int): Boolean =
	{
		val forwardCoursing = (2 to nbells by 2).toList ++ (3 to nbells by 2).toList.reverse ++ List(2)
		forwardCoursing.sliding(2).contains(List(b1,b2))
	}

	/** For treble-dominated, PB-leadhead methods only */
	def isCoursingSet(bells: Array[Int], nbells: Int): Boolean =
	{
		def ascendsByTwos(xs: Array[Int]) =
		{
			var result = true
			for (i <- 1 until xs.size)
				if (xs(i)-xs(i-1)!=2)
					result = false
			result
		}
		val (even, odd) = bells.partition{_%2==0}
		val evenSorted = even.sorted
		val oddSorted = odd.sorted
		if (!ascendsByTwos(evenSorted) || !ascendsByTwos(oddSorted))
			false
		else if (oddSorted.isEmpty)
			true
		else if (oddSorted.head==1)
			false
		else if (evenSorted.isEmpty)
			true
		else if (evenSorted.head==2 && oddSorted.head==3)
			true
		else if (evenSorted.last==(nbells/2)*2 && oddSorted.last==((nbells-1)/2)*2+1)
			true
		else
			false
	}

	/** Returns true if the bells given can create a run anywhere in the plain course of the (PB-leadhead) method.
		* E.g. if given four bells, this will include sequences 2345, 3254, 5432 or 4523, plus any equivalent coursing four. */
	def isRunPosition(bells: Array[Int], nbells: Int): Boolean =
	{
		def cp(i: Int, j: Int) = isCoursingPair(bells(i), bells(j), nbells)
		val s = bells.length
		if (s<2)
			true
		else if (!cp(0,1) && !cp(s-1, s-2))	// Either the first or last pairs must be in course (depending on which way the run goes)
			false
		else	// Every pair of bells separated by two places must be in course
			(0 until s-2).forall((x)=> cp(x,x+2))
	}


	/** Returns true if the bells given can create a run anywhere in the plain course of the (PB-leadhead) method, */
	/** including 467890 and 537890 type positions and, if requested, swaps like 657890 or 647890. */
	def isPotentialRun(bells: Array[Int], nbells: Int, swapsAllowed: Boolean): Boolean =
	{
		val s = bells.length
		if (s<4)
			isRunPosition(bells, nbells)
		else
			isPotentialRunUp(bells, nbells, swapsAllowed) || isPotentialRunUp(bells.reverse, nbells, swapsAllowed)
	}

	private def isPotentialRunUp(bells: Array[Int], nbells: Int, swapsAllowed: Boolean): Boolean =
	{
		def cp(i: Int, j: Int) = isCoursingPair(bells(i), bells(j), nbells)
		def isGoodEnd(a: Int, b:Int, c:Int, d:Int) =
			(cp(a,c) && cp(b,d)) || (cp(a,b) && (cp(a,c) || cp(b,d)))
		val s = bells.length
		if (cp(s-1,s-2) && (2 until s-2).forall((x)=> cp(x,x+2)))
		{
			isGoodEnd(0, 1, 2, 3) || (swapsAllowed && isGoodEnd(1, 0, 2, 3))
		}
		else
			false
	}


	def generateAll(nbells: Int): List[Row] = generateAll(Row(nbells).toString)

	def generateAll(from: String): List[Row] =
	{
		val rows = ListBuffer[Row]()

		def generateRows(row: List[Char], bellsLeft: IndexedSeq[Char])
		{
			if (bellsLeft.size == 0)
				rows+= Row(row)
			else
				for (i <- 0 until bellsLeft.size)
					generateRows(bellsLeft(i) :: row, bellsLeft.take(i) ++ bellsLeft.drop(i + 1))
		}

		generateRows(Nil, from)
		rows.toList
	}

	def genTenorsTogetherCourseHeads(nbells: Int) = Row.generateAll(5).map{_.shift(1).extendTo(nbells)}
	def genPositiveTenorsTogetherCourseHeads(nbells: Int) = Row.generateAll(5).filter{_.positive}.map{_.shift(1).extendTo(nbells)}

	def main(args: Array[String])
	{
		//test()
		for (n <- 6 to Row.MaxStage by 2)
		{
			val queens = SpecialRows.queens(n)
			val tittums = SpecialRows.tittums(n)
			val group = queens.toPerm.generateGroup
			val hasTittums = group.exists(Row(_)==tittums)
			println(n+" "+queens.order+" "+(if (hasTittums) "T" else "")+" "+queens+" "+tittums)
		}
	}

	def test()
	{
		var r = Row(8)
		println(r)
		r = r.apply(PN("x"))
		println(r)
		r = r.apply(PN("38"))
		println(r)
		assert(Row(8).apply(PN("x")) == Row("21436587"))
		assert(Row(8).apply(PN("38")) == Row("21354768"))

		//println(PN.generateAll(8).mkString(", "))

		assert(Row("48672351").pnBetween(Row("84627531"))==Some(PN("38")))

		val rowA = Row("26143857")
		val rowB = Row("62148735")
		val permBetween = rowA.permutationBetween(rowB)
		assert(permBetween==Perm("16843257"))
		assert(rowA.permuteBy(permBetween)==rowB)

		println(permBetween)
		val lh = Perm("15738264")
		val rowC = Row("25134678")
		println(rowB.permuteBy(lh))
		assert(rowB.permuteBy(lh)==rowC)

		val perm2 = rowA.permutationBetween(rowC)
		println(perm2)
		println(permBetween.permuteBy(lh))

		def checkPR(bells: String): Unit =
		{
			assert(Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, false))
			assert(Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, true))
		}

		def checkSPR(bells: String): Unit =
		{
			assert(!Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, false))
			assert(Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, true))
		}

		def checkNPR(bells: String): Unit =
		{
			assert(!Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, false))
			assert(!Row.isPotentialRun(Row(bells).asBellNumbers.toArray, 8, true))
		}

		checkPR("5678")
		checkPR("6587")
		checkPR("2345")
		checkPR("5432")
		checkPR("3254")
		checkPR("4523")
		checkPR("4678")
		checkPR("5378")
		checkPR("8735")
		checkPR("8764")
		checkPR("7853")
		checkPR("7846")

		checkSPR("6578")
		checkSPR("5687")
		checkSPR("4687")
		checkSPR("3578")
		checkSPR("5387")

		checkNPR("3678")
		checkNPR("7658")
	}
}

