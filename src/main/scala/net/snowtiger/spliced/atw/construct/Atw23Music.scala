package net.snowtiger.spliced.atw.construct

import net.snowtiger.ringing.{PN, Perm, Row}
import net.snowtiger.spliced.atw.AtwMethodBuilder
import net.snowtiger.spliced.atw.AtwMethodBuilder.{compMusicForRows, compRotations}
import net.snowtiger.spliced.score.{Music56Rollup, StandardMajorMusic}

import scala.collection.mutable


object Atw23Music
{
	var maxMusic = 0
	var totalMusic = 0
	var nMusic = 0

	// For method music only
	def musicAnalysis = "Max music = "+maxMusic+" av "+(totalMusic.toDouble/nMusic)

	def addMethodMusic(music: Int)
	{
		totalMusic += music
		nMusic += 1
		if (music > maxMusic)
			maxMusic = music
	}

}

case class CompMusic(total: Int, r5678: Int, queens: Boolean) extends Ordered[CompMusic]
{
	def +(other: CompMusic) = CompMusic(total+other.total, r5678+other.r5678, queens|other.queens)
	def isGoodEnough = total>=140 && r5678>=36
	override def compare(that: CompMusic) = total.compare(that.total)
	override def toString = total+"/"+r5678+(if (queens) "Q" else "")
}

object CompMusic
{
	val allMusic = new StandardMajorMusic
	val r5678music = new Music56Rollup()
	def apply(): CompMusic = CompMusic(0, 0, false)
	def apply(perm: Perm, row: Row): CompMusic =
	{
		val permedRow = row.permuteBy(perm)
		CompMusic(allMusic.countMusic(permedRow), r5678music.countMusic(permedRow), permedRow==Row("13572468"))
	}
}

case class CompRotMusic(perRotation: List[CompMusic])
{
	def this(rows: Iterable[Row]) = this(rows.map(compMusicForRows).reduce{_+_}.perRotation)

	def this() = this(List.fill(23)(CompMusic()))

	def +(other: CompRotMusic) = CompRotMusic( (perRotation,other.perRotation).zipped.map{_+_})

	def add(rows: Iterable[Row]) = rows.map(compMusicForRows).foldRight(this){_+_}

	def bestRot: (CompMusic,Row) = perRotation.zip(compRotations).maxBy{_._1}
}

abstract class CumulativeSliceMusic[M, T <: CumulativeSliceMusic[M,T]](val perSlice: Map[Int,M])
{
	lazy val cumulative = new mutable.ArraySeq[M](8)
	var cumulativeFill = 0

	def total: M = getCumulative(8)

	def getCumulative(uptoPos: Int) =
	{
		if (cumulativeFill==0)
		{
			cumulativeFill = perSlice.keys.min
			cumulative(cumulativeFill-1) = perSlice(cumulativeFill)
		}
		while (cumulativeFill < uptoPos)
		{
			cumulative(cumulativeFill) = addMusic(cumulative(cumulativeFill-1), perSlice(cumulativeFill+1))
			cumulativeFill+= 1
		}
		cumulative(uptoPos-1)
	}

	protected def addMusic(a: M, b: M): M

	protected def doAdd(pair: (Int, M)): Map[Int,M] =
	{
		val treblePos = pair._1
		var newMusic = pair._2
		if (perSlice.contains(treblePos))
			newMusic = addMusic(newMusic, perSlice(treblePos))
		perSlice + (treblePos->newMusic)
	}
}

class MethodSliceMusic(perSlice0: Map[Int,Int]) extends CumulativeSliceMusic[Int,MethodSliceMusic](perSlice0)
{
	def this() = this(Map[Int,Int]())

	def add(row: Row) = new MethodSliceMusic(doAdd(calcMusic(row)))

	override protected def addMusic(a: Int, b: Int) = a+b

	private def calcMusic(row: Row) =
	{
		def score4(four: Array[Int]) =
		{
			if (Row.isCoursingSet(four, 8))
				if (Row.isRunPosition(four, 8))
					2
				else
					1
			else
				0
		}
		def newScore(four: Array[Int]) =
		{
			val pcRun = if (Row.isRunPosition(four, 8)) 2 else 0
			if (Row.isCoursingPair(four(2), four(3), 8))
				if (Row.isCoursingSet(four, 8))
					pcRun+3
				else if (four(0)==1 || four(1)==1)
					0
				else
					pcRun+1
			else
				pcRun*2
		}

		// Always return 2 for rounds (even though actual scoring function gives higher) - doesn't outweigh other changes then.
		if (row.isRounds)
			(1, 2)
		else
		{
			val bells = row.asBellNumbers.toArray
			//score4(bells.slice(0,4))+score4(bells.slice(4,8))
			val score = newScore(bells.slice(0,4).reverse)+newScore(bells.slice(4,8))
			(row.placeOf(1), score)
		}
	}
}

class CompSliceMusic(perSlice0: Map[Int,CompRotMusic]) extends CumulativeSliceMusic[CompRotMusic,CompSliceMusic](perSlice0)
{
	def this() = this(Map())

	override protected def addMusic(a: CompRotMusic, b: CompRotMusic) = a+b

	def add(rows: List[Row]) = new CompSliceMusic(doAdd(calcMusic(rows)))
	def add(treblePos: Int, music: CompRotMusic) = new CompSliceMusic(doAdd(Tuple2(treblePos, music)))

	private def calcMusic(rows: List[Row]) = (rows.head.placeOf(1), rows.map(compMusicForRows).reduce{_+_})
}

/** Holds both method music and comp music per slice. Both gave getCumulative methods, for use once all slices are populated. */
case class MusicTracker(musicRow: Row, methodMusic: MethodSliceMusic, compSliceMusic: CompSliceMusic)
{
	def this(initialMusicRow: Row, initialCompRows: List[Row]) =
		this(initialMusicRow, new MethodSliceMusic().add(initialMusicRow), new CompSliceMusic().add(initialCompRows))
	/** For use from Stalagmites, where method music is not tracked */
	def this(initialCompRows: List[Row]) =
		this(Row(8), new MethodSliceMusic(), new CompSliceMusic().add(initialCompRows))

	def getMethodMusic(treblePos: Int) = methodMusic.getCumulative(treblePos)
	def getCompMusic(treblePos: Int) = compSliceMusic.perSlice(treblePos)
	def getTotalCompMusic() = compSliceMusic.getCumulative(8)

	/** Adds to method and comp music */
	def next(pn: PN, newHeadRows: List[Row]) =
	{
		val newMusicRow = musicRow.apply(pn)
		val newMethodMusic = methodMusic.add(newMusicRow)
		val newCompMusic = compSliceMusic.add(newHeadRows)
		MusicTracker(newMusicRow, newMethodMusic, newCompMusic)
	}

	/** This one only does the method music, leaving the compMusic fixed where it was */
	def next(pn: PN) =
	{
		val newMusicRow = musicRow.apply(pn)
		val newMethodMusic = methodMusic.add(newMusicRow)
		MusicTracker(newMusicRow, newMethodMusic, compSliceMusic)
	}

	/** Adds to comp music only, setting the method music to 0 (for use from initial Stalagmite build) */
	def add(newHeadRows: List[Row]) =
	{
		val newCompMusic = compSliceMusic.add(newHeadRows)
		val treblePos = newHeadRows.head.placeOf(1)
		val newMethodMusic = new MethodSliceMusic(methodMusic.perSlice + (treblePos->0))
		MusicTracker(musicRow, newMethodMusic, newCompMusic)
	}

	def addStalagCompMusic(treblePos: Int, sliceMusic: CompRotMusic) =
		MusicTracker(musicRow, methodMusic, compSliceMusic.add(treblePos, sliceMusic))

}


