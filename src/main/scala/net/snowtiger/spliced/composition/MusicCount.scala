package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{Music, Row}

/**
 * Simple immutable wrapper for an integer music score; can also count up secondary music scores, which are maintained
 * separately, but only enter comparisons with other music count instances if the main score is the same.
 *
 * @author mark
 */
class MusicCount(val defs: Array[Music]) extends Ordered[MusicCount] with Subtractable[MusicCount]
{
	private val counts = new Array[Int](defs.size)

	def +(other: MusicCount): MusicCount =
	{
		val newMusic = new MusicCount(defs)
		for (i <- 0 until counts.size)
			newMusic.counts(i) = counts(i) + other.counts(i)
		newMusic
	}

	def -(other: MusicCount): MusicCount =
	{
		val newMusic = new MusicCount(defs)
		for (i <- 0 until counts.size)
			newMusic.counts(i) = counts(i) - other.counts(i)
		newMusic
	}

	override def clone(): MusicCount =
	{
		val newMusic = new MusicCount(defs)
		for (i <- 0 until counts.size)
			newMusic.counts(i) = counts(i)
		newMusic
	}

	def count(rows: Iterable[Row]): MusicCount =
	{
		val newMusic = clone()
		for (row <- rows; i <- 0 until defs.size)
			newMusic.counts(i)+= defs(i).countMusic(row)
		newMusic
	}

	def apply(i: Int) = counts(i)

	def compare(that: MusicCount) =
	{
		var c = 0
		var i = 0
		while (c==0 && i<counts.size)
		{
			c = counts(i).compare(that.counts(i))
			i+= 1
		}
		c
	}

	override def toString = counts.mkString("/")
}

object MusicCount
{
	def sumAll(musicCounts: Iterable[MusicCount]): MusicCount =
	{
		sumAll(musicCounts.head, musicCounts.tail)
	}

	def sumAll(initialMusic: MusicCount, musicCounts: Iterable[MusicCount]): MusicCount =
	{
		val newMusic = initialMusic.clone()
		for (music <- musicCounts; i <- 0 until initialMusic.defs.size)
			newMusic.counts(i)+= music(i)
		newMusic
	}

}