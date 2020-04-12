package net.snowtiger.ringing

import scala.collection.mutable.ListBuffer

/**
 * @author Mark
 */
case class Perm(val perm: String)
{
	import Row.Rounds

	def this(p: Seq[Char]) = this(p.mkString)
	def this(row: Row) = this(row.bells)

	val size = perm.size

	lazy val order = generateGroup.size

	/**
	 * Permutes the sequence of bells in the current perm by the permutation given by the other.
	 * It is the bells not the places which are permuted.
	 * For example, if this=4231 and other=2314, bells 123 will be rotated to give 4312.
	 * If you wish to permute by the places in "other", simply reverse the direction of the call: other.permuteBy(this)
	 * @param other
	 * @return
	 */
	def permuteBy(other: Perm): Perm =
	{
		assert(size == other.size)
		val result = for (i <- 0 until size) yield other.perm(Rounds.indexOf(perm(i)))
		new Perm(result)
	}

	/**
	 * Applies a perm to itself n times. For example, 231*2 = 312.
	 * @param n
	 * @return
	 */
	def *(n: Int): Perm =
	{
		if (n==0)
			new Perm(Row(size))
		else if (n<0)
			inverse*n
		else
		{
			var perm = this
			1 until n foreach{ (n)=> perm = perm.permuteBy(this) }
			perm
		}
	}

	def inverse: Perm =
	{
		val result = for (i <-0 until size) yield Rounds(perm.indexOf(Rounds(i)))
		new Perm(result)
	}

	def isSelfInverse = this==inverse

	/** Identity perm first, then all perms generated from that by this. */
	def generateGroup: List[Perm] =
	{
		val identity = Row(size).toPerm
		val perms = ListBuffer(identity)
		var nextPerm = this
		while (nextPerm!=identity)
		{
			perms+= nextPerm
			nextPerm = nextPerm.permuteBy(this)
		}
		perms.toList
	}

	def generateGroupFrom(row: Row): List[Row] = generateGroup.map(row.permuteBy)
}