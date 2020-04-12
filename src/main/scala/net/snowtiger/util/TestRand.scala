package net.snowtiger.util

import scala.util.Random

/**
 * Monte-carlo Beetle simulation
 *
 * @author mark
 */

object TestRand
{
	def main(args: Array[String]): Unit =
	{
		//println(monteCarlo(1.0/6, 10000000))
		println(monteCarloBeetle(100000000))
	}

	def monteCarlo(p: Double, n: Int): Double =
	{
		var tot = 0L
		var i = n
		while (i>0)
		{
			tot+= countTillEvent(p)
			i-= 1
		}
		tot.toDouble/n
	}

	def countTillEvent(p: Double) =
	{
		var c = 0
		var achieved = false
		while (!achieved)
		{
			c+= 1
			val result = Random.nextDouble()
			achieved = result < p
		}
		c
	}

	def monteCarloBeetle(n: Int): Double =
	{
		var tot = 0L
		var i = n
		while (i>0)
		{
			tot+= countBeetle()
			i-= 1
		}
		tot.toDouble/n
	}

	def dieThrow = 1+Random.nextInt(6)

	def countBeetle() =
	{
		var c = 1
		var head = 0
		var tail = 0
		var legs = 0
		var antennae = 0
		var eyes = 0
		while (dieThrow!=6)
			c+= 1
		while (eyes<2 || antennae<2 || legs<6 || tail<1)
		{
			c+= 1
			dieThrow match
			{
				case 1 => if (head>0 && eyes<2) eyes+= 1
				case 2 => if (head>0 && antennae<2) antennae+= 1
				case 3 => if (legs<6) legs+= 1
				case 4 => if (tail<1) tail+= 1
				case 5 => if (head<1) head+=1
				case _ =>
			}
		}
		c
	}
}