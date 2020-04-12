package net.snowtiger.spliced.search.coursingorder

import net.snowtiger.ringing.{Perm, Row}

/**
 * @author mark
 */

case class CoursingOrder(raw: String)
{
	lazy val positive = Row("1"+raw).positive
	var linksPopulated = false
	var callLinks: Map[String, CoursingOrder] = null
	var relations: Set[CoursingOrder] = null

	def populateLinks(calls: List[String], allCOs: Map[String, CoursingOrder])
	{
		callLinks = Map[String, CoursingOrder]() ++ calls.map{(c)=> (c, allCOs(call(c).raw))}
		def genRelations(bob: String)
		{
			relations+= allCOs(call(bob).raw)
			relations+= allCOs(call(bob).call(bob).raw)
			val single = "s"+bob
			val singled = call(single)
			if (allCOs.contains(singled.raw))
			{
				relations+= allCOs(singled.raw)
				relations+= allCOs(singled.call(bob).raw)
				relations+= allCOs(singled.call(bob).call(bob).raw)
			}
		}
		relations = Set[CoursingOrder]()
		genRelations("M")
		genRelations("W")
		genRelations("H")
		linksPopulated = true
	}

	def before = CoursingOrder(""+raw(4)+raw.substring(0,4))
	def middle = CoursingOrder(raw.substring(0,2)+raw.substring(3,5)+raw(2))
	def wrong = CoursingOrder(raw.substring(1,3)+raw(0)+raw.substring(3,5))
	def home = CoursingOrder(""+raw(0)+raw.substring(2,4)+raw(1)+raw(4))
	def singleMiddle = CoursingOrder(raw.substring(0,2)+raw.substring(2,5).reverse)
	def singleWrong = CoursingOrder(raw.substring(0,3).reverse+raw.substring(3,5))
	def singleHome = CoursingOrder(""+raw(0)+raw.substring(1,4).reverse+raw(4))

	def call(c: String) =
	{
		if (c.startsWith("s"))
		{
			c.tail match
			{
				case "H" => singleHome
				case "M" => singleMiddle
				case "W" => singleWrong
				case _ => throw new UnsupportedOperationException("Invalid call: "+c)
			}
		}
		else
		{
			c match
			{
				case "H" => home
				case "M" => middle
				case "W" => wrong
				case "B" => before
				case _ => throw new UnsupportedOperationException("Invalid call: "+c)
			}
		}
	}

	lazy val sign = new Row(raw).signChar

	def callBetween(other: CoursingOrder) =
	{
		if (""+raw(0)+raw(4) == ""+other.raw(0)+other.raw(4))
			"H"
		else if (raw.substring(3,5)==other.raw.substring(3,5))
			"W"
		else if (raw.substring(0,2)==other.raw.substring(0,2))
			"M"
		//else if (before==other || other.before==this)
		//	"B"
		else
			"-"
	}

	def coPerm(to: CoursingOrder) =
	{
		val from = this
		val r1 = Row(from.raw).shift(-1)
		val r2 = Row(to.raw).shift(-1)
		val perm = r1.permutationBetween(r2)
		val check = r1.permuteBy(perm)
		perm
	}

	def permCO(perm: Perm) =
	{
		val r1 = Row(this.raw).shift(-1)
		val r2 = r1.permuteBy(perm)
		CoursingOrder(r2.shift(1).toString)
	}

	def toCourseHead(nbells: Int) = Row("1"+raw(2)+raw(1)+raw(3)+raw(0)+raw(4)+Row.Rounds.substring(6, nbells))

	override def toString = raw
}

object CoursingOrder
{
	val all = Row.generateAll(5).map{ (r)=> CoursingOrder(r.shift(1).toString)}
	val (positive, negative) = all.partition{_.sign=="+"}
	val Start = CoursingOrder("53246")
}