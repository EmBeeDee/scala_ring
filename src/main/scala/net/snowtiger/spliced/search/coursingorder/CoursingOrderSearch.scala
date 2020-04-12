package net.snowtiger.spliced.search.coursingorder

import net.snowtiger.ringing.Row

/**
 * @author mark
 */

class CoursingOrderSearch
{
	def coFromCH(ch: String) = CoursingOrder( Row(ch).coursingOrder() )

	trait CallType
	{
		def permute(co: CoursingOrder):CoursingOrder
		def permuteSingle(co: CoursingOrder):CoursingOrder
		def name: String
		override def toString = name
	}

	case class Before() extends CallType
	{
		def permute(co: CoursingOrder) = co.before
		def permuteSingle(co: CoursingOrder) = throw new UnsupportedOperationException("No single Befores")
		def name = "B"
	}

	case class Wrong() extends CallType
	{
		def permute(co: CoursingOrder) = co.wrong
		def permuteSingle(co: CoursingOrder) = co.singleWrong
		def name = "W"
	}

	case class Middle() extends CallType
	{
		def permute(co: CoursingOrder) = co.middle
		def permuteSingle(co: CoursingOrder) = co.singleMiddle
		def name = "M"
	}

	case class Home() extends CallType
	{
		def permute(co: CoursingOrder) = co.home
		def permuteSingle(co: CoursingOrder) = co.singleHome
		def name = "H"
	}
}