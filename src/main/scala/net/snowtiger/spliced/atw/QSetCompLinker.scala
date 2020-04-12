package net.snowtiger.spliced.atw

import net.snowtiger.spliced.search.coursingorder.{CoursingOrder, CoursingOrderSearch}

import scala.collection.mutable.ListBuffer

/**
 * Attempts to link a set of coursing orders using M, W and H, bobs and singles, ensuring that entire courses of each CO are rung.
 * WARNING: The calling position order is assumed to be MWH - if your method or splice gives WMH, it should still be possible to
 * derive true compositions from the output, but they may be linked together differently.
 */
object QSetCompLinker extends CoursingOrderSearch
{
	val AllCalls = List(Middle(), Wrong(), Home())

	val Start = CoursingOrder.Start

	val CosToLink = Set("53246", "43526", "63425", "65324", "34625", "53624", "43652", "43256", "53426", "63524", "64325", "35624", "43625", "53642")

	def main(args: Array[String])
	{
		for (comp <- search(CosToLink.map{CoursingOrder(_)}, true))
			println(comp)
	}

	def search(cosToLink: Set[CoursingOrder], findAll: Boolean) =
	{
		val nextCall = Map[CallType,CallType](Home()->Middle(), Middle()->Wrong(), Wrong()->Home())
		val requiredLength = cosToLink.size*3
		val results = ListBuffer[String]()

		def search(co: CoursingOrder, call: CallType, revComp: List[String], cosUsed: Map[CallType, Set[CoursingOrder]], length: Int): Boolean =
		{
			var found = false
			if (length==requiredLength && co==Start)
			{
				results+= revComp.reverse.mkString(" ")
				found = true
			}
			else if (cosToLink.contains(co))
			{
				val oldUsed = cosUsed(call)
				val newUsed = oldUsed+co
				if (newUsed.size>oldUsed.size)
				{
					val newCosUsed = cosUsed+(call->newUsed)
					val newCall = nextCall(call)
					val newLength = length+1
					found = search(co, newCall, revComp, newCosUsed, newLength)
					if (findAll || !found)
						found|= search(newCall.permute(co), newCall, newCall.name::revComp, newCosUsed, newLength)
					if (findAll || !found)
						found|= search(newCall.permuteSingle(co), newCall, "s"+newCall.name::revComp, newCosUsed, newLength)
				}
			}
			found
		}

		assert(cosToLink.contains(Start))
		val empty = Set[CoursingOrder]()
		search(Start, Home(), Nil, Map(Home()->empty, Middle()->empty, Wrong()->empty), 0)
		results.toList
	}

	/** COs must have had their links populated */
	def searchByCOLinks(start: CoursingOrder, cosToLink: Set[CoursingOrder], findAll: Boolean) =
	{
		val nextCall = Map[CallType,CallType](Home()->Middle(), Middle()->Wrong(), Wrong()->Home())
		val requiredLength = cosToLink.size*3
		val results = ListBuffer[String]()

		def search(co: CoursingOrder, call: CallType, revComp: List[String], cosUsed: Map[CallType, Set[CoursingOrder]], length: Int): Boolean =
		{
			var found = false
			if (length==requiredLength && co==Start)
			{
				results+= revComp.reverse.mkString(" ")
				found = true
			}
			else if (cosToLink.contains(co))
			{
				val oldUsed = cosUsed(call)
				val newUsed = oldUsed+co
				if (newUsed.size>oldUsed.size)
				{
					val newCosUsed = cosUsed+(call->newUsed)
					val newCall = nextCall(call)
					val newLength = length+1
					found = search(co, newCall, revComp, newCosUsed, newLength)
					def searchWithCall(callStr: String) =
					{
						co.callLinks.get(callStr) match
						{
							case Some(co2) => search(co2, newCall, callStr::revComp, newCosUsed, newLength)
							case None => false
						}
					}
					if (findAll || !found)
						found|= searchWithCall(newCall.name)
					if (findAll || !found)
						found|= searchWithCall("s"+newCall.name)
				}
			}
			found
		}

		assert(cosToLink.contains(start))
		val empty = Set[CoursingOrder]()
		search(start, Home(), Nil, Map(Home()->empty, Middle()->empty, Wrong()->empty), 0)
		results.toList
	}

}