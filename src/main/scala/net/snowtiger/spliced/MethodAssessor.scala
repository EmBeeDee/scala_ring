package net.snowtiger.spliced

import net.snowtiger.ringing.{Method, MethodAnalysis, NamedMethod}

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * @author mark
 */

case class MethodAssessor(nBells: Int) extends NiceMethods
{
	val longestAcceptableDodging = nBells+2
	val longestAcceptable4PlaceWork = nBells
	val good4Runs = nBells*2
	val veryGood4Runs = nBells*3

	type MethodFilter = (NamedMethod)=>Boolean

	def isHalfDecent(m: MethodAnalysis) = m.isPlainBobType && m.isDecentPN

	def isDecent(m: MethodAnalysis) =	m.isPlainBobType && m.isGoodPN

	def isPrettyGood(m: MethodAnalysis) =
		isDecent(m) && m.longestRunInSameDodgingPosition<=longestAcceptableDodging && m.longestRunOfSameBells(m.bellsInPosition(1, 4))<=longestAcceptable4PlaceWork

	def isGood(m: MethodAnalysis) =	isPrettyGood(m) && m.potential4Runs>=good4Runs
	def isVeryGood(m: MethodAnalysis) =	isPrettyGood(m) && m.potential4Runs>=veryGood4Runs
	def isGoodRoyal(m: MethodAnalysis) = isPrettyGood(m) && m.count5Runs>=22

	def isTricky(m: MethodAnalysis) = m.difficulty>=60

	def hasNewWork(existingMethods: List[NamedMethod]): MethodFilter =
		(m: NamedMethod) => existingMethods.forall{ _.differentWorks(m) }

	private def differentStart(m1: NamedMethod, m2: NamedMethod) =
		m1.lead.head!=m2.lead.head || (m1.lead.head.isCross && m1.lead.tail.head!=m2.lead.tail.head)

	def hasNewStart(existingMethods: List[NamedMethod]): MethodFilter =
		(m: NamedMethod) => existingMethods.forall{ differentStart(_, m) }

	private def differentLHGroup(m1: NamedMethod, m2: NamedMethod) =
		m1.firstLeadHead!=m2.firstLeadHead || m1.leadheadPN!=m2.leadheadPN

	def hasNewLHGroup(existingMethods: List[NamedMethod]): MethodFilter =
		(m: NamedMethod) => existingMethods.forall{ differentLHGroup(_, m) }

	def parseMethods(file: Source): List[NamedMethod] =
	{
		val buf = ListBuffer[NamedMethod]()
		var namePos = -1
		var notationPos = -1
		var hlPos = -1
		for (line <- file.getLines())
		{
			val tokens = line.split(",")
			if (tokens.size==3 && tokens(2).size<3)
			{
				// Old csv format
				val name = tokens(0)
				val pn = tokens(1)
				val lh = tokens(2)
				val m = new Method(nBells, pn, lh)
				val method = new NamedMethod(name, "*", m)
				buf.append(method)
			}
			else
			{
				// CCCBR library format
				// Tony Smith library format
				if (line.startsWith(" No. Name") && line.contains("Notation") && line.contains(" hl "))
				{
					namePos = line.indexOf("Name")
					notationPos = line.indexOf("Notation")
					hlPos = line.indexOf("hl")
				}
				else if (!line.isEmpty && line.head.isLetter)
				{
					hlPos = -1
				}
				else
				{
					try
					{
						val n = line.substring(0,namePos).trim.toInt
						if (hlPos>0 && line.length>hlPos)
						{
							val name = line.substring(namePos, notationPos).trim
							val pn = line.substring(notationPos, hlPos+2).trim
							val lh = line.substring(hlPos+3, hlPos+5)
							val m = new Method(nBells, pn, lh)
							// Uses full name as abbrev!
							val method = new NamedMethod(name, name, m)
							buf.append(method)
						}
					}
					catch
					{
						case e: Exception => // no-op
					}
				}
			}
		}
		println(buf.size+" methods parsed.")
		buf.toList
	}

}

object MethodAssessor
{
	val NBells = 10

	val methodAssessor = MethodAssessor(NBells)

	def main(args: Array[String])
	{
		if (args.length!=1)
			println("Must supply single file argument")
		else
			parseMethods(args(0))
	}

	def parseMethods(filename: String)
	{
		println("Parsing methods from file "+filename)
		var goodMethods = methodAssessor.parseMethods(Source.fromFile(filename))
		//goodMethods = filterByNewWork(goodMethods, List(london, bristol)) //.filter(_.lead.head==PN("58"))
		//goodMethods = goodMethods.filter{_.workBelow==hanwell.workBelow}
		goodMethods = goodMethods.filter{methodAssessor.isGoodRoyal(_)}
		goodMethods = goodMethods.filter{_.isPlainBobType}
		goodMethods = goodMethods.filter(_.isSymmetric)
		goodMethods = goodMethods.filter(_.wrongPlaceFactor>0)
		def comp4runs(p1: NamedMethod, p2: NamedMethod) =
		{
			val r1 = p1.potential4Runs
			val r2 = p2.potential4Runs
			if (r1==r2)
				p1.name < p2.name
			else
				r1 > r2
		}
		def comp5runs(p1: NamedMethod, p2: NamedMethod) =
		{
			val r1 = p1.count5Runs
			val r2 = p2.count5Runs
			if (r1==r2)
				p1.name < p2.name
			else
				r1 > r2
		}
		def compDiff(p1: NamedMethod, p2: NamedMethod) = p1.difficulty > p2.difficulty
		val sorted = goodMethods.sortWith( comp5runs )
		for (method <- sorted)
			println(method.count5Runs+" "+method.name+" "+method.lhGroup)
		//println(method.difficulty+" "+method.potential4Runs+" "+method.name)
	}


}