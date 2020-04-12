package net.snowtiger.ringing

import scala.collection.immutable

/**
 * @author mark
 */

trait FCHCalculator
{
	this: NamedMethod =>

	lazy val (positiveFCH, negativeFCH) = calcFCH()

	lazy val fch = if (!isPlainBobType || nLeads!=nbells-1) "?" else if (nbells<8) "" else if (nbells==8) fchMajor else fchHigherNumbers

	def fchMajor = {
		val combined = positiveFCH.mkString("")+negativeFCH.mkString("").dropWhile(!_.isLower)
		combined.replaceAll("1.2", "")
	}
	def fchHigherNumbers = positiveFCH.mkString("")+"/"+negativeFCH.mkString("")

	def calcFCH(): (List[String], List[String]) =
	{
		val lhPerm = firstLeadHead.toPerm
		val byTreblePos = firstLead.groupBy(_.placeOf(1))
		val fch: Set[String] = byTreblePos.values.map{(xs)=> calcFCH(lhPerm, xs)}.toSet.flatten
		val backBells = Row.Rounds.substring(6,nbells)
		val tenorsTogether = fch.filter(_.substring(6)==backBells).map(_.substring(1,6))
		val posFCH = FCH.pos.filter(_._2.exists(tenorsTogether.contains)).keys.toList
		val negFCH = FCH.neg.filter(_._2.exists(tenorsTogether.contains)).keys.toList
		(posFCH, negFCH)
	}

	def calcFCH(lhPerm: Perm, firstLeadRows: List[Row]): Set[String] =
	{
		def fchFrom(row1: Row, row2: Row): String =
		{
			val permBetween = row1.permutationBetween(row2)
			val falseLH = Row(permBetween)
			val falseCH = falseLH.rotateUntilBellHome(nbells, lhPerm)
			if (falseCH.isEmpty)
				println("hum")
			falseCH.get.toString
		}

		def calcOnePos(row: Row, otherRows: List[Row]): Set[String] =
			otherRows.map(fchFrom(row, _)).toSet

		def calcAll(allRows: List[List[Row]]): Set[String] =
			allRows match
			{
				case rows :: remaining => calcOnePos(rows.head, remaining.flatten) ++ calcAll(remaining)
				case Nil => Set()
			}

		val courseRows = firstLeadRows.map(lhPerm.generateGroupFrom)
		calcAll(courseRows)
	}

}

object FCH
{
	val pos: Map[String,Set[String]] = immutable.ListMap(
		"A" -> Set("23456"),
		"B" -> Set("24365"),
		"C" -> Set("25634"),
		"D" -> Set("32546","46253"),
		"E" -> Set("32465","43265"),
		"F" -> Set("32654","45236"),
		"G" -> Set("56423","63542"),
		"H" -> Set("53462","63425"),
		"I" -> Set("54632","65324"),
		"K" -> Set("53624","65432"),
		"L1" -> Set("26543"),
		"L2" -> Set("36245","42563"),
		"M" -> Set("23564","23645","25463","26435"),
		"N" -> Set("34562","46325","54263","62345"),
		"O" -> Set("36524","46532","52643","65243"),
		"P1" -> Set("54326","64352"),
		"P2" -> Set("56342","64523"),
		"R" -> Set("35642","45623","56234","62534"),
		"S" -> Set("34625","45362","52364","64235"),
		"T" -> Set("24536","24653","25346","26354","36452","43526","53246","62453"),
		"U1" -> Set("34256","35426","42356","43652","52436","63254"),
		"U2" -> Set("35264","42635"))

	val neg: Map[String,Set[String]] = immutable.ListMap(
		"B" -> Set("23654","25436","32456","43256"),
		"C" -> Set("34265","42365"),
		"D" -> Set("24356","53426","63452"),
		"E" -> Set("24635","25364"),
		"F" -> Set("24563","26345"),
		"H" -> Set("36542","46523","56243","62543"),
		"K1" -> Set("34526","46352","52346","64253"),
		"K2" -> Set("54362","64325"),
		"N1" -> Set("35462","43625","53264","62435"),
		"N2" -> Set("35624","45632","52634","65234"),
		"O" -> Set("53642","56432","63524","65423"),
		"T" -> Set("32564","32645","45263","46235"),
		"a1" -> Set("23465"),
		"a2" -> Set("23546","26453"),
		"b" -> Set("25643","26534"),
		"c" -> Set("35246","36254","42536","42653"),
		"d" -> Set("34652","45326","54236","62354"),
		"e" -> Set("36425","43562","52463","63245"),
		"f" -> Set("54623","56324","64532","65342"))

	val split: Map[String,Set[String]] = immutable.ListMap(
		"X" -> Set("257643","374652","627534","723645","265743","437625","632754","724653","276354","457632","635742","736425","276435","475623","657423","746532","346752","526734","672453","762354","367245","546723","673245","763542","367524","564732","675324","764235"),
		"Y" -> Set("267534","364752","625743","724635","275643","367542","635724","726534","276453","376425","637425","734652","675423","764532"),
		"Z" -> Set("457623","546732","672354","763245"))

	def main(args: Array[String])
	{
		println(pos.values.flatten.toSet.size)
	}
}