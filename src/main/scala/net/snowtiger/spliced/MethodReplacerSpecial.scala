package net.snowtiger.spliced

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Music, NamedMethod, PN, Row}
import net.snowtiger.special.spliced.CyclicSpliceSearch
import net.snowtiger.spliced.MethodReplacer.PruneVals
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.score.MusicRun
import net.snowtiger.spliced.tables.Lead

import scala.collection.mutable.ListBuffer


/**
 * @author mark
 */

object MethodReplacerSpecial
{
	val nbells = 10
	val harleston2 = NamedMethod("Harleston No.2", 10, "-38-1478-70-38-1470-38-14-3470-38-90", "10")
	val pbRoyal = NamedMethod("Plain Bob", 10, "-10-10-10-10-10", "12")
	val tatlowDiff = NamedMethod("Differential", 10, "-14-16-18-10-10-10-10-10-10-10")
	val triton = NamedMethod("Triton", 10, "30-30.14-12-10-12-50.14-14.70.16-18.90", "12")

	def main(args: Array[String]): Unit =
	{
		val openingPns = List(PN("34"), PN("36"), PN("50"), PN("30"), PN("-"))
		val allPns = GoodPn(nbells).oneConsec.toSet
		val methodPns = openingPns::openingPns::openingPns::(MethodReplacer.trebleDodgingPns(nbells, allPns).tail.tail.tail)

		val musicDefs: Array[Music] = Array(new MusicRun(4), new MusicRun(5), new MusicRun(6))
		val musicCount = new MusicCount(musicDefs)

		val tatlowComp = generateLeadsOfCyclicComp(tatlowCompFirstPart)
		replace("Tatlow Spliced", tatlowComp, harleston2, methodPns, musicCount)
	}

	def tatlowCompFirstPart: Map[NamedMethod,List[Row]] =
	{
		// Comp starts with four leads of Triton
		val tritonLeads = genPlainLeads(Row("1234567890"), triton, 4)
		// Then one lead of the Differential
		val differentialLeads = genPlainLeads(Row("1089674523"), tatlowDiff, 1)
		// After a 14 bob, nine leads of Harleston No.2
		val harlestonLeads = genPlainLeads(Row("1023495867"), harleston2, 9)
		// After a 16 bob, one lead of PB
		val pbLeads = genPlainLeads(Row("1023456978"), pbRoyal, 1)
		Map(triton->tritonLeads, tatlowDiff->differentialLeads, harleston2->harlestonLeads, pbRoyal->pbLeads)
	}

	def genPlainLeads(startLH: Row, method: NamedMethod, n: Int): List[Row] =
	{
		val lhs = ListBuffer[Row]()
		var lh = startLH
		for (i <- 0 until n)
		{
			lhs += lh
			lh = lh.apply(method.plainPerm)
		}
		lhs.toList
	}

	def generateLeadsOfCyclicComp(firstPart: Map[NamedMethod,List[Row]]): Map[NamedMethod, List[Lead]] =
	{
		def genLeadsForMethod(method: NamedMethod): (NamedMethod, List[Lead]) =
		{
			val lhs = firstPart(method)
			val leads = lhs.flatMap(CyclicSpliceSearch.genCyclicLeadHeads(method,_)).map(Lead(_, method))
			(method, leads)
		}
		firstPart.keys.map(genLeadsForMethod).toMap
	}

	def rowFn(nRows: Int) = if (nRows<6) 0 else if (nRows<8) nRows/2 else if (nRows<12) nRows-1 else (nRows*1.5).toInt
	//def rowFn(nRows: Int) = if (nRows<6) 0 else if (nRows<12) nRows-1 else nRows+1

	def replace(compDesc: String, comp: Map[NamedMethod,List[Lead]], methodToReplace: NamedMethod, methodPns: List[List[PN]], musicCount: MusicCount): Unit =
	{
		val leadsToReplace = comp(methodToReplace)
		val otherRows = comp.filterKeys(_!=methodToReplace).values.flatten.map(_.getRows).flatten.toSet

		val musicToReplace = musicCount.count(leadsToReplace.map(_.getRows).flatten)
		println("Original music = "+musicToReplace)
		val currentMusic = musicToReplace(0)+musicToReplace(2)
		val currentMusicPerRow = currentMusic.toFloat/(nbells*2)

		def pruner(level: Int, currentRows: List[Row], pv: PruneVals): Option[PruneVals] =
		{
			def rowToPositions(row: Row): List[Set[Char]] = row.bells.grouped(2).map(_.toSet).toList
			def notStatic(positions: List[Option[Set[Char]]], prevRows: List[Row], nStatic: Int): Boolean =
			{
				if (positions.forall(_.isEmpty))
					true
				else if (nStatic>10)
					false
				else if (prevRows.isEmpty)
					true
				else
				{
					val rowPositions = rowToPositions(prevRows.head)
					val nextPositions = rowPositions.zip(positions).map((p)=> if (p._2.isDefined && p._2.get==p._1) Some(p._1) else None)
					notStatic(nextPositions, prevRows.tail, nStatic+1)
				}
			}
			val nRows = pv.firstHLRows.size
			val effectiveRows = rowFn(nRows)
			val expectedMusic = effectiveRows*currentMusicPerRow
			val newMusic = pv.nextMusic(currentRows)
			val musicScore = newMusic(0)+newMusic(2)
			val musicGood = musicScore>=expectedMusic
			if (musicGood && notStatic(rowToPositions(currentRows.head).map(Some(_)), pv.firstHLRows, 0))
				Some(pv.next(currentRows, newMusic))
			else
				None
		}

		MethodReplacer.replaceMethods(compDesc, List(methodToReplace), methodPns, musicCount, otherRows, leadsToReplace, 1, pruner)
	}


}