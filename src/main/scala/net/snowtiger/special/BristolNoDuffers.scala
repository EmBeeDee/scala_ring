package net.snowtiger.special

import net.snowtiger.ringing._
import net.snowtiger.spliced.NiceMethods
import net.snowtiger.spliced.score._

import scala.collection.mutable

/**
 * @author mark
 */

object BristolNoDuffers extends NiceMethods
{
	val leadLength = bristol.leadLength
	val plainPerm = bristol.plainPerm
	val bobPerm = bristol.callPerm(PN("14"))
	val singlePerm = bristol.callPerm(PN("1234"))
	val calls = Map('p'->plainPerm, '-'->bobPerm, 's'->singlePerm)
	val MinLength = 5000

	val musicDefs = Array(new MusicRun(4), new Music56Rollup, new Music65Rollup, new MusicBack4Combinations, new MusicQueens, new MusicQueensRow, new MusicWhittingtonsRow)
	val foreCallings = Map("ppppp"->"", "pp-pp"->"B")
	val aftCallings = Map("pp"->"", "-pp"->"W ", "p-p"->"H ", "pp-"->"M ", "--pp"->"2W ", "-p-p"->"WH ", "-pp-"->"WM ", "p--p"->"2H ", "p-p-"->"HM ", "pp--"->"2M ")
	val initialCallings = Map("p"->"", "-p"->"H ", "p-"->"M", "-p-"->"H M", "--p"->"HH ", "p--"->"MM")
	val courseHeads = Row.generateAll(5).filter(_.positive).map(_.shift(1).extendTo(8))

	val startNodes = generateStartNodes()
	val foreNodes = generateForeNodes(courseHeads)
	val aftNodes = generateAftNodes(courseHeads)
	val allNodes = startNodes++foreNodes++aftNodes

	val foreNodeMap = foreNodes.groupBy(_.entry).toMap
	val aftNodeMap = aftNodes.groupBy(_.entry).toMap

	def main(args: Array[String]): Unit =
	{
		println("Finding false nodes...")
		startNodes.foreach(findFalse)
		foreNodes.foreach(findFalse)
		aftNodes.foreach(findFalse)

		println("Searching...")
		//val startNode = startNodes.find(_.desc=="").get
		for (startNode <- startNodes)
			searchFore(startNode.exit, List(startNode), Set(startNode))
	}

	def searchFore(lh: Row, revNodes: List[BristolNode], nodeSet: Set[BristolNode]): Unit =
	{
		for (node <- foreNodeMap.getOrElse(lh, Nil))
			if (node.falseNodes.forall(!nodeSet(_)))
				searchAft(node.exit, node::revNodes, nodeSet+node)
	}

	def searchAft(lh: Row, revNodes: List[BristolNode], nodeSet: Set[BristolNode]): Unit =
	{
		for (node <- aftNodeMap.getOrElse(lh, Nil) /*.sortBy(_.desc.length)*/)
			if (node.falseNodes.forall(!nodeSet(_)))
			{
				val comp = node::revNodes
				if (node.comesRound)
				{
					val length = comp.map(_.length).sum
					val music = comp.map(_.music).reduce((x,y)=> x.zip(y).map(p=>p._1+p._2))
					val score = music(0)+music(1)+music(2)
					if (length>=MinLength)
						println("" + length +" "+ score +" ("+ music.mkString(" ") +") "+ comp.reverse.mkString)
				}
				else
				{
					searchFore(node.exit, comp, nodeSet + node)
				}
			}
	}

	def findFalse(node: BristolNode): Unit = node.falseNodes = allNodes.filter(node.falseWith).toSet

	/** Start nodes are from Home->Middle */
	def generateStartNodes(): List[BristolNode] =
	{
		val nodeStart = Row(8)
		def makeNodes(entry: Row): Iterable[BristolNode] = initialCallings.map{(c)=> makeInitialNode(entry,c._1, c._2)}
		makeNodes(nodeStart).toList
	}


	/** Fore node is from the Middle->Wrong */
	def generateForeNodes(courseEnds: List[Row]): List[BristolNode] =
	{
		val nodeStarts = courseEnds.map(jumpToLead(1,_))
		def makeNodes(entry: Row): Iterable[BristolNode] = foreCallings.map{(c)=> makeForeNode(entry,c._1, c._2)}.flatten
		nodeStarts.map(makeNodes).flatten
	}

	/** Aft node is from the Wrong->Middle */
	def generateAftNodes(courseEnds: List[Row]): List[BristolNode] =
	{
		// All must end at a Middle lead, i.e. two plains required
		val nodeStarts = courseEnds.map(jumpToLead(6,_))
		def makeNodes(entry: Row): Iterable[BristolNode] = aftCallings.map{(c)=> makeAftNode(entry,c._1, c._2)}.flatten
		nodeStarts.map(makeNodes).flatten
	}

	def jumpToLead(n: Int, from: Row): Row =
	{
		var to = from
		for (i <- 1 to n)
			to = to.apply(plainPerm)
		to
	}

	case class BristolNode(rows: List[Row], val exit: Row, val desc: String)
	{
		def this(rowsPlusExtraLead: List[Row], desc: String) =
			this(removeLastLead(rowsPlusExtraLead), lastLeadhead(rowsPlusExtraLead), desc)

		val entry = rows.head
		val comesRound = exit.isRounds

		val length = rows.length
		val music = musicDefs.map(_.countMusic(rows))

		var falseNodes = Set[BristolNode]()

		def falseWith(other: BristolNode): Boolean = !rows.intersect(other.rows).isEmpty

		override def toString = desc
	}

	def removeLastLead(rows: List[Row]): List[Row] = rows.take(rows.length-leadLength)

	def lastLeadhead(rows: List[Row]): Row = rows(rows.length-leadLength)

	def generateRows(entry: Row, calling: String): List[Row] =
	{
		var lh = entry
		val leadheads = lh::calling.map{(c)=> lh = lh.apply(calls(c)); lh}.toList
		val buf = mutable.Buffer[Row]()
		leadheads.foreach(bristol.generateLead(_, buf))
		buf.toList
	}

	/** Returns None unless both halves of the node (before and after the Before) have music */
	def makeForeNode(entry: Row, calling: String, desc: String): Option[BristolNode] =
	{
		val rows = generateRows(entry, calling)
		val (firstHalf,secondHalf) = rows.splitAt(leadLength*3)
		val music1 = musicDefs.map(_.countMusic(firstHalf))
		val music2 = musicDefs.map(_.countMusic(secondHalf))
		if (music1.sum>=2 && music2.sum>=2)
			Some(new BristolNode(rows, desc))
		else
			None
	}

	/** Returns None if no music in any lead, or if it comes round before all calls completed */
	def makeAftNode(entry: Row, calling: String, desc: String): Option[BristolNode] =
	{
		val rows = generateRows(entry, calling)
		val music = musicDefs.map(_.countMusic(rows))
		if (music.sum>=2)
		{
			val comesRound = rows.exists(_.isRounds)
			val effectiveRows = removeLastLead(rows)
			if (comesRound)
			{
				if (lastLeadhead(effectiveRows).isRounds)
					Some(BristolNode(removeLastLead(effectiveRows), Row(8), desc))
				else
						None
			}
			else
				Some(BristolNode(effectiveRows, lastLeadhead(rows), desc))
		}
		else
			None
	}

	/** Always works */
	def makeInitialNode(entry: Row, calling: String, desc: String): BristolNode =
	{
		val rows = generateRows(entry, calling)
		new BristolNode(rows, desc)
	}

}

