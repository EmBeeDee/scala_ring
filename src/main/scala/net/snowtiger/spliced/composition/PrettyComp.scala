package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{Method, NamedMethod, PN, Row}
import net.snowtiger.spliced.tables.Node

/**
 * @author mark
 */

case class PrettyComp(headings: List[String], lines: List[PrettyLine], nodes: List[Node])
{
	val nbells = nodes.head.methods.head.nbells
	val nBrackets = lines.count(_.bracketted)
	/** Length of longest method string */
	val maxMethodLength = lines.map{_.methods.size}.max

	lazy val partEnd = lines.last.ch
	lazy val nParts = partEnd.order
	lazy val length = nodes.map{_.length}.sum * nParts
	lazy val methodList = nodes.flatMap{_.methods}
	def countMethod(m: NamedMethod) = nodes.map{_.methods.count(_==m)}.sum * m.leadLength * nParts
	lazy val methodChanges = Map() ++ methodList.toSet.map{ (m: NamedMethod)=> Tuple2(m, countMethod(m))}

	def print()
	{
		println(length+" Spliced TD "+Row(nbells).stage)

		def methodsForLen(len: Int) = methodChanges.filter{_._2==len}.map{_._1.description}.toList.sorted
		val uniqueLengths = methodChanges.values.toSet.toList.sorted.reverse
		val methodsByLen = uniqueLengths.map{(len)=> len+" "+methodsForLen(len).mkString(", ")}
		println(methodChanges.keys.size+"m: "+methodsByLen.mkString("; "))

		val maxCHSize = lines.map{_.courseHeadSize}.max
		val rounds = Row(maxCHSize).toString.substring(1)
		println(" "+rounds+" " + PrettyLine.spacing + headings.mkString(PrettyLine.spacing))
		for (line <- lines)
			println(line.toString(maxCHSize))

		if (nParts>1)
			println(nParts+" part.")
	}

	/** Attempts to combine lines using 2, 3 etc in calling positions; maxLen is the longest method string allowable */
	def combine(maxLen: Int) =
	{
		case class MultiCall(multicall: String)
		{
			val (n, call) =
				if (multicall.size>1 && multicall(0).isDigit)
					( multicall(0).asDigit, multicall.substring(1) )
				else
					( 1, multicall )
			// Only combine bobs, e.g. we have "W" not "sW"
			val isCombinable = call.size==1
			def combine(next: MultiCall): Char = ('0'+n+next.n).toChar
		}

		def combine(currLine: PrettyLine, linesLeft: List[PrettyLine], accept: (PrettyLine)=>Boolean): List[PrettyLine] =
		{
			if (linesLeft.isEmpty)
				List(currLine)
			else
			{
				val nextLine = linesLeft.head
				val combinedSize = currLine.methods.size+nextLine.methods.size
				if (combinedSize>maxLen || (!accept(currLine)))
					currLine::combine(nextLine, linesLeft.tail, accept)
				else
				{
					val nextCalls = nextLine.toCalls(headings)
					// Be careful to deal with the case the last line has no calls
					if (nextCalls.isEmpty)
						currLine::combine(nextLine, linesLeft.tail, accept)
					else
					{
						val lastCall = MultiCall(currLine.toCalls(headings).last)
						val firstCall = MultiCall(nextCalls.head)
						if (firstCall.isCombinable && firstCall.call==lastCall.call)
						{
							val startLine1 = currLine.calls.reverse.dropWhile(_==' ').tail.reverse
							val endLine2 = nextLine.calls.dropWhile(_==' ').tail
							val newCalls = startLine1 ++ List(lastCall.combine(firstCall)) ++ endLine2
							val combined = PrettyLine(nextLine.ch, nextLine.bracketted, newCalls, currLine.methods+nextLine.methods)
							combine(combined, linesLeft.tail, accept)
						}
						else
							currLine::combine(nextLine, linesLeft.tail, accept)
					}
				}
			}
		}

		def doCombine(lines: List[PrettyLine], accept: (PrettyLine)=>Boolean): List[PrettyLine] =
			combine(lines.head, lines.tail, accept)

		val combineBracketted = doCombine(lines, (line)=>line.bracketted)
		val combinedHomes = doCombine(combineBracketted, (line)=>line.calls.last!=' ')
		val combinedAll = doCombine(combinedHomes, (line)=>true)
		PrettyComp(headings, combinedAll, nodes)
	}

}

object PrettyComp
{
	val MaxMethodLen = 20

	def makeCombined(nodes: List[Node], callingPositions: List[String]) =
	{
		val pretty = makeUncombined(nodes, callingPositions, false)
		// Attempt to combine lines, but don't let the method string get any longer than it is already
		pretty.combine(pretty.maxMethodLength.max(MaxMethodLen))
	}

	/**
	 * Create basic PrettyComp without attempting to collapse lines (e.g. with "2" bobs) at all.
	 * @param nodes
	 * @param callingPositions
	 * @param newLineOnEveryCoursehead if set, will break out a new line if a coursehead found midway through the calling positions
	 * @return
	 */
	def makeUncombined(nodes: List[Node], callingPositions: List[String], newLineOnEveryCoursehead: Boolean) =
	{
		val nbells = nodes.head.methods.head.nbells
		var nodeHead = Row(nbells)
		var revComp: List[PrettyLine] = Nil
		var callsLeftOnLine = callingPositions
		var revCallsSoFar: List[Char] = Nil
		var methodsSoFar = ""
		var lastLine: Option[PrettyLine] = None
		val courseHeadChecker = cyclicCourseHead(_)

		def genLeadheads(from: Row, methods: List[Method]) =
		{
			var lh = from
			for (m <- methods)
				yield {val curr = lh; lh = curr.apply(m.plainPerm); curr}
		}

		def abandonLastLine()
		{
			if (lastLine.isDefined)
			{
				methodsSoFar = lastLine.get.methods + methodsSoFar
				lastLine = None
			}
		}

		def goToNextCall(callPos: String)
		{
			val (callsUsed, callsLeft) = callsLeftOnLine.span{_!=callPos}
			revCallsSoFar = List.fill(callsUsed.size)(' ') ++ revCallsSoFar
			callsLeftOnLine = callsLeft
		}

		def addCall(call: Call)
		{
			revCallsSoFar = call.symbol :: revCallsSoFar
			callsLeftOnLine = callsLeftOnLine.tail
			if (!call.isPlain)
				methodsSoFar+= call.splicedSymbol
			abandonLastLine()
		}

		def makePrettyLine(lh: Row, lastMethod: Method) =
		{
			val calls = revCallsSoFar.reverse.padTo(callingPositions.size, ' ')
			if (courseHeadChecker(lh))
				PrettyLine(lh, false, calls, methodsSoFar)
			else
				virtualCH(lh, lastMethod, courseHeadChecker) match
				{
					case Some(ch) => PrettyLine(ch, true, calls, methodsSoFar)
					case None => PrettyLine(lh, false, calls, methodsSoFar)
				}
		}

		def newPrettyLine(lh: Row, lastMethod: Method)
		{
			// Extract any methods added to any previous, unused PrettyLine
			// (It may sometimes be redundant lines are produced halfway across the course, which we abandon)
			abandonLastLine()
			lastLine = Some(makePrettyLine(lh, lastMethod))
			methodsSoFar = ""
		}

		def emitLine()
		{
			revComp = lastLine.get :: revComp
			callsLeftOnLine = callingPositions
			revCallsSoFar = Nil
			lastLine = None
		}

		for (node <- nodes)
		{
			val callPos = node.nodeType.end.name
			val leadheads = genLeadheads(nodeHead, node.methods)
			val methods = node.methods.map{_.abbrev}.mkString
			val chPos = leadheads.indexWhere{courseHeadChecker(_)}
			// See if we have an internal course head; if so save up a PrettyLine in lastLine - but note it may not be used
			if (chPos>0 && chPos<leadheads.size-1)
			{
				methodsSoFar+= methods.substring(0, chPos)
				newPrettyLine(leadheads(chPos), node.methods(chPos-1))
				if (newLineOnEveryCoursehead)
					emitLine()
				methodsSoFar = methods.substring(chPos)
			}
			else
			{
				methodsSoFar+= methods
			}
			// If the last node left us at the end of a line, issue the previous line.
			// (We delay this to help with cases where a few methods from the next node will get us to a course end)
			if (callsLeftOnLine.isEmpty)
				emitLine()
			// Now process the call at the end of the node
			val lastMethod = node.methods.last
			val lastLHOfNode = leadheads.last
			goToNextCall(callPos)
			// If we need a new line before the calling position, then we must output a PrettyLine
			if (callsLeftOnLine.isEmpty)
			{
				if (lastLine.isEmpty)
					newPrettyLine(lastLHOfNode, lastMethod)
				emitLine()
				goToNextCall(callPos)
			}
			// Now add the call and move on to the next node head
			addCall(node.nodeType.endCall)
			nodeHead = lastLHOfNode.apply( lastMethod.callPerm(node.nodeType.endCall.pn) )
			// Prepare a new PrettyLine for the end of the node - if it's a Home it's likely to be used soon
			newPrettyLine(nodeHead, lastMethod)
		}
		if (callsLeftOnLine.isEmpty)
			emitLine()

		PrettyComp(callingPositions, revComp.reverse, nodes)
	}

	/** In fact deals with tenors-split cases where the back bells are mixed up, but stay together at a course end */
	def tenorsTogetherCourseHead(lh: Row) =
		lh.maskFrontBells(6).toString.substring(0,6)=="******"

	def cyclicCourseHead(lh: Row) =
	{
		if (tenorsTogetherCourseHead(lh))
			true
		else
		{
			val nbells = lh.nbells
			def workingBells(trebleLH: Row) = Row(trebleLH.toString.substring(1))
			val workingLH = workingBells(lh)
			val rounds = workingBells(Row(nbells))
			val revRounds = rounds.reverse
			def isCyclic(lh: Row) = lh.isCyclicPermOf(rounds) || lh.isCyclicPermOf(revRounds)
			isCyclic(workingLH) || isCyclic(workingLH.apply(PN("1"))) || isCyclic(workingLH.apply(PN(""+Row.Rounds(nbells-2))))
		}
	}

	def virtualCH(lh: Row, lastMethod: Method, courseHeadCheck: (Row)=>Boolean) =
		bestCH(lh.generateGroupByPlaces(lastMethod.plainPerm), courseHeadCheck)

	def bestCH(leadHeads: List[Row], courseHeadCheck: (Row)=>Boolean) =
	{
		val n = leadHeads.head.nbells
		// Try and find any "proper" course ends in the set first
		leadHeads.find{courseHeadCheck(_)} match
		{
			case Some(ch) => Some(ch)
			// If not, fall back to anything with the tenor home.
			case None => leadHeads.find(_.bellAt(n)==n)
		}
	}
}

case class PrettyLine(ch: Row, bracketted: Boolean, calls: List[Char], methods: String)
{
	import PrettyLine.spacing
	private def bracket(c: Char) = if (bracketted) c else ' '
	private def courseHead(lastBell: Int) = bracket('(') + ch.toString.substring(1,lastBell) + bracket(')')

	override def toString = toString(courseHeadSize)
	def toString(courseHeadSize: Int) = courseHead(courseHeadSize) + spacing + calls.mkString(spacing) + spacing*2 + methods

	def courseHeadSize = ch.highestBellNotHome.max(6)

	def toCalls(headings: List[String]) =
	{
		calls.zip(headings).flatMap{
			case (' ', h) => None
			case ('-', h) => Some(h)
			case ( c , h) => Some(c+h)
		}
	}

}

object PrettyLine
{
	val spacing = " "

}
