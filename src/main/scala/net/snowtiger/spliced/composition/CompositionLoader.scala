package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.tables.{Node, Splice}

import scala.io.Source

/** Loads compositions as lists of Nodes, but does not add to Tables nor create Compositions */
class CompositionLoader(compPlan: CompositionPlan, methods: List[NamedMethod])
{
	val methodMap = methods.map{_.me}.toMap

	def loadNodesFromFile(file: Source) =
	{
		file.getLines().flatMap{lineToNodes(_)}
	}

	protected def lineToNodes(line: String) =
	{
		val trimmedLine = line.trim
		if (trimmedLine.isEmpty || trimmedLine.startsWith("#"))
			None
		else
			parseLine(trimmedLine) match
			{
				case Some(compStr) => Some(parseNodes(compStr, compPlan))
				case _ => None
			}
	}

	protected def parseLine(line: String) =
	{
		val i1 = line.indexOf(')')
		val i2 = line.indexOf('(', i1+1)
		if (i1<0 || i2<0)
			None
		Some(line.substring(i1+2, i2-2).filter(_!=' '))
	}

	protected def parseNodes(compStr: String, compPlan: CompositionPlan) =
	{
		val calls = compPlan.calls.toSet
		val callMap = calls.map(_.me).toMap
		var splice: List[NamedMethod] = Nil
		var nodes: List[Node] = Nil
		val startLH = Row(methods.head.nbells)
		var lh = startLH

		def addSplice(call: Call)
		{
			val pair = makeNode(lh, Splice(splice.reverse, compPlan), call, compPlan)
			lh = pair._2
			nodes = pair._1::nodes
			splice = Nil
		}
		// Repeatedly read the comp string, allowing e.g. a one-part composition plan to be satisfied with a three-part calling
		while (nodes.size < compPlan.calling.size)
		{
			for (c <- compStr)
			{
				if (callMap.contains(c))
					addSplice(callMap(c))
				else if (c!=' ')
					splice = methodMap(c.toString)::splice
			}
			// Deal with compositions ending with a plain lead
			// If it's a plain lead, need to make the plain Call from the last PN of the last method.
			if (splice!=Nil)
				addSplice(new Call(splice.head.lead.last))
		}
		if (nodes.size!=compPlan.calling.size)
			throw new AssertionError("Parsed composition has different number of nodes to composition plan: "+nodes.size+" vs "+compPlan.calling.size)
		assert(lh.maskBackBells(2)==Row(methods.head.nbells).maskBackBells(2))
		nodes.reverse
	}

	protected def makeNode(startLH: Row, splice: Splice, call: Call, compPlan: CompositionPlan): (Node, Row) =
	{
		val endLH = splice.genLeadheads(startLH).last.apply(splice.lastMethod.callPerm(call.pn))
		val nodeType = new NodeType(compPlan.callingPositions.get(startLH).get, endLH, call)
		val node = Node(startLH, nodeType, splice)
		(node, endLH)
	}

}