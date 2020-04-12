package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{Music, NamedMethod, Perm, Row}
import net.snowtiger.spliced.search.SearchDefinition
import net.snowtiger.spliced.tables.{Lead, Node, _}

import scala.collection.immutable.BitSet
import scala.collection.mutable.ListBuffer

/**
 * @author mark
 */

class CompositionPlan(val compStr: String, val calls: List[Call], val singleMethod: NamedMethod, val musicDefs: Array[Music]) extends PartEndPermutes
{
	def this(searchDef: SearchDefinition) = this(searchDef.calling, searchDef.calls, searchDef.getCallingMethod, searchDef.musicDefs)

	val rounds = Row(singleMethod.nbells)
	val nodeTypeTable = new NodeTypeTable()

	val callingPositions: StageCalls = singleMethod.nbells match
	{
		case 8 => Major
		case 10 => Royal
		case 12 => Maximus
		case 16 => Sixteen
		case _ => throw new Exception("No calling positions currently set up for "+singleMethod.nbells+" bells.")
	}

	var (calling, partend) = parseDoubleComp(compStr)
	val partEndPerm = partend.toPerm
	var allPartEndPerms = generateAllPartEndPerms()
	val nodeStartLeads = calling.map{_.startLH}.toSet
	val nodeTypes = calling.map{_.nodeType}.toSet
	var nparts = partEndPerm.order

	/** Can be used to exclude leads from the composition - these should not be present in the composition plan, either */
	var excludedLeads = Set[Lead]()
	var excludedLeadNumbers = BitSet()
	var excludedLength = 0
	/** Populated during table build */
	var excludedMusic = new MusicCount(musicDefs)

	/** Add any leads which must be true against the result compositions, but will be added manually afterwards */
	def addExcludedLeads(leads: Iterable[Lead], leadTable: LeadTable)
	{
		excludedLeads = leads.toSet
		for (lead <- leads)
			excludedLeadNumbers+= leadTable.intern(lead).n
		excludedLength = excludedLeads.toList.map {_.method.leadLength}.sum
	}

	def generateAllPartEndPerms() = partEndPerm.generateGroup

	def addExtraPartEndPerm(extraPerm: Perm)
	{
		allPartEndPerms = allPartEndPerms ++ allPartEndPerms.map {_.permuteBy(extraPerm)}
		nparts = allPartEndPerms.size
		println("Part end perms: "+allPartEndPerms.mkString(", "))
	}

	def permuteByPartEnds(leadhead: Row): List[Row] =
	{
		if (nparts==1)
			List(leadhead)
		else
			allPartEndPerms.map{leadhead.permuteBy(_)}
	}

	/** Get the basic, empty Atw map - if there are excluded leads, the map should include the methods/pbs in these */
	lazy val baseAtw =
	{
		var atw = MultiAtw()
		for (lead <- excludedLeads)
			atw = atw + BitAtw(lead.method, lead.startLH)
		atw
	}

	/**
	 * Allow split compositions using / followed by reset leadhead followed by more calls.
	 * @param compStr
	 * @return
	 */
	def parseDoubleComp(compStr: String): (List[CompositionNode], Row) =
	{
		def parseSubComp(s: String) =
		{
			val (startLH, comp) = s.trim.span(_!=' ')
			val startCP = callingPositions.get(Row(startLH)).get
			// Must pad blocks to final call using postfix "p", even for last block running home
			parseComp(comp.trim, Row(startLH), startCP, false)
		}

		if (compStr.contains("/"))
		{
			val comps = (rounds.toString+" "+compStr).split("/")
			val parsedComps = comps.map{ parseSubComp(_) }
			val nodes = parsedComps.flatMap{_._1}.toList
			// What's the part end of a composition made up of several groups? Here assume rounds, i.e. one part
			(nodes, rounds)
			// ... and here assume it's the last lead of the first group.
			//(nodes, parsedComps.head._2)
		}
		else
		{
			parseComp(compStr, rounds, callingPositions.Home, true)
		}
	}

	/** padToHome will be ignored if the comp ends with a "p" */
	def parseComp(compStr: String, rounds: Row, startCP: CallingPosition, padToHome: Boolean): (List[CompositionNode], Row) =
	{
		val nodes = ListBuffer[CompositionNode]()
		var lh = rounds
		var lastCP = startCP

		def addNode(nodeLH: Row, nextCP: CallingPosition, call: Call): Row =
		{
			val nextLH = nextCP.apply(nodeLH, call.pn)
			val nodeType = nodeTypeTable.intern(new NodeType(lastCP, nextLH, call))
			nodes += CompositionNode(nodeLH, nodeType)
			nextLH
		}

		val plainCall = if (singleMethod.is2ndsPlace) callingPositions.Plain12 else callingPositions.Plain1N
		//val plainCall = callingPositions.Plain1N
		val specialCallMap = calls.map{_.me}.toMap + ('p'->plainCall)
		// Assume we're ending with a handstroke home if the last call has a "h" appended to it; this last call is expected
		// to be the calling position immediately after the handstroke home finish, e.g. a Wrong for 2nd's place methods.
		// If you have methods with the other finish (e.g. here Nth's place methods) you'd better exclude them from the
		// terminating position in the last node, in e.g. acceptNode().
		val HandstrokeHomeChar = 'h'
		var endsWithHandstroke = false
		var endsWithExplicitPlain = false
		for (chs <- (compStr+" ").sliding(2))
		{
			val c = chs(0)
			if (!c.isWhitespace && !specialCallMap.contains(c) && c!=HandstrokeHomeChar)
			{
				val nextCP = callingPositions.ByCall(c.toString)

				val postfix = chs(1)
				val call = specialCallMap.get(postfix) match
				{
					case Some(call) => call
					case None if postfix==HandstrokeHomeChar => c match
						{
							case 'W' => callingPositions.Plain12
							case 'H' => callingPositions.Plain1N
							case _ => throw new Exception("Handstroke home composition is expected to have final node of W or H")
						}
					case _ => callingPositions.Bob
				}
				endsWithExplicitPlain = false
				if (chs(1)=='h')
					endsWithHandstroke = true
				else if (chs(1)=='p')
					endsWithExplicitPlain = true
				lh = addNode(lh, nextCP, call)
				lastCP = nextCP
			}
		}

		// Add a plained node to the Home if the part ends with a plain lead.
		if (!endsWithHandstroke && !endsWithExplicitPlain && lastCP!=callingPositions.Home && padToHome)
		{
			val lhPN = singleMethod.lead.last
			val plain = new Call(lhPN)
			lh = addNode(lh, callingPositions.Home, plain)
		}
		if (endsWithHandstroke)
			lh = rounds
		(nodes.toList, lh)
	}

	def buildSingleMethodComp(tables: Tables, mustBeTrue: Boolean) =
	{
		assert(tables.methods.contains(singleMethod), "Tables must contain the single method for the composition")
		val nodes = ListBuffer[Node]()
		for (compNode <- calling)
		{
			val nodeMethods = tables.splices(compNode.nodeType).filter{_.isInSingleMethodComp(singleMethod)}
			assert(nodeMethods.size==1, "Expecting just one single-method node: "+nodeMethods.size)
			val node = tables.nodeTable.intern(Node(compNode.startLH, compNode.nodeType, nodeMethods.head))
			nodes+= node
			if (!node.isInTable)
			{
				val leads = tables.addToTable(node)
				if (!node.isInTable)
					if (mustBeTrue)
						throw new Exception("Failed to load comp plan; node is false: "+node)
					else
					// Must force the lead into the table even though it is false
						tables.doAddNode(node, leads)
			}
		}
		Composition(nodes.toList, tables)
	}

	override def toString = calling.size+" nodes:\n"+calling.mkString("\n")+" part end: "+partend

	def shortDesc = calling.size+" "+partend

	case class CompositionNode(startLH: Row, nodeType: NodeType)
}

case class NodeType(start: CallingPosition, end: CallingPosition, endCall: Call) extends Numberable
{
	def this(start: CallingPosition, endLH: Row, endCall: Call) = this(start, start.get(endLH).get, endCall)

	override def toString = start+"->"+end+endCall
}

class NodeTypeTable extends Numberer[NodeType]



