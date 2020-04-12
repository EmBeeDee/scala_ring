package net.snowtiger.spliced.composition

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.tables.{Node, Tables}
import net.snowtiger.util.StringDistance

import scala.collection.{BitSet, mutable}


/**
 * @author mark
 */
trait Composition extends Ordered[Composition]
{
	val tables: Tables
	val compPlan = tables.compPlan

	def nodes: List[Node]

	def isTrue = nodes.forall( (node)=> nodes.forall{ (other)=> node.n==other.n || (node.startLH!=other.startLH && (node.falseLeads&other.leads).isEmpty)} )

	def falseScore:Int = Composition.falseScore(nodes)

	def makeNew(nodes: List[Node], tables: Tables): Composition
	def toImmutableComp: ImmutableCompositionBase

	def getNodeAt(i: Int) = nodes(i)
	def replaceNode(i: Int, newNode: Node): Composition
	def undoLastReplace()
	def leadBitSetWithoutNode(i: Int): BitSet
	def falseLeadBitSetWithoutNode(i: Int): BitSet

	/** Includes the length of any excluded leads - it is assumed these will be manually added to the result compositions */
	def length: Int

	def music: MusicCount

	/** Includes excluded leads; not massively efficient since it regens and re-interns the (included) leads */
	def allLeads = nodes.flatMap{_.toLeads}.map{tables.leadTable.getInterned(_)} ++ compPlan.excludedLeads.toList

	def leadsWithoutMusic = nodes.map{_.leadsWithoutMusic}.sum

	/** Doesn't include any COM caused by post-search addition of the excluded leads - too difficult to determine */
	def com =
	{
		var com = 0
		var node = nodes.head
		for (nextNode <- nodes.tail)
		{
			com+= node.countComTo(nextNode)
			node = nextNode
		}
		com+= node.countComTo(nodes.head)
		com * compPlan.nparts
	}

	def longestNoComRun: Int =
	{
		val methodList = nodes.flatMap(_.methods)
		var longest = 0
		var current = 1
		var prev = methodList.head
		var tail = methodList.tail
		if (compPlan.nparts>1)
			tail++= prev::tail.takeWhile(_==prev)
		for (m <- tail)
		{
			if (m==prev)
			{
				current+= 1
				if (current>longest)
					longest = current
			}
			else
				current = 1
			prev = m
		}
		longest
	}

	/** Includes methods used in excluded leads */
	def methodsUsed: Set[NamedMethod]

	def sortedMethodsUsed: List[NamedMethod]

	/** Includes methods in excluded leads */
	def methodCounts(methods: List[NamedMethod]): List[Int]

	def leastPopularMethodCount(counts: List[Int]): Int = counts.reduceLeft{(a,b) => math.min(a,b)}

	def mostPopularMethodCount(counts: List[Int]): Int = counts.reduceLeft{(a,b) => math.max(a,b)}

	/**
	 * Returns LTLM ("leads to last method" introduced in the composition) and LA ("longest absence" - longest gap
	 * between appearances of a method).
	 * @return
	 */
	def methodDistribution =
	{
		val nMethods = tables.methods.size
		val lastOccurrence = mutable.Map[NamedMethod, Int]()
		val longestAbsence = mutable.Map[NamedMethod, Int]()
		val leadsToFirst = mutable.Map[NamedMethod, Int]()
		val methodsToZero = tables.methods.map{(_,0)}.toMap
		lastOccurrence++= methodsToZero
		longestAbsence++= methodsToZero

		var lead = 0

		def reachMethod(m: NamedMethod)
		{
			val gap = lead-lastOccurrence(m)
			if (!leadsToFirst.contains(m))
				leadsToFirst+= m->gap
			if (gap>longestAbsence(m))
				longestAbsence+= m->gap
			lastOccurrence+= m->lead
		}

		for (node<-nodes; m<-node.methods)
		{
			reachMethod(m)
			lead+= 1
		}
		// Finish up counts at end of comp
		for (m<-tables.methods)
			reachMethod(m)
		(leadsToFirst.values.max, longestAbsence.values.max)
	}

	def leadsToLastMethod = methodDistribution._1
	def longestAbsence = methodDistribution._2

	/** Includes methods in excluded leads */
	def methodMusic(methods: List[NamedMethod]): List[Int] =
	{
		val counts = mutable.Map[NamedMethod, Int]()
		for (lead <- allLeads)
		{
			val c = counts.getOrElseUpdate(lead.method, 0)
			counts+= lead.method->(c+lead.music(0))
		}
		methods.map{counts.getOrElse(_, 0)}
	}

	protected def atwTable: MultiAtw

	def atwScore = atwTable.atwScore
	def isAtw = atwTable.isAtw

	def levenshteinPartDistance() =
	{
		val nodesDifferentScore = 10

		def zipParts(parts: List[List[Node]]): List[Set[String]] =
		{
			var nodesFromEachPart: Set[String] = Set()
			var remainingParts: List[List[Node]] = List()
			var remainingLen = 1000
			for (part <- parts)
			{
				nodesFromEachPart+= part.head.methodString
				remainingParts::= part.tail
				if (part.tail.size<remainingLen)
					remainingLen = part.tail.size
			}
			if (remainingLen==0)
				nodesFromEachPart::Nil
			else
				nodesFromEachPart::zipParts(remainingParts)
		}

		/** Maximum value of the levenshtein difference between all pairs in the list */
		def maxLevenshtein(nodeCallings: List[String]): Int =
		{
			nodeCallings match
			{
				case Nil => 0
				case c1::Nil => 0
				case c1::c2::rest => Math.max((c2::rest).map{StringDistance.levenshtein(_, c1)}.max, maxLevenshtein(c2::rest))
			}
		}

		def levenshteinParts(zippedParts: List[Set[String]]): Int =
		{
			def extra(n: Int) = if (n==0) 0 else n+nodesDifferentScore
			zippedParts match
			{
				case Nil => 0
				case nodeCallings::rest => levenshteinParts(rest) + extra(maxLevenshtein(nodeCallings.toList))
			}
		}

		val nparts = 2 // Not compPlan.nparts because the whole point is that we are searching for one-parts that are near 2-part in nature
		if (nparts==1)
		{
			0
		}
		else
		{
			val parts = nodes.grouped(nodes.size/nparts).toList.take(nparts)
			val l = levenshteinParts(zipParts(parts))
			//if (l==0) nodesDifferentScore else l
			l
		}
	}

	/** Levenshtein distance between two callings */
	def differenceBetween(that: Composition) = StringDistance.levenshtein(callingString, that.callingString)

	def score = tables.scoreFn(this)

	def compare(that: Composition) =
	{
		var c = that.score.compare(score)
		if (c==0)
			c = that.music.compare(music)
		if (c==0)
			c = longestNoComRun.compare(that.longestNoComRun)
		if (c==0)
			c = that.com.compare(com)
		// Try to ensure comps scoring the same but with differing callings are different
		if (c==0)
			c = that.nodes.hashCode().compare(nodes.hashCode())
		c
	}

	def callingString = nodes.map{_.shortForm}.mkString
	def methodCountMusicZip = methodCounts(sortedMethodsUsed).zip(methodMusic(sortedMethodsUsed))
	def balanceString = sortedMethodsUsed.zip(methodCountMusicZip).map( (p)=> p._1.toString+"("+p._2._1+"/"+p._2._2+") " ).mkString
	def fullOutput = nodes.mkString("\n")
	def countHomes = callingString.filter(_==' ').size

	override def toString =
	{
		(if (isTrue) "T" else "F("+falseScore+")") + " "+
		length+" "+methodsUsed.size+"-Spliced ("+
		"Score="+score+
		", COM="+com+
		//", Lev = "+levenshteinPartDistance+
		", LR="+longestNoComRun+
		", ATW="+atwScore+
		", music="+music+
		", LWM="+leadsWithoutMusic+
		/* ", false="+falseScore+ */
		") "+callingString+" "+balanceString+
		" (LTLM="+leadsToLastMethod+", LA="+longestAbsence+", homes="+countHomes+")" +
		(if (isAtw) " ATW" else "")
	}
}

object Composition
{
	//def apply(nodes: List[Node], tables: Tables) = new ImmutableComposition(nodes, tables)
	def apply(nodes: List[Node], tables: Tables) =
		new ImmutableFalseTrackerComposition(falseScore(nodes), nodes, MethodUseTracker(nodes, tables.compPlan.excludedLeads), tables)

	def falseScore(node: Node, other: Node): Int = if (node.n==other.n) 0 else (node.falseLeads&other.leads).size
	def falseScore(nodes: List[Node]): Int = nodes.map{ (node)=> nodes.map{ (other)=> falseScore(node, other)}.sum }.sum
}
