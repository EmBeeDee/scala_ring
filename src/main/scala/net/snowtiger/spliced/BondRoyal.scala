package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * 5032 for Simon Bond's birthday
 *
 * @author mark
 */
object BondRoyal extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val superlative = NamedMethod("Superlative No.2", 10, "x38x1478x70x38x1470x38x14x3470x38x90", "12")	// b
	val cambridge = NamedMethod("Cambridge", 10, "x30x14x1250x36x1470x58x16x70x18x90", "12")						// b
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val littleport = NamedMethod("LittlePort", 10, "x50x14.50x50.36.14x14.50x14x18", "10")							// m

	val methods = List(littleport, bristol, yorkshire)

	val calling1 = "WHH W MMWW BVFB MH W" 	// Bond
	val calling1a = "WHH W MMWW BVFB MH W  WHH W MMWW BVFB MH W" 	// Bond

	val calling = calling1a

	//override val seed = Some("bondRoyal/pretty.txt")
	//override val seed: Option[String] = Some("T 5040 3-Spliced (Score=3021552, COM=40, LR=9, ATW=243, music=342/12/91/4, LWM=38)  YYYYY-L L- L- YYYYY-BBBYB-B BBBBBBB-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-BYBL-B L- LB-YYYY YYYYY-L L- L- YYYYY-BBBYB-B BBBBBBB-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-BYBL-B L- LB-YYYY  B(50/117) L(30/74) Y(52/151)  (LTLM=13, LA=34, homes=8) ATW [3187 moves/s]")
	//override val seed: Option[String] = Some("T 5056 3-Spliced (Score=3021326, COM=48, LR=10, ATW=243, music=335/12/90/4, LWM=44)  YYYYY-L L- L- LY-L YBBYB-B BBBBBBB-LY BL-BBBBBBBB-YB-YYYY-LY-YYYY-BYBL-B L- LLLLLLLLL-YYYY YYYYY-L L- L- LY-L YBBYB-B BBBBBBB-LY BL-BBBBBBBB-YB-YYYY-LY-YYYY-BYBL-B L- LLLLLLLLL-YYYY B(48/102) L(38/80) Y(48/153)  (LTLM=12, LA=27, homes=18) ATW")
	override val seed: Option[String] = Some("T 5040 3-Spliced (Score=3021552, COM=40, LR=9, ATW=243, music=342/12/91/4, LWM=38)  YYYYY-L L- L- YYYYY-BBBYB-B BBBBBBB-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-BYBL-B L- LB-YYYY YYYYY-L L- L- YYYYY-BBBYB-B BBBBBBB-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-BYBL-B L- LB-YYYY  B(50/117) L(30/74) Y(52/151)  (LTLM=13, LA=34, homes=8) ATW")
	//override val seed: Option[String] = Some("T 5056 3-Spliced (Score=3005538, COM=52, LR=9, ATW=243, music=348/12/92/1, LWM=34)  YYYYY-L L- BBBBBBBB- LY-L YBBYB-L-LY BL-BBBBBBBB-L YBY-YYYY-LB-YYYY-YB-LLLLLLLL L- BL-YYYY YYYYY-L L- BBBBBBBB- LY-L YBBYB-L-LY BL-BBBBBBBB-L YBY-YYYY-LB-YYYY-YB-LLLLLLLL L- BL-YYYY B(48/132) L(38/66) Y(48/150)  (LTLM=7, LA=13, homes=9) ATW")
	//override val seed: Option[String] = Some("T 5072 3-Spliced (Score=3007997, COM=36, LR=9, ATW=243, music=373/12/94/4, LWM=33)  YYYYY-L L- BBBBBBBB- LY-BBYBB-L-B-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-YY-LB- LLLLLLLLL-YYYY YYYYY-L L- BBBBBBBB- LY-BBYBB-L-B-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-YY-LB- LLLLLLLLL-YYYY B(46/130) L(46/88) Y(44/155)  (LTLM=7, LA=21, homes=6) ATW")
	//override val seed: Option[String] = Some("T 5072 3-Spliced (Score=3003963, COM=38, LR=9, ATW=243, music=373/12/94/4, LWM=33)  YYYYY-L L- BBBBBBBB- LY-BBYBB-L-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-YY-LB- LLLLLLLLL-YYYY YYYYY-L L- BBBBBBBB- LY-BBYBB-L-Y-BBBBBBBB-YB-YYYY-LLL LLLLLL-YYYY-YY-LB- LLLLLLLLL-YYYY B(44/130) L(46/88) Y(46/155)  (LTLM=7, LA=21, homes=6) ATW")

	def generate() = tunnel(this)
	//def generate() = prettyPrint()

	def scoreFn(comp: Composition) = scoreTweak(comp)

	def score1(comp: Composition) =
			comp.methodsUsed.size*1000000 +
			15*strictLenScore(comp) -
			25*comp.falseScore +
			10*(ScoreFactory.balanceScore(comp)) +
			1*(comp.atwScore) +
			20*comp.com +
			(4*comp.music(0)+10*comp.music(1)+2*comp.music(2)) -
			10*comp.longestNoComRun -
			0*(comp.longestAbsence)-0*comp.leadsToLastMethod -
			1*comp.leadsWithoutMusic

	def scoreTweak(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				70*strictLenScore(comp) -
				1*comp.levenshteinPartDistance -
				40*comp.falseScore +
				5*(ScoreFactory.balanceScore(comp)) +
				1*(comp.atwScore) +
				20*comp.com +
				(4*comp.music(0)+10*comp.music(1)+2*comp.music(2)) -
				10*comp.longestNoComRun -
				0*(comp.longestAbsence)-0*comp.leadsToLastMethod -
				1*comp.leadsWithoutMusic

	val targetLen = 5032

	def strictLenScore(comp: Composition): Int =
	{
		val length = comp.length
		if (length==targetLen)
			10
		else if (length<5000)
			(length-5000)/2
		else if (length>2*targetLen-5000)
			(2*targetLen-5000-length)/2
		else
			0
	}

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)

	/** Leads in the node passed as a convenience, to avoid cost of regen */
	override def acceptNode(node: Node, leads: List[Lead]) = containsPartEnd(node, leads)

	def containsPartEnd(node: Node, leads: List[Lead]) =
	{
		if (node.startLH!=Row("1536749208"))
			true
		else
		{
			val partEnd = leads.find(_.startLH==Row("1654327890"))
			partEnd.isDefined
		}
	}
}