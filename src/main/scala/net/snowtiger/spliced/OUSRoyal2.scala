package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod, Row}
import net.snowtiger.spliced.composition._
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Splice, _}

/**
 * @author mark
 */

object OUSRoyal2 extends SplicedGenerator with SearchDefinitionBase
{
	val bristol = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val pangaea = NamedMethod("Pangaea", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "10")		// l
	val sgurr = NamedMethod("Sgurr A'Chaorachain", 10, "x50x14.50x12.30x34x50.16x16.70x16x70 ", "10")		// k1
	val triton = NamedMethod("Triton", 10, "30x30.14x12x10x12x50.14x14.70.16x18.90", "12")							// f
	val yorkshire = NamedMethod("Yorkshire", 10, "x30x14x50x16x1270x38x14x50x16x90", "12")							// b
	val remus = NamedMethod("Remus", 10, "-3-4-2.5.6-34-5-6-67-6-7", "10")														// l
	val phobos = NamedMethod("O Phobos", 10, "-3-4-2.5.6-34-5-6-67-6.34.7", "12")
	val re_sg = NamedMethod("H Re-Sg", 10, "-3-4-2.5.6-34-5-6-67-6-7-6-7.6-6.5-34-3.2-5.4-5-10")
	val pooh = NamedMethod("Pooh", 10, "3-5.4-5-36-34.7.8-6-7.6-78.10", "12")													// c1
	val smallsfriends = NamedMethod("F Small's Friends", 10, "-5-4.5-2.3-3478-58-6-7.6-8.9", "10")			// k1

	//val methods = List(remus, precambrian, bristol, sgurr)
	val methods = List(remus, triton, sgurr, bristol)
	//val methods = List(remus, pooh, bristol, sgurr)

	val resgLeads1 = Set("1089674523", "1685074923", "1324685079").map{Row(_)}
	val calling1 = "Hp Xx V Xx H' M'VpHH' F'MS S'W'H'M' HBMxX MxFV HxHx WxWx SxSx"
	val calling2 = ("Ip /1973526480 Vp /1573908264 V Xx H' M' /1489076523 HH' F'MS S'W'H'M' HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads1)
	val calling3 = ("Ip /1973526480 Vp /1573908264 Xx M H' M' /1489076523 HH' F'MS S'W'H'M' HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads1)

	// Variant 1
	val calling4 = ("Ip /1973526480 Vp /1573908264 Xx M H' M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads1)

	// Variant 1 + 4
	val resgLeads5 = Set("1089672534", "1685074923", "1324685079").map{Row(_)}
	val calling5 = ("WHW Ip /1974536280 X Vp /1573908264 Xx M H' M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads5)
	val calling5a = ("WHW Ip /1974536280 X Vp /1573908264 Xx M H' M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxVW Hp", resgLeads5)

	// Variant 1 + 4a
	val resgLeads6 = Set("1089673524", "1685074923", "1324685079").map{Row(_)}
	val calling6 = ("H' Ip /1974526380 X' Vp /1573908264 Xx M H' M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads6)
	val resgLeads7 = Set("1089674523", "1685074923", "1089476325").map{Row(_)}
	val calling7 = ("Ip /1973526480 Vp /1573908264 Xx MWH Bx WIp /1975324680 W'H' H' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads7)

	// New
	val secondPlaceFinish = false
	val resgLeads8 = Set("1089476325", "1620783459", "1089674523", "1685074923").map{Row(_)}
	//val calling8 = ("MWH BxW Ip /1975324680 W'H' Fp /1089472563 M'H' Ip /1972546380 V V /1573908264 FxFx IxIx BxBx VxVx " +
	val calling8 = ("MWH BxW Ip /1975324680 W'H' Fp /1089472563 M'H' Ip /1972546380 Bx VxVx " +
			(if (secondPlaceFinish) "Wh" else "Hh"), resgLeads8)

	val resgLeads9 = Set("1089476325", "1620783459", "1089674523").map{Row(_)}
	val calling9 = ("MWH BxW Ip /1975324680 W'H' Fp /1089472563 M'H' Ip /1972546380 Hx WxWx SxSx Xx Hp", resgLeads9)

	val resgLeads10= Set("1324685079").map{Row(_)}
	val calling10 = ("H WBx WHx W'M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx Hp", resgLeads10)

	val calling11 = ("H WBx WHx W'M' /1489076523 HH' F'M  BxBxBx   HBMxX MxFV HxHx WxWx SxSx XxXx Hp", resgLeads10)

	val calling12 = ("MH WBx MWH Hx B' MH'H' F'M SS'W' MHH BMxX MxFV HxHx WxWx SxSx Hp", Set[Row]())

	val calling13 = ("MH WBx MWH Hx B' MH'H' F'M SS'W' MHH B MxMVxH V Wx SxSx XxHp", Set[Row]())

	val calling14 = ("H WBx WMH Hx M'B' HH'F' MBx Bx BxH B MxX MxFV HxHx WxWx SxSx Hp", Set[Row]())

	val (calling, resgLeads) = calling14

	override def getExcludedLeads = resgLeads.map{(lh)=> Lead(lh, re_sg)}

	override def acceptNode(node: Node, leads: List[Lead]) =
	{
		/*
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		if (inSeed)
			true
		// For handstroke home finishing at the Wrong, we need a second's place method as the last one.
		else if (node.startLH.coursingOrder(getCallingMethod.plainPerm)=="864235790")
		{
			if (secondPlaceFinish)
				node.methods.last.is2ndsPlace
			else
				node.methods.last.isNthsPlace
		}
		else
		*/
		goodNode(node)
	}

	/*
	val methodAssessor = MethodAssessor(10)
	val c1methods = methodAssessor.parseMethods(Source.fromFile("TDRoyal.csv")).filter{_.lhGroup=="d1"}.
			filter{methodAssessor.isGoodRoyal(_)}
	for (method <- c1methods)
		println(method.namePlusClass+" "+method.outputPN());
	*/


	override lazy val seedProvider = new OriginalCompSeedMayBeFalse(this)

	override val seed = Some("ousRoyal2/BRST.txt")
	def generate() = tunnel(this)
	//val seed = Some("ousRoyal/pretty.txt")
	//def generate() = prettyPrint()

	override lazy val calls = List(Royal.Bob, Royal.Single, Royal.BigBob)

	def scoreFn(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				/*comp.atwScore*2 + */
				8*(ScoreFactory.balanceScore(comp)) +
				lengthScore(comp)*40 +
				(ScoreFactory.strictLenScore(comp, 0.4)*3)/2 -
				comp.leadsWithoutMusic*5 -
				comp.com*7 -	// 6
				comp.longestNoComRun*2 +	// 2
				/* (if (comp.isAtw) 100 else 0) + */
				comp.music(0)+2*comp.music(7)

	def lengthScore(comp: Composition) =
	{
		val length = comp.length
		if (length<5000)
			(length-5100)/2
		else if (length>5180)
			(5200-length)/2
		else
			0
	}



	val mus56 = new Music56Rollup()
	val mus65 = new Music65Rollup()
	val musRun4 = new MusicRun(4)
	val musRun5 = new MusicRun(5)
	val musRun6 = new MusicRun(6)
	val mus4Course = new Music4Course()
	val musQRows = new MusicQueens()
	val musQK = new CompositeMusic(new MusicQueensRow, new MusicKingsRow)
	val musCaters = new MusicAscendingRunTenorBehind(4,2)
	val compositeMusicDef = new CompositeMusic(mus56, musRun4, musRun5, musRun6, mus4Course, musQRows, musCaters, mus65, musQK)
	override lazy val musicDefs = Array(compositeMusicDef, new MusicPlainCourseLeads(), mus56, musRun4, musRun5, musRun6, mus4Course, musQRows, musCaters, musQK)

	override def acceptLead(lead: Lead) =
		lead.method!=re_sg || resgLeads(lead.startLH)

	override def acceptSplice(splice: Splice) =
	{
		//splice.com==splice.methods.size-1 || (splice.com==0 && splice.firstMethod==getCallingMethod)

		//splice.methods.size<=4 || splice.methodsUsed.size>2 || (splice.com==0 && splice.firstMethod==getCallingMethod)

		//splice.com==0  || (splice.methods.size>4 && splice.com==1)

		//splice.com<3
		true
	}

	private def goodNode(node: Node) =
	{
		val inSeed = node.methodsUsed == Set(getCallingMethod)
		inSeed || node.startLH!=Row("1357924680") || node.music(9)==2
	}
}