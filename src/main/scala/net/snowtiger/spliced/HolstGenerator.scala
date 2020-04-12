package net.snowtiger.spliced

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.Lead

/**
 * @author mark
 */

object HolstGenerator extends SplicedGenerator
{
	val mercury = NamedMethod("K Mercury", 8, "-56-14.56-58.36.14-34.58.34.16-58", "18")					// l BE
	val uranus = NamedMethod("Uranus", 8, "-38-1456-56-16-1234-3458-16-58", "12")									// b BD
	val venus = NamedMethod("Venus", 8, "-56-1456-58-36-34-38.14-14.58", "12")										// f cps
	val mars = NamedMethod("Mars", 8, "-38-14-58-16-14-1238.56-1236.78", "12")										// b BD
	val marsD = NamedMethod("m Mars D", 8, "34-34.18-56-18-14-58-14-18", "18")										// m D
	val jupiter = NamedMethod("Jupiter", 8, "-36-1456-58-16-34-38.12-14.58", "12")								// f BE
	val jupiterD = NamedMethod("j Jupiter D", 8, "-38-14-1258-1236-14-18.34-1256.78", "12")				// b BDO

	val saturn = NamedMethod("Saturn", 8, "-58-1458-58-36.14-34.58-1256-78", "12")								// a cps
	val saturn2 = NamedMethod("Saturn 2", 8 , "-38-14-12.58.36.14-12.58-14.56.18", "18")					// m BD = "Y"
	val saturn3 = NamedMethod("Saturn 3", 8, "-58-14.58-56.38.12-14.58-16.34.18", "18")						// m BM	= "M"
	val saturn4 = NamedMethod("Saturn 4 (N3)", 8, "-56-14-56-36.14-14.58-14.56.18", "18")					// m D	= "D"
	val saturn5 = NamedMethod("Saturn 5 (N2)", 8, "-58-14.58-58.36.14-12.58.34.12.34.18", "18") 	// m BD	= "G"
	val saturnD3 = NamedMethod("Saturn D3 (ND3)", 8, "-56-14.56-58.1236-14-18.36.14.56.18", "18")	// m DE
	val saturnD4 = NamedMethod("Saturn D4 (ND4)", 8, "-56-14.56-58.1236-34-18.36.12.56.18", "18")	// m B
	val saturnD5 = NamedMethod("Saturn D5 (ND5)", 8, "-56-14.56-58.36-34-18.36.14.36.18", "18")		// m BE

	val neptune = NamedMethod("Neptune", 8, "-56-14.56-58.36.14-14.38.14-14.58", "12")						// c K
	val neptune2 = NamedMethod("Neptune 2", 8, "-58-14.58-58.36.14-12.58.34.12.34.18", "18") 			// m BD	= "G"
	val neptune3 = NamedMethod("Neptune 3", 8, "-56-14-56-36.14-14.58-14.56.18", "18")						// m D	= "D"
	val neptuneD1 = NamedMethod("Neptune D1", 8, "-58-14.58-56.38-14-18.36-36.78", "12")					// c/h E
	val neptuneD2 = NamedMethod("Neptune D2", 8, "-56-14.56-58.16-34-18.36-56.18", "18")					// m DF
	val neptuneD3 = NamedMethod("Neptune D3", 8, "-56-14.56-58.1236-14-18.36.14.56.18", "18")			// m DE
	val neptuneD4 = NamedMethod("Neptune D4", 8, "-56-14.56-58.1236-34-18.36.12.56.18", "18")			// m B
	val neptuneD5 = NamedMethod("Neptune D5", 8, "-56-14.56-58.36-34-18.36.14.36.18", "18")				// m BE


	//val MethodLookup = Map(mercury.me, venus.me, mars.me, jupiter.me, uranus.me, saturn3.me, neptune3.me)
	//val MethodLookup = Map(mercury.me, venus.me, marsD.me, jupiter.me, uranus.me, saturn4.me, neptuneD1.me)
	val MethodLookup = Map(mercury.me, venus.me, mars.me, jupiterD.me, uranus.me, saturn3.me, neptuneD2.me)
	//val MethodLookup = Map(mercury.me, venus.me, mars.me, jupiterD.me, uranus.me, saturn3.me, neptuneD3.me)

	def generate()
	{
		// 1-3 - with Neptune 2 and Saturn 2, primary method is Jupiter
		//val calling = ("H H W MW MW MHHH MMW MMWH MW MWH MW MMMWW MH MMWWH", "2mx_1")
		//val calling = ("MMWW MMWWW MW BH MH BWH W BMWH MWWH WW MMH WHHH", "2mx_2")
		//val calling = ("HH W MW MW MW BWWH BH BH MWHH BHHH MMW BBWWH W MWWH", "2mx_3")

		// 4-7 - with Neptune 2 and Saturn 2, primary method is Jupiter
		//val calling = ("MW MMBBH MBHH MBBH WHHH MH BBWHH BBH W MH BBH BH BBH BWH", "2mx_r4")
		//val calling = ("MW MWH MW MHH W MHH W MH BBH W MH BH BH WH MH BH BH MBH MWH BH BH BWH", "2mx_r5")
		//val calling = ("MBH BBH BH BBH W MH BBHH MBBH WHHH MH BBWHH BWH BBWW MWH", "2mx_r6")
		//val calling = ("MWH MB MBH MBH MBWH MBBH WH MBWH MBWH MH BWH BH BH MBH W MH BWH BH MBH MW MH BWH BH BH BWH", "2mx_r7")

		//var methods = List(mercury, mars, saturn2, neptune2, venus, uranus, jupiter)
		//val xleads = List(Lead(Row("13245678"), neptune2), Lead(Row("14325678"), saturn2), Lead(Row("12435678"), neptune2))

		// x - with Neptune 3 and Saturn 3, primary method is Mercury
		//val calling = ("BHH BH MWH MBH BHHH BHH BWH WH MWHH WH BBBHHH MMWHHH MWH MMBHH BHH B", "2mx_x7")

		// n - with Neptune 3 and Saturn 3, primary method is Neptune3
		//val calling = ("W MH MBWW BWH WH BH WHH WW MMBBWWH MMBWWHH MMBWW MMBWWHH MH MMBWW MWH", "2mx_n1")
		// First calling to generate ATW!
		// val calling = ("W BWWH MMBWH WW MMH BWWH WW MHH MBWW MMH BH MBWW MMBWWH MMH MBWW MWH", "2mx_n2")

		//val calling = ("BBH MBWWH BH BWW MMBH MBWWHH MMBWW MMBWW MMBH BB MMB MH MMBWWH BWH WH", "n1")
		//val calling = ("BH MMBWWHH MMBWW BBHH MH MBWWH BWH WW MMHH MBBWW MMBWWHH MMBWWH BWH WH", "n2") // ATW 138/31
		//val calling = ("BH MMBWWHH MMBWW MMBBWW MMBHH MBH WH BWWH WW MMBBWWH MMBWWHH MMBWWH BWH WH", "n3") // ATW 122/32
		//val calling = ("MH WH MHH MMBWW MWWH BWW MMBWHH MMBWHH MMBWW B MMBWWHH MBWW MWWHH MMWWH WH", "n4")	// ATW 156/38
		//val calling = ("M HW HM HHMMBWW M HMMBWW HMMBW HBWW MMBW HHMMBW HHMMBWW HHMMBWW MMB HHMMWW HW H", "n4.2")
		//val calling = ("M HBWW MMBWW M HMMBWW MMBWW MMBW HHMMBW HHMMBWW HHMMBWW BW HM HMB HHMMWW HW H", "n4.3")
		//val calling = ("MBWWH MMHH MMBWWHH MBWW MMBWW MMBWHH MMBWHH W MWHH MWWH MMH MBH MMWWH WH MH", "n5")
		//val calling = ("BBBBWW MMBH MBWWHH MMBWW BB MBH WW MMBBWWH MMBWWHH MMBWWH BWH WH", "n6")
		//val calling = ("MH WWH MMBWHH MMBWHH MMBWW B MMBWWHH MBWWHH MBWW MWWH BHH MH WWHH MMWWH WH", "n10")
		//val calling = ("MH WH MMH BWW MH MMBWW MMBWW MMBWHH MMBWHH MMBWWHH MMBWWHH BWHH MBHH MMWWH WH", "n11")
		//val calling = ("MH WH MMH BWW MW MMBWW BWHH BWW MMBWHH MMBWWHH MMBWWHH MBWW MWWHH MMWWH WH", "n23")

		//var methods = List(mars, saturn3, neptune3, venus, uranus, jupiter, mercury)
		//var methods = List(mars, venus, uranus, jupiter, mercury, saturn3, neptune3)
		//val xleads = List(Lead(Row("13245678"), saturn3), Lead(Row("14325678"), neptune3), Lead(Row("12435678"), saturn3))

		// ***************** NEPTUNE MUST BE DELIGHT! *************************************
		// with Saturn4 from Neptune3, Mars Delight, and Neptune D1
		//val calling = ("MH WHH WH MWH W MW MWH MMWB MWHH MWB MWWH MW MW MH MWH MHH MH WH", "md1")

		//var methods = List(marsD, venus, uranus, jupiter, mercury, saturn4, neptuneD1)
		//val xleads = List(Lead(Row("13245678"), saturn4), Lead(Row("14325678"), marsD), Lead(Row("12435678"), saturn4))

		// Back to Saturn3 and Mars Surprise, and with new mx NeptuneD2 and NSN finish
		//val calling = ("BBHH BWW BH BHHH MHH WHHH BH BHHH BH BH WHH MW BWHHH BH BH MH BHH", "n2k1")
		//val calling = ("BH MH MMMWH BBH B MHH BH BHH WHH MHH WWHH MH WHHH MH WBH BHH", "n2k2")	// ATW!
		//val calling = ("BHH WHH MHH BH BHH WHHH BH BHH BW MW BH BH B MMBHHH BHHH BWW BH", "n2k3")
		//val calling = ("BHH WHH MBH BHH BH MH BH BHH WHHH BH BWH BWWHH MW BH BH B MMBH MH BWH BH", "n2k4")
		//val calling = ("BHH WHH MBH BHH BH MH BH BHH WHHH BH BWH BHHH BH BHHH MMBH MHH BH MWWHHH BH", "n2k8")
		//val calling = ("BH MBHH BHH BW BH BH MHH BBH BHHH MMBHH BHHH BH BH WH BWH BH BWHHH BBH", "n2k9")
		//var methods = List(mars, venus, jupiterD, saturn3, neptuneD2, uranus, mercury)

		// These from Uranus, old nd naming scheme
		//val calling = ("BMWWHHH MMW MHHH MMWH BMW MH MMMW BBMW M BMW M BH", "nd2")			// Best, just about got atw! ***
		//val calling = ("B MW WH H H M MW MH H H M MWH B MW MH M M MW BH B MW MH B MW MH BH", "nd2b")
		//val calling = ("B MW WH H H M MW MH W MW MH B MW WH H M MWH B MW MH M M MW BH H BH", "nd2c")
		//val calling = ("BMWWHHH MMW BW MWH BMW MH MMMW BBMWWHHH W MW M BH", "nd3")      // Best, now got atw! ***
		//val calling = ("BW MW BW MW BBMWWWH W MW BH MWWHHH W MWWHHH MMW BH", "nd4")
		//val calling = ("BW MW MHHH MMW BBMWWWH W MW BH MW M BMWWHHH MMW BH", "nd5")
		//val calling = ("BW MW MH BMW MH MMW BBMWWWHHH MW M BMWWHHH MMW BH", "nd6")
		//var methods = List(mars, venus, jupiterD, saturn3, mercury, neptuneD2, uranus)
		val xleads = List(Lead(Row("13245678"), neptuneD2), Lead(Row("14325678"), saturn3), Lead(Row("12435678"), neptuneD2))
		// As above, with SNS finish.
		//val calling = ("B MWHHH MWH BWHHH BHHH MH BWHHH BBH BBHH BBH BHH BHHH BBHHH BBH", "n2m1")
		//val xleads = List(Lead(Row("13245678"), saturn3), Lead(Row("14325678"), neptuneD2), Lead(Row("12435678"), saturn3))
		//var methods = List(mars, venus, jupiter, saturn3, neptuneD2, uranus, mercury)

		// Back to Saturn3 and Mars Surprise, and with new mx NeptuneD3; also trying Jupiter Delight
		//val calling = ("B M MW WH H M B WH W BH H M M MH M MW B MW B B MW M MH H H B BH", "n3u2") // Only one ended in H
		//val calling = ("BBBBH BBBBBH BH BBH BBH BBBBH BH BBBBBH BH BBBH BBH BBBBH BBH BH BBBBH B", "n3j1") // All are short-course BH
		//val calling = ("BH MMBH BH MMH MMBWHH MB MMBHH W MMBWHH MMWWHH MBWWHH MMBHH BWW MMBH WWH MWHH MMBH", "n3n1")
		//var methods = List(mars, venus, saturn3, mercury, uranus, jupiterD, neptuneD3)
		//val xleads = List(Lead(Row("13245678"), saturn3), Lead(Row("14325678"), neptuneD3), Lead(Row("12435678"), saturn3))

		// New NeptuneD4
		//val calling = ("B MBH BW BH WWHH WW BH BBHH MBWW BH BHH W B MBBH BH BWW BH", "n4n1")
		//val calling = ("B MMH BH BBH BBWHH MMBWH BBWH BHH BH BH BWH BH BH WHH BW BH", "n4n2")
		//val calling = ("B MMH BH BBHH MBWW BHH WW MW BBW B MBH BBWH W BBWW BH", "n4n3")	// **** BEST *****
		//val calling = ("B MMH BH BBH BBBWH W BBBWH BBW B MHH BHH WW BW BH", "n4n4")
		//val calling = ("B MMH BH BBH MBHH BH BH BWH BBWH BBW B MBH BBWH WHH BW BH", "n4n5")
		//val calling = ("H BH MBHH BH BHH W B MBHH BWH B MBHH WHH MBWW MMB MB MMH BBWHH BWW BH", "n4n6") //crap
		//val calling = ("H BH MBHH BH BHH W MMBHH BH BHH B MB MBBWW BBBH MMBWH BH WWHH BW BH", "n4n7") // crap
		//val calling = ("B MBBWW BH BBH MBHH BH BHH W B MBBH BH BBWH BH WWHH BW BH", "n4n8")
		//val calling = ("H BH MBHH BH BHH W B MBHH BWH B MBHH WHH W MMB MB MMH BBWHH BWW BH", "n4n9")
		//val calling = ("B MM HB HB B HHMBWW BWW HB HB HHBW BW HHBW BW B BW B BW HHBW B H", "n4n10")
		//val xleads = List(Lead(Row("13245678"), neptuneD4), Lead(Row("14325678"), saturn3), Lead(Row("12435678"), neptuneD4))
		//var methods = List(mars, venus, saturn3, mercury, uranus, jupiter, neptuneD4)

		//val calling = ("B MMHH BBWH BBWWHH BWWHH MBH BHH BBW BH BH MHH BH MBH MWHH MMBH", "n4bn1")	// best of a bad bunch
		//val calling = ("B MMHH B MMH BBWWHH BHH MHH BH MHH BH MBH MWHH BWHH MHH MHH MBH", "n4bn2")
		//val calling = ("B MMHH BBWH BBWWHH BHH MMH BH BW BH BH MHH BH MBH MWHH BW BH", "n4bn3")
		//val calling = ("BBHH BWWHH BWH MHH MW BBHH MH BB MHH BHH MHH BH MBWW MHH MBH", "n4bn4")
		//val xleads = List(Lead(Row("13245678"), saturn3), Lead(Row("14325678"), neptuneD4), Lead(Row("12435678"), saturn3))
		//var methods = List(marsD, venus, saturn3, mercury, uranus, jupiter, neptuneD4)

		// New NeptuneD5
		//val calling = ("BH MH MBWH WH BH BHHH BW BWHHH BH BHHH BWH MWHHH BHH BBHHH MWHHH MH", "n5k1") // done, bad
		//val calling = ("BH MH MBWH WH BBBWHHH BH BHHH BH BWHH B MBHHH BBHHH MWHHH MH", "n5k2")
		//val calling = ("H MWH BH WW MMWWHH MBH WW BHH B MH MBWWHH MBWW MMWH MMBWHH MMBH WWH MWWH", "n5n1") // crap
		//val calling = ("MW MMBWWHH MBWWHH MBWWH MWW MMBHH WWH MMBWHH MMBWWH MH MMWWH WW MMH B MWWHH", "n5n2")
		//val calling = ("MWH MH BWWHH MBWW MMH MMWW MMWW MMBHH WWH MMBWHH MMBWWHH MMBWW MMBH B MWWHH", "n5n3")
		//val calling = ("MWH MBH WW BWW MMBWHH MMBHH B MBH WW MMW MWHH MH MBWWHH MBWW MMWW MMBWHH", "n5n4")
		//val calling = ("H MBWW MMH MMWH MBWW BW B MH MBWWHH MBWWHH MMW MMBH WHH WW MWW MMW MWWH", "n5n5")
		//val xleads = List(Lead(Row("13245678"), neptuneD5), Lead(Row("14325678"), saturn3), Lead(Row("12435678"), neptuneD5))
		//var methods = List(marsD, venus, saturn3, uranus, jupiter, neptuneD5, mercury)

		//val plan = new CompositionPlan(calling._1, methods.reverse.head)
		//plan.addExcludedLeads(xleads)

		// 10-part with Saturn3, NeptuneD2, Jupiter D and Mars S
		//val calling = ("W BW BW", "3pt1")
		//val calling = ("M BM BM", "3pt2")
		//val calling = ("MHW", "3pt3")
		//val calling = ("WHHHM", "3pt4")
		//val calling = ("BWBWHW", "3pt5")
		//val calling = ("WW BH", "3pt6")
		//val calling = ("W BW BW /12436578 M BM BM", "pal3pt1")
		//val calling = ("M BM BM /12436578 W BW BW", "pal3pt1")
		//val calling = ("W BW BW /12436578 W BW BW", "pal3pt1")
		//val calling = ("WBWHW /12436578 WBWHW", "pal3pt5")
		val calling = ("WBWHW /12436578 MHMBM", "pal3pt5")	// Yay!! Gives 7-spliced 5120 with good method balance.
		//val calling = ("MHMBM /12436578 WBWHW", "pal3pt5")

		val compCalls = calling._1
		val compName = calling._2

		var methodList = List(mercury, uranus, neptuneD2, saturn3, jupiterD, mars, venus)

		val searchDef = new SearchDefinitionBase {
			val methods = methodList
			// "The" 5056
			val calling = compCalls
			def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)
			override val seed = Some("holstDelight\\"+compName+".txt")
		}

		//plan.addExtraPartEndPerm(Perm("12436578"))

		println("Start Holst gen for "+compName+", methods are "+methodList.map{_.name}.mkString(", ")+", comp is "+compCalls)

		var variations = greedy(searchDef)

		if (false)
		{
			methodList ::= venus
			variations = varyComps(searchDef, methodList, variations)

			methodList ::= uranus
			variations = varyComps(searchDef, methodList, variations)
		}
	}

}