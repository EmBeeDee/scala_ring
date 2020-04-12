package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object Milne10 extends SplicedGenerator with SearchDefinitionBase
{
	val pooh = NamedMethod("Winnie the Pooh", 8, "-34-1458-56-16-14-1238-12-78", "12")				// d	BD
	val kanga = NamedMethod("Kanga", 8, "-3458-14-56-16-14-38-16-58", "12")										// b	BD
	val roo = NamedMethod("Roo", 8, "-3458-14-56-16-14-38-16-58", "18")												// g	BD

	val robin = NamedMethod("Christopher Robin", 8, "-56-1458-1256-36-14-58-16-78", "12")			// c	E
	val piglet = NamedMethod("Piglet", 8, "-58-1458-58-38-14-1458-14-58", "12")								// a	cps
	val eeyore = NamedMethod("Eeyore", 8, "56-56.14-58-36-34-1238-1256-18", "12")							// e	C
	val rabbit = NamedMethod("A Rabbit", 8, "-56-14-56-36-34-1458-3456-38", "18")							// m	cps
	val tigger = NamedMethod("Tigger", 8, "-38-14-1256-16-34-1258-12-58", "12")								// f	B
	val heffalump = NamedMethod("Heffalump", 8, "-38-1458-56-36-14-1238-3456-78", "12")				// b	B
	val owl = NamedMethod("Owl", 8, "-58-14-58-38-34-58-1456-18", "12")												// b	D

	//val touch = ("W MWWW BHH W BH BBBH WHHH MMW MH MMMWWWH W MWWH", "k1")
	//val touch = ("W MWWH W MW BH W BHH W BHH MMWWHHH W MWWWH W MWWH", "k2")
	//val touch = ("W MWWW BHH W BH BBW MW MH MMMW BHHH W MWWH W MWWH", "k3")
	//val touch = ("W MWWW BH BMWWWH W MW M BBH BMHHH MMW MH MMW MHH", "k4")
	//var methods = List(rabbit, robin, pooh, piglet, eeyore, owl, roo, tigger, heffalump, kanga)
	//var methods = List(robin, pooh, piglet, eeyore, owl, roo, tigger, heffalump, kanga)

	//val touch = ("BBM BW BBBH BW MWW MW M BH BBH BBBH BMW MH BBW BMH BM BM", "r1")
	//val touch = ("BBM BW BH BMH MH BBW BBMW MH BM BM BBBBBH BW BH BB", "r2")
	//val touch = ("BH BMWH BM BBBH BM BM BW BBBH BM BBBBW BBBMWW BH BM BM", "r3")
	//val touch = ("BBM BW BWWW BBH BM BBBBW BM BM BBBH BH BBMWW BH BM BM", "r4")
	//var methods = List(rabbit, robin, pooh, piglet, eeyore, owl, tigger, heffalump, kanga, roo)

	//val touch = ("HH MH W BHH BBBH WWWMMMH BBMHHH W BBH MHHH MMH BBMH", "w1")
	//val touch = ("HH MH WH BH M WMH W BBHH WWMH MH WH BBMHH WMMH WH WMHHH WMH", "w2")
	//val touch = ("WH M BH MHH MH WWWH WWMHHH MHH WH BH WWMHH MH WH WMMH", "w3")
	//val touch = ("MMH BH WWMMMHH BWMH BM BM WM WMM WWWMHH WM BH WMHHH", "w4")
	//val touch = ("HH MH WH BH M WMH W BBHHH BBMH WMMH M BMH WMMH WH WMHHH WMH", "w5")
	//var methods = List(robin, piglet, eeyore, owl, tigger, heffalump, kanga, roo, rabbit, pooh)

	//val calling = ("H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH", "a1") // "The" 5056
	//val calling = ("BBBBWWH MB MBHH MBH BH BHH B MMB MMHH MH WW BW BBWW BWHH B", "a2c") // 5152
	//val calling = ("BBB MWH' W'W BBH BH BB MM HH' W'W MHH W BBB MBH WW B", "a3")	// singles
	val touch = ("BBBBWWH MB MBHH MBH BH BHH B MMB MMHH MH WW BW BBWW BWHH B", "a2opt") // 5152

	val methods = List(robin, pooh, piglet, eeyore, owl, tigger, heffalump, kanga, roo, rabbit)
	val calling = touch._1
	override val seed = Some("pooh\\"+touch._2+".txt")
	def scoreFn(comp: Composition) = ScoreFactory.atwFinder(comp)

	def generate() = tunnel(this)
}
