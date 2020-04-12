package net.snowtiger.spliced

import net.snowtiger.ringing.{CompositeMusic, NamedMethod}
import net.snowtiger.spliced.composition.{Composition, Maximus}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score._
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.Splice

/**
 * Spliced Delight Max for Tom Hinks
 *
 * @author mark
 */
object HinksMax extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")				// m
	val azura = NamedMethod("U Azura", 12, "-5T-14.5T-12.3T-12-1T-16-7T.16-16.7T.16-16.7T", "1T")				// k1
	val gracechurch = NamedMethod("Gracechurch", 12, "34-5T.14-12-3T-34-1T-16-7T.16-16.7T-16-7T", "1T")	// l
	val counters = NamedMethod("Counter's Creek", 12, "x5x4.5x2.3x2x1x6x7.6x6.7x6x7", "1T")							// k1
	val deimos = NamedMethod("Deimos", 12, "34-5T.16-56-36.7T.34-1T-70.18.3T.10.3T", "1T")							// g
	val snowTiger = NamedMethod("Snow Tiger", 12, "3T-5T.14-5T-3T-12-1T-ET-10-18-9T.18-10.ET", "12") 		// f
	val templeRow = NamedMethod("Temple Row", 12, "3T-3T.14-12-1T.34-14.5T.14.56-167T-78-9T-18-9T", "12")	// f
	val tritone = NamedMethod("Tritone", 12, "3T-3T.14-12-1T-12-5T.14-14.7T.16-18.9T.14-14.9T", "12")		// f
	val reggaeGirl = NamedMethod("Reggae Girl", 12, "3T-5T.14-5T-1T-14-5T.14-14.70.16-18.9T-90-1T", "12")	// e
	val glenkeen = NamedMethod("Glenkeen", 12, "-5T-14.5T-5T.36.14-7T.58.16-16.7T-16-1T.90-90.1T", "1T")	// m
	val newBoy =  NamedMethod("New Boy", 12, "-5T-14.5T-5T.36.14-12.5T-12-1T-18-9T.18-90.1T", "1T")				// k
	val anomalous = NamedMethod("M Anomalous", 12, "34-5T.14-5T-1T-14-125T-14-70.16-18.9T-90-1T", "1T")		// k2
	val aesthetic = NamedMethod("E Aesthetic", 12, " -34-14.5T-7T.36-7T-1T-90.16.70-18-9T.18-10.ET", "1T")	// l

	val hinks1 = "W WH WWBxH' M'H WWBxH MH MMWWHH"
	val hinks2 = "WH BxWWH BxH WW BxMHH BxHH MWWHH"
	val hinks3 = "WW BxH BxBxBxH BxWH MH WWH WWHH"
	val bradshaw1 = "WWH WWWH MMH MWMH WMWH"
	val bradshaw2 = "WWHH MMH MW MH WWHH WWW MH"
	val mbd1 = "H WH WMH WWHH MHH' M'W MWH"
	val mbd2 = "WWH WWWH MMH MW MH WM WH"
	val mbd6 = "WWH WWWH MMH MWMH WNNNMWH"
	val mbd8 = "WWH H MMH MWH MH MMH WNNNMWH"
	val mbd12 = "M H H W M H H M W M H H W H H W H"
	val mbd13 = "H H M M W W H H M H H M W M H W"
	val reading3 = "WHH WWHH W' M'WWHH MMH MWBF'I'B MWWHH"

	val calling = hinks2

	val methods = List(avon, templeRow, counters, azura, reggaeGirl)

	def generate() = greedy(this)
	//override val seed = Some("T 5040 5-Spliced (Score=5049757, COM=89, LR=2, ATW=605, music=1256/525/339/339/216/83/35/13/8/6/27/4, LWM=14)  AUAGAT-T- GAGAxGR-GTRUR-UAGRU- GGUAAGGRxURURTU- ARUAGA-RGTTRTR-A RTRxGTR G-TRRTRG A- UAGGR- AGTTxUG- GURU- RRTUU-ARGGAT-UAATTU-A A- TUAATU- A(23/397) G(22/280) R(22/196) T(19/188) U(19/195)  (LTLM=12, LA=26, homes=13) ATW")
	override val seed = Some("T 5088 5-Spliced (Score=5049761, COM=88, LR=3, ATW=605, music=1293/514/366/366/241/88/32/12/7/6/26/1, LWM=12)  ATTRTTRT-UTCU- CUTCCxRR-RTTRCT-CAUC- UCTCUxCT- ACTUA-RUAAC-UCU CTxUCTTUC-TRRUA A- UARRR- UCAUUxAC A- TRCTC- RRCAC-AAARTRT-UUAR-UTCU- UAUAT- A(19/238) C(23/345) R(18/201) T(23/273) U(23/236)  (LTLM=10, LA=26, homes=12) ATW")
	//def generate() = prettyPrint()

	val complibMusic = new CompositeMusic(new MusicRun(4), new MusicRun(5), new MusicRun(6), new MusicRun(7), new MusicRun(8), new MusicRun(9), new MusicRun(10), new MusicRun(11), new Music56Rollup(), new Music65Rollup())
	override lazy val musicDefs = Array(complibMusic, new MusicRun(4), new MusicRun(5), new MusicRun(5), new MusicRun(6), new MusicRun(7), new MusicRun(8), new MusicRun(9), new MusicRun(10), new MusicRun(11), new Music56Rollup(), new Music65Rollup())
	//override lazy val musicDefs = Array(new MusicLB(4), new MusicLB(5), new MusicLB(6), new Music56Rollup(), new Music65Rollup(), new MusicRun(12))

	override lazy val calls = List(Maximus.Bob, Maximus.Single, Maximus.BigBob)

	override def scoreFn(comp: Composition) = score1(comp)

	def score1(comp: Composition) =
		comp.methodsUsed.size*1000000 +
				comp.atwScore*80 +
				ScoreFactory.balanceScore2(comp)*2 +
				2*ScoreFactory.strictLenScore(comp, 0.2) -
				comp.falseScore*40 -
				comp.longestNoComRun*10 + comp.com*1 +
				comp.music(0)+2*comp.music(10) //2*comp.music(0)+2*comp.music(1)+comp.music(2)+5*comp.music(3)+comp.music(4)+10*comp.music(5)

	override def acceptSplice(splice: Splice) =
	{
		splice.isInSingleMethodComp(getCallingMethod) || splice.nLeads<5 || (splice.methodsUsed.size>2) // && splice.nLeads<10)
	}
}
