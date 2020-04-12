package net.snowtiger.spliced

import net.snowtiger.methodmaker.GoodPn
import net.snowtiger.ringing.{Music, NamedMethod, PN, Row}
import net.snowtiger.spliced.composition.{Composition, CompositionPlan, MusicCount, TableCompositionLoader}
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase
import net.snowtiger.spliced.tables.{Lead, Tables}

import scala.io.Source


/**
 * @author mark
 */

object MethodReplacer extends SplicedGenerator
{
	val bristol = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val deva = NamedMethod("Deva", 8, "-58-14.58-58.36-14-58-36-18", "18")								// j, G
	val cornwall = NamedMethod("Cornwall", 8, "-56-14-56-38-14-58-14-58", "18")						// l, cps
	val lessness = NamedMethod("Lessness", 8, "-38-14-56-16-12-58-14-58", "12") 					// f, B
	val malpas = NamedMethod("Malpas", 8, "34-58.14-58-36-14-58.14-14.7", "18")						// l, D
	val superlative = NamedMethod("Superlative", 8, "-36-14-58-36-14-58-36-78", "12") 		// b, E

	val yorkshire = NamedMethod("Yorkshire", 8, "-38-14-58-16-12-38-14-78", "12")					// b, B
	val adelaide = NamedMethod("Adelaide", 8, "-58-14-56-36-14-58.36-36.18", "12")				// c, BE
	val glasgow = NamedMethod("Glasgow", 8, "36-56.14.58-58.36-14-38.16-16.38","18")			// g, BE
	val venusium = NamedMethod("Venusium", 8, "-56-14.56-58.36-14-58-36-18", "18")				// j, G

	val cadmium = NamedMethod("K Cadmium", 8, "36-56.14.58-58.16-12-38.14-56.18", "18")			// j, BE
	val jamesCollege = NamedMethod("James College", 8, "-58-14.58-12.36.12-14.38.12-16.78", "12")	// f, B
	val queenCamel = NamedMethod("Queen Camel", 8, "-58-14.58-56.38-14-58-14-18", "18")			// g, cps

	val methods = List(adelaide, bristol, cornwall, deva, lessness, malpas, superlative, venusium, yorkshire, glasgow,
		cadmium, jamesCollege, queenCamel)

	def generate()
	{
		val searchDef = new SearchDefinitionBase {
			val methods = List(venusium, glasgow, yorkshire, adelaide, superlative, malpas, lessness, cornwall, deva, bristol)
			// "The" 5056
			val calling = "H W BH MBBBBBH B MH BH W B MMH MW MW MW MW MWW MH"
			def scoreFn(comp: Composition) = ScoreFactory.musicFinder(comp)
			override val seed = None
		}

		val tables = new Tables(searchDef)
		tables.buildPrepTables()

		val file = "onepartfiles\\BCDLMSAYGV.txt"
		val comps = loadComps(file, searchDef.getCompPlan, tables)

		//for (comp<-comps)
		val comp = comps.head
		//replaceMethods(comp, List(deva, malpas, glasgow, yorkshire, superlative), new MusicCount(searchDef.getCompPlan.musicDefs))
		replaceMethods(comp, List(glasgow, venusium), new MusicCount(searchDef.getCompPlan.musicDefs))
		//showMusicPerMusic(comp, methods, plan.musicDefs)

	}


	def loadComps(filename: String, plan: CompositionPlan, tables: Tables) =
	{
		val source = Source.fromFile(filename)
		val loader = new TableCompositionLoader(tables, methods)
		println("Loading seed compositions from file "+filename)
		loader.loadFromFile(source)
	}

	def showMusicPerMusic(comp: Composition, methods: List[NamedMethod], musicDefs: Array[Music])
	{
		for (method <- methods)
		{
			val leads = comp.allLeads.filter(_.method==method)
			val music = new MusicCount(musicDefs).count(leads.flatMap{_.getRows})
			print(method+" "+music+" ")
		}
		println(comp)
	}

	def replaceMethods(comp: Composition, methods: List[NamedMethod], music: MusicCount)
	{
		val method = methods.head
		println("Attempting to replace "+method+" for comp "+comp)
		val (replaceableLeads, fixedLeads) = comp.allLeads.partition{(l)=> methods.contains(l.method)}
		val fixedRows = fixedLeads.toSet.flatMap{(l:Lead)=> l.getRows}
		val allPns = (GoodPn(method.nbells).oneConsec.toSet - PN("18"))
		val tdPns = trebleDodgingPns(method.nbells, allPns)
		replaceMethods(comp.toString, methods, tdPns, music, fixedRows, replaceableLeads, 0, acceptAllMusic)
	}

	case class PruneVals(firstHLRows: List[Row], music: MusicCount)
	{
		def nextMusic(currentRows: List[Row]) = music.count(currentRows)
		def next(currentRows: List[Row]): PruneVals = next(currentRows, nextMusic(currentRows))
		def next(currentRows: List[Row], newMusic: MusicCount): PruneVals = PruneVals(currentRows.head::firstHLRows, newMusic)
	}

	/** Level, current rows, current PruneVals => Some(new PruneVals) if we can carry on */
	type PruneFn = (Int,List[Row],PruneVals) => Option[PruneVals]

	def acceptAllMusic(level: Int, currentRows: List[Row], pv: PruneVals) = Some(pv.next(currentRows))

	def replaceMethods(compDesc: String, methods: List[NamedMethod], tdPns: List[List[PN]], music: MusicCount,
										 fixedRows: Set[Row], replaceableLeads: List[Lead], level: Int, pruner: PruneFn)
	{
		val method = methods.head
		val (replaceThisTime, replaceNextTime) = replaceableLeads.partition{_.method==method}
		def getFirstAndLastRows(lead: Lead) = {val rows = lead.getRows; List(rows.head, rows.last)}
		val replaceHLs = replaceThisTime.flatMap{getFirstAndLastRows(_)}
		var high = 0

		def replaceRows(currentRows: List[Row], rowsInComp: Set[Row], revPnsChosen: List[PN], tdPns: List[List[PN]], pv: PruneVals)
		{
			val newRowsInComp = rowsInComp++currentRows
			if (newRowsInComp.size==rowsInComp.size+currentRows.size)
			{
				pruner(level, currentRows, pv).foreach{(newPV)=>
					if (tdPns.isEmpty)
					{
						// First two rows are assumed to come from the two halfleads of the same (first) lead
						val hlPn = currentRows.head.pnBetween(currentRows.tail.head)
						if (hlPn.isDefined && GoodPn.isAllowableConsecutivePn(hlPn.get, revPnsChosen))
						{
							val leadPns = (hlPn.get :: revPnsChosen).reverse.tail
							val flagNew = if (method.lead.startsWith(leadPns)) "" else "NEW "
							if ((level>0 || flagNew.length>0) && acceptMethod(method.nbells, leadPns))
							{
								println(" " * level + method + " " + flagNew + PN.output(leadPns) + " " + newPV.music + " " + compDesc)
								if (!methods.tail.isEmpty)
									replaceMethods(compDesc, methods.tail, tdPns, newPV.music, newRowsInComp, replaceNextTime, level + 1, pruner)
							}
						}
					}
					else
					{
						for (pn <- tdPns.head)
							if (acceptableConsecutivePn(pn, revPnsChosen))
								replaceRows(currentRows.map(_.apply(pn)), newRowsInComp, pn::revPnsChosen, tdPns.tail, newPV)
					}
				}
			}
		}

		replaceRows(replaceHLs, fixedRows, List(method.leadheadPN), tdPns, PruneVals(List(replaceHLs.head), music))
	}

	def acceptMethod(nbells: Int, halfLeadPNs: List[PN]): Boolean =
	{
		var method = NamedMethod("NEW", nbells, halfLeadPNs, PN("12"))
		method.longestRunInSameDodgingPosition < 10
	}

	val MaxConsecs = 4

	def acceptableConsecutivePn(pn: PN, revPrevPN: List[PN]) =
		GoodPn.isAllowableConsecutivePn(pn, revPrevPN) && limitConsecs(MaxConsecs, pn :: revPrevPN)

	def limitConsecs(max: Int, pn: List[PN]) = pn.map(_.consecutives.size).sum < max


	def trebleDodgingPns(nbells: Int, allpns: Iterable[PN]):List[List[PN]] =
	{
		var tdPns:List[List[PN]] = Nil
		for (i <- 1 until nbells by 2)
		{
			val dodgingPns = pnsAllowingMoveUp(i, allpns)
			tdPns::= dodgingPns
			tdPns::= dodgingPns
			tdPns::= dodgingPns
			if (i+1<nbells)
				tdPns::= pnsAllowingMoveUp(i+1, allpns)
		}
		tdPns.reverse
	}

	def pnsAllowingMoveUp(from: Int, pns: Iterable[PN]) =	pns.filter {_.allowsSwap(from)}.toList

}