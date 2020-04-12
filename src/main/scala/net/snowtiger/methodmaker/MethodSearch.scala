package net.snowtiger.methodmaker

import net.snowtiger.ringing._
import net.snowtiger.spliced.MethodAssessor

/**
 * Method search - generic parameters allow a lot of control over pruning
 * @author mark
 */
abstract class MethodSearch[V <: MethodSearchVariables, P <: MethodSearchParams](nbells: Int) extends MethodSearchBase(nbells)
{
	val longNthsAllowed = false

	var nFound: Long = 0
	var highestScore = 0

	def tdSearch(searchParams: P) = search(genTDPath(), searchParams)

	/** Separate n parameters allows little paths < nbells */
	def plainSearch(n: Int, searchParams: P) = search(genPlainPath(n), searchParams)

	def search(treblePath: List[Int], searchParams: P)
	{
		val pns = getPNsForPath(treblePath)
		//val pns = avon.lead.slice(0,24).toList.map(List(_))
		//val pns = PN.parse("34-3T.16-56-3T.14-7T.1258.34.90.12.3670.1458-12.367T.1458.90-1T").toList.map(List(_))
		doSearch(pns, searchParams)
	}

	def doSearch(pathPNs: List[List[PN]], searchParams: P)
	{
		println
		println("SEARCHING...")
		val searchVars = initialSearchVars(searchParams)
		doSearch(Nil, pathPNs, initialSearchVars(searchParams))
		println("Found "+nFound+" methods with best score = "+highestScore)
	}

	def initialSearchVars(searchParams: P): V

	def doSearch(revPNs: List[PN], pnsLeft: List[Seq[PN]], searchVars: V): Unit =
	{
		if (pnsLeft.isEmpty)
			foundMethod(revPNs.reverse, searchVars)
		else
		{
			val newFalseRows = getFalseRows(searchVars)
			if (newFalseRows.isDefined)
			{
				val newScore = searchVars.score + searchVars.searchParms.scoreFn(searchVars.lastRow)
				val required = requiredScore(searchVars.nRows + 1, searchVars.searchParms)
				if (newScore >= required)
					pnsLeft.head.foreach{ doSearch(_, newScore, newFalseRows.get, revPNs, pnsLeft.tail, searchVars) }
			}
		}
	}

	def doSearch(pn: PN, newScore: Int, newFalseRows: Map[String,Set[Row]], revPNs: List[PN], pnsLeft: List[Seq[PN]], searchVars: V): Unit =
	{
		if (searchVars.searchParms.acceptPN(pn, revPNs))
		{
			val nextRow = searchVars.lastRow.apply(pn)
			val newSearch = tryNext(nextRow, newFalseRows, newScore, searchVars)
			if (newSearch.isDefined)
				doSearch(pn :: revPNs, pnsLeft, newSearch.get)
		}
	}

	def requiredScore(nRows: Int, searchParms: MethodSearchParams) = nRows*searchParms.requiredScorePerRow

	def tryNext(nextRow: Row, newMaskedRows: Map[String, Set[Row]], newScore: Int, searchVars: V): Option[V]

	def foundMethod(pns: List[PN], searchVars: V)
	{
		val searchParms = searchVars.searchParms
		if (searchParms.acceptMethod(pns, searchVars.score))
			for (lhPN <- goodPN.lhPN; if (searchParms.acceptLHPN(lhPN, pns)))
			{
				val method = NamedMethod("Search", nbells, pns, lhPN)
				if (searchParms.acceptMethod(method))
				{
					val stats = MethodSearchStats(method)
					if (searchParms.acceptMethod(stats))
						outputMethod(searchVars, stats)
				}
			}
	}

	def getFalseRows(searchVars: V): Option[Map[String,Set[Row]]] =
	{
		val lastRow = searchVars.lastRow
		val treblePlace = lastRow.placeOf(1)
		val posKey = lastRow.signChar+treblePlace
		val negKey = (if (posKey.startsWith("+")) "-" else "+")+treblePlace
		val posFalseRows = searchVars.searchParms.getFalseRows(lastRow, positiveFalsePerms)
		val negFalseRows = searchVars.searchParms.getFalseRows(lastRow, negativeFalsePerms)
		val oldPosSet = searchVars.maskedRows.getOrElse(posKey, Set())
		val oldNegSet = searchVars.maskedRows.getOrElse(negKey, Set())
		val newPosSet = oldPosSet ++ posFalseRows
		val newNegSet = oldNegSet ++ negFalseRows
		if (newPosSet.size>=oldPosSet.size+posFalseRows.size && newNegSet.size>=oldNegSet.size+negFalseRows.size)
			Some(searchVars.maskedRows + (posKey -> newPosSet) + (negKey -> newNegSet))
		else
			None
	}

	def outputMethod(searchVars: V, mStats: MethodSearchStats): Unit =
	{
		nFound+= 1
		if (searchVars.score > highestScore)
			highestScore = searchVars.score
		printMethod(searchVars, mStats)
	}

	def printMethod(searchVars: V, mStats: MethodSearchStats): Unit =
		println(mStats.stats)


}

class MethodSearchBase(val nbells: Int)
{
	val goodPN = new GoodPn(nbells)
	val bestPN = goodPN.noConsec
	val allPN = goodPN.oneConsec

	val rounds = Row(nbells)
	val hlPerm = PN("1", nbells).toPerm(nbells)
	val pbPerm = Row(nbells).apply(PN("12")).apply(hlPerm).toPerm
	val lhPerms = (0 until nbells).map{pbPerm*_}.toSet
	val positiveFalsePerms = lhPerms
	val negativeFalsePerms = lhPerms.map{_.permuteBy(hlPerm)}

	val assessor = MethodAssessor(nbells)

	val bristol12 = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	val avon = NamedMethod("Avon", 12, "x5x4.5x5.30.4x70.1.36x9.30.8x8.9x8x1", "1T")
	val snowtiger12 = NamedMethod("Snow Tiger", 12, "3x5.4x5x3x2x1xEx0x8x9.8x0.E", "12")
	val aotearoa = NamedMethod("Aotearoa", 12, "-5T-14.5T-12.3T-12-1T-ET-10.ET-18.9T-18-ET", "1T")					// j2
	val orion12 = NamedMethod("Orion", 12, "36-7T.18-9T.50.36.14-1470.5T.16-9T.30.18-14.3T.50.14-1T", "1T")	// m
	val zanussi = NamedMethod("Zanussi", 12, "-5T-14.5T-12.3T.14-12.5T.16-16.7T.58-18.9T-18-9T", "1T")			// j2
	val moontiger = NamedMethod("Moon Tiger", 12, "-5T-14.5T-12.3T-12-1T-1690-70.16.78-1T-10-9T", "1T")			// j
	val blueSilk = NamedMethod("Blue Silk", 12, "3T-5T.14-5T-1T-14-145T-14-70.16-18.9T-90-1T", "12")				// e
	val snowfish = NamedMethod("Snowfish", 12, "3T-5T.14-12-3T.14-7T.10.36-90.3T.18-12.367T.1458-90.1T", "12")		// b
	val alphaZero = NamedMethod("Alpha Zero", 12, "34-3T.16-56-3T.14-7T.10.36-90.3T.18-12.367T.1458-90.1T", "1T")	// m
	val oumuamua = NamedMethod("'Oumuamua", 12, "-3T-147T-12.5T.18.34.78-1T-12-1450.38.16.38.1T.70-18.ET", "1T")	// j2

	val bristol10 = NamedMethod("Bristol", 10, "x50x14.50x50.36.14x70.58.16x16.70x16x10", "10")						// g
	val snowtiger10 = NamedMethod("Snow Tiger", 10, "3x5.4x5x3.2x9.8x6x7.6x8.9", "12")
	val new1 = NamedMethod("New 1", 10, "30-50.14-50-30-12-10-16-70.1258.34-10", "12")
	val precambrian = NamedMethod("Precambrian", 10, "34x50.14x50x36.14.78x18x16x70.16x18.90", "12")		// f
	val bristol8 = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")				// mx, cps
	val conistonBluebird = NamedMethod("Coniston Bluebird", 8, "-3-4-2-1-2-5.4-6.7", "12" )	// b

	/*
	println("Max:")
	println(MethodSearchStats(bristol12).stats)
	println(MethodSearchStats(avon).stats)
	println(MethodSearchStats(snowtiger12).stats)
	println(MethodSearchStats(aotearoa).stats)
	println(MethodSearchStats(orion12).stats)
	println(MethodSearchStats(zanussi).stats)
	println(MethodSearchStats(moontiger).stats)
	println(MethodSearchStats(blueSilk).stats)
	println(MethodSearchStats(snowfish).stats)
	println(MethodSearchStats(alphaZero).stats)
	println(MethodSearchStats(oumuamua).stats)
	*/
	/*
	println("Royal:")
	println(stats(bristol10))
	println(stats(snowtiger10))
	println(stats(new1))
	println(stats(precambrian))
	println("Major:")
	println(stats(bristol8))
	println(stats(conistonBluebird))
	println(stats(NamedMethod("Abaia", 8, "56-56.14-56-36-14-58-16-58", "12")))
	println(stats(NamedMethod("Basilisk", 8, "58-34.16-56.12.36-34-38-14-58", "12")))
	println(stats(NamedMethod("Cerberus", 8, "-58-14-58-38-12-38.56-14.58", "12")))
	println(stats(NamedMethod("Djinn", 8, "-58-16-56-16.34-12.38.14-16.58", "12")))
	println(stats(NamedMethod("Ettin", 8, "-36-16.58-56.38-12-38.14-34.58", "12")))
	println(stats(NamedMethod("Fenrir", 8, "-56-1458-58-38.12-12.58-14-38", "12")))
	println(stats(NamedMethod("Gorgon", 8, "38-58.16-56-36-12-58.16-12.58", "12")))
	println(stats(NamedMethod("Hydra", 8, "-38-1458-12-38.12-12.58-14-18", "12")))
	println(stats(NamedMethod("Incubus", 8, "-56-16-56-38-12-58-14-18", "12")))
	println(stats(NamedMethod("Kraken", 8, "-38-14-56-38-12-38.16-12.58", "12")))
	println(stats(NamedMethod("Marshwiggle", 8, "34-36.1458-58-16-12-58.16-36.78", "12")))
	println(stats(NamedMethod("Naiad", 8, "58-38.16-56-16-12-38-12-18", "12")))
	println(stats(NamedMethod("Ouroboros", 8, "58-58.14-56-16-34-1458-16-38", "12")))
	println(stats(NamedMethod("Panserbjorn", 8, "-34-14-56-16-12-58.34-14.58", "12")))
	println(stats(NamedMethod("Roc", 8, "58-36.14.56-56.38-12-1458.36-34.58", "12")))
	println(stats(NamedMethod("Sphinx", 8, "58-38.14-58-16-12-58.36-14.38", "12")))
	println(stats(NamedMethod("Titan", 8, "-36-1458-56-16.34-12.38-16-18", "12")))
	println(stats(NamedMethod("Ungoliant", 8, "-58-16-56-16-34-38-16-58", "18")))
	println(stats(NamedMethod("Valkyrie", 8, "-56-14.56-56.38-12-38-34-58", "12")))
	println(stats(NamedMethod("Wyvern", 8, "-58-1458-56.12.38-12-38-56-78", "12")))
	println(stats(NamedMethod("Yeti", 8, "58-36.14-12-36.12-12.38.12-34.78", "12")))
	println(stats(NamedMethod("Zombie", 8, "-34-14.56-58.36.12-12.58.16-16.58", "12")))

	println(stats(NamedMethod("Earis PB Course 1", 8, "-5-4-56-36-4-5.36.2-7", "12")))
	println(stats(NamedMethod("Earis PB Course 2", 8, "-5-4.5-5.36.4-34.5-2.56.1", "18")))
	*/

	def getPNsForPath(treblePath: List[Int]): List[List[PN]] =
		treblePath.sliding(2).map{ (xs)=> genPNsForTrebleTransition(xs.head, xs.tail.head) }.toList ++
				List(goodPN.hlPN(treblePath.last))

	protected def genPNsForTrebleTransition(pos1: Int, pos2: Int) =
		allPN.filter{ (pn)=> pnAllowsTrebleTransition(pn, pos1, pos2) }

	protected def pnAllowsTrebleTransition(pn: PN, pos1: Int, pos2: Int) =
	{
		def genTrebleRow(pos: Int) = Row( ("*"*(pos-1))+"1"+("*"*(nbells-pos)) )
		val startRow = genTrebleRow(pos1)
		val endRow = genTrebleRow(pos2)
		startRow.apply(pn)==endRow && acceptPN(pn, pos1, pos2)
	}

	protected def acceptRightPlaceAbovePN(pn: PN, treblePos1: Int) =
		treblePos1%2==nbells%2 || pn.placesAbove(treblePos1).isCross

	protected def acceptRightPlaceBelowPN(pn: PN, treblePos1: Int) =
		treblePos1%2==0 || pn.placesBelow(treblePos1).isCross

	def genTDPath() = genPlainPath(nbells).grouped(2).flatMap{ (xs)=> xs++xs }.toList

	def genPlainPath(n: Int) = (1 to n).toList

	/** Called during establishment of treble-path */
	def acceptPN(pn: PN, treblePos1: Int, treblePos2: Int) = true
	//acceptRightPlaceAbovePN(pn, treblePos1)

}

class StandardMethodSearch(nbells: Int) extends MethodSearch[MethodSearchVariables,MethodSearchParams](nbells)
{
	override def initialSearchVars(searchParams: MethodSearchParams) = new MethodSearchVariables(searchParams, nbells)

	override def tryNext(nextRow: Row, newMaskedRows: Map[String, Set[Row]], newScore: Int, searchVars: MethodSearchVariables) =
		Some(searchVars.makeNext(nextRow, newMaskedRows, newScore))

}

class PathTrackingSearch(nbells: Int) extends MethodSearch[PathBasedSearchVars, PathSearchParams](nbells)
{
	override def initialSearchVars(searchParams: PathSearchParams) = new PathBasedSearchVars(searchParams, nbells)

	override def tryNext(nextRow: Row, newMaskedRows: Map[String, Set[Row]], newScore: Int, searchVars: PathBasedSearchVars) =
	{
		val newPaths = searchVars.paths.map((p)=> (p._1, nextRow.placeOf(p._1)::p._2))

		val requiredPathScore = searchVars.pathSearchParms.calcRequiredIncrementalPathScore(searchVars.nRows+1)
		val newPathScore = searchVars.pathScore + newPaths.values.filter(hasOddPoints).size
		if (newPathScore>=requiredPathScore.toInt)
			Some( searchVars.makeNext(nextRow, newMaskedRows, newScore, newPaths, newPathScore))
		else
			None
	}

	override def printMethod(searchVars: PathBasedSearchVars, mStats: MethodSearchStats) =
		println(mStats.stats+", PS="+searchVars.pathScore)

	/** Returns true if last three blows of the path have an odd point - pointing down into an even place, or up into an odd. */
	def hasOddPoints(path: List[Int]): Boolean =
	{
		def hasOddPoints(a: Int, b: Int, c: Int) =
		{
			if (a%2==0)
				a<b && b>c
			else
				a>b && b<c
		}

		path match
		{
			case a::b::c::rest => hasOddPoints(a,b,c)
			case _ => false
		}
	}


	val highestOpeningPlace = if (nbells<=10) 5 else 7
	def decentOpeningPN(pn: PN) = (highestOpeningPlace until nbells).forall(!pn.isPlace(_))

	/** TD methods only! Gets rid of nasty opening PN like 78, 9T */
	def forceDecentOpeningPNs(pns: List[List[PN]]) =
		pns.slice(0, 3).map(_.filter(decentOpeningPN)) ++ pns.tail.tail.tail

}

object MethodSearch
{
	val best1280 = "Deva, Eucalyptus, Midhopestones, Eleuthera, Folly Hall, Venusium, Rhodium, Hat and Feathers, Bimini, Zyyi, Todmorden, Ethanim, Queenborough, St Margaret's, Commonwealth, Jevington, Northwich, Evora, Bearwood, Barbuda, Dunster, Whittlesey, Whirlow, Yateley, Cranborne Chase, Llanbedr Gwynllwg, Bristol Parkway, Ceredigion, Afan, Red Tilapia, Firbeck, Taplow, East Molesey, Peterstone Wentloog, Yelling, Bristol, Ditchling, Dumbledore, Tuesday, Muppet, how, Bolsterstone, Stanedge Pole, Old Trafford, Windmill Hill, Zyyian, Turneffe, Zinc, Crookes, Zonda, Harleston, Sublime, Winford, Xarob, Brynbuga, Mark, Tin, Cantium, Love Bug, Quenby, Longley, Llhotse, Burnham Beeches, Packington, Abel Magwitch, Hampshire, Petworth, Caerffili, Voran, Brecon Castle, Moorgreen, Butis, Bibroci, Hobgoblin, Jorvik, Elaine, Shefford, Zircon, Chelmsford, Radon, Rhondda, Partridge Inn, Jellied Eel, Bond 007, Priddy Fair, Mahoganous, Southgate, Bus Pass, Pershore, Edgecombe, Speedball, Arbeia, Xeres, Waladli, Reverse Dordrecht, Dordrecht, CCL, Abbots Bromley, Goring, Hawthorn Leslie, Booth, Coseley, Giotto, Gillett, Bradfield, Ton, Lamport, Tepic, Eardisland Manor, Bravo!, Ambleside, Ugborough, Terni, Taofu, Taree, Vicarage, Grundisburgh, Haffeboche, Lucas, Town, treet, Bolonium, Bloodstone, Juror, Evans, Carlton Curlieu, Oulu, Usk, Double Dordrecht, Celebration, Brandau, Camulosessa, Ibb, Three Point Turn, Fowlmere, Nempnett Thrubwell, Operation Market Garden, Furness, Hathern, Quorndon, Keilmerse, Norfolk, Nine Elms, St Jude's, Foxton, Mrs Trellis of North Wales, Humberstone, Shambles, Gwynedd, Jacob, Holloway Road, Limehouse, Alobergium, Minox, Dalston, Meden Vale, Thulium, Zany, Osbaldwick, East of Arcadia, Iznit, Metz, Galveston, Yorkville, Shreveport, Aemodae, Alvionus, Tavus, Jaca, Ruel, Rusne, Qom, Erophobia, Queenhithe, Dereham, Addenburgh, Darwin, Drogheda, Dublin, Thorpe, atchville, Micklegate Bar, Dolomite, Andamooka, Begesse, Crystal Palace, Blue Gum, Moose, Norwich, Navesberie, Docklands, Loatland, Arkansas, Mytholm, Burghill, Dore, Unnilbium, Elsworth, Stoneville, Gryffindor, Erbium, Corinium Dobunnorum, Jarrow, Langport, Quimper, Virgo, Reverse Dublin, Ventoux, Friday, West Croydon, Ephebe, Octarine, Gloucestershire, Nantwich, Fairfield, Purbeck, Fillongley, Superlative, Duke of Norfolk, Jumper's Town, Aebudae, New Mills, Whitburn, Black Dragon, East Hoathly, Oakley, Yawthorpe, Spanner, Bishop Auckland, Ouse, Riley Green, Cornwall, Kenninghall, Puxley, Double Dublin, Niton, Zoisite, High Willhays, Badshot Lea, Umbridge, Gressenhall, Edmundsbury, Premier, Stanton, Oswestry, Harwich, Piglet, Princess Pocahontas, Golden Heart, Spitalfields Market, Xavier, St Nicholas' Warwick, Baldock, Lancashire, Manchester, Peak District, Ashford, Clevedon, Rothamsted, Bolton, Peterborough, Quodlibet, Portsmouth, Excalibur, Bower Grove, High Neb, Cobb County, Buntingford, Pinner Green, Naphill, Lord Deramore, Jovium, Shoesmith Hall, Swindon Town, Vacaville, Sagar, Razan, Yibna, Thetford, Sahy, Marib, Ipin, Ipu, Tao, Tobel, Soron, Stip, Sohar"
	val best1280_2 = "Eves Watergate Eggplant Quarkonium Dido Bar T'at Severus Curve Sibsey Heyford Prince Consort Illinium Trang Tasil Sucha Bouchavesnes Montgomery Essex Yaverland Caicos Earlterndale Zif Kersey Flowery Field Forty Mirfield Moor Derwent College Millcote Actinium Bletchingley K2 Castra Exploratum Seibo Rypin Raahe Intercalary Annable's London Vyrnwy Painswick Wight Allotment Settelemo Scrumpy and Western East Raynham Kirkby-in-Ashfield Zephyranthes Ewan Lord of the Rings Odyssey Upware Zakynthos Finchingfield Bishops Frome Bells Madurai Spitalfields Festival Marsium Abbeydale Moonville Nice Yarra Antimony Frodsham Charleston South Dunedin Ambergate Narrowgate Xantium Bosmere Fantastic Four Candles Caldy Xenophon Alderney Rollestone Ugley Monkton Combe Grantsville Quietus Guthlaxton Desperate Housewives Orionids Foulness Grestun Pitmaston Kornwall Rabbit Cray Frindsbury Hurworth Evenlode Nearest Canal Corley Uckfield Lapis Lazuli Beryllium Macclesfield Decadium Erythrite Gaulby Caerfyrddin May Aberhonddu Yosemite Benson Moorside Yeaminster 109 Heptonstall Kingswood Llanfeugan Rustat Ictis Insula Woodgett Minerve Peel Hinkleborough Birstall Sunday Carbon Bremia Zermatt Kilmington Saturday Adlington Vinovia Rutupiae Farthest Canal Calcium Monday Sheaf Georgia Rivelin Innermessan Clarendon Winchmore Hill San Marino Orpington Fourcore Stranton Woodfield Hebburn Newburn Bushbury Elswick Staindrop Sawston Trelawney Ytterbium Kyneburgha Aluminium Hill Ridware Ingoldmells Tillingham Sandy Holbeach Wild Hare Fen Ditton Newton Nottage Zeals Marshland Calais Horsham Liden Blue John Lower Place Middle Place Veteranorum Lexden Dagenham Porthcawl Vieux Fort Tebeth Ardastra Raydale Coccium Moricambe Meze Yeading Tripontium Brick Lane Ytterbite HM & HG Kiveton Small Hall Fulwell Easthampstead Saya Seda Rori Sarna Qus Quyon Earn Stretford Manorbier Castle Erasipteron Bolsoveri Adron Berne Blackburn Kings Ripton Little Coggeshall Holm Cultram Bedmond Truro Woodhouse Raunds Capheaton Mullengudgery Morfa Mawddach Belisama Goodricke College Venn Ottery Kearsley Bulbourne Bohrium Machen Blaenafon Avon Gorge Upton Hellions Compton Gifford Wantage Alvington Barnstormer Kalna Orbe Chanf Hisya Griva Gams Kutu Kem Sutterton Half-Century Loxley Flerovium Illston Tungsten Zagreb Wapping Americium Stibium Wicksteed Transuranium Skelton Carlton Xaintrey Treales Romsey Abbey Xertigny Gruz Coritani Delgovicia Munsley Selwood Tiger Meaux Abbey Abney Park Datchworth Gippeswyck Allingham Barbican Faringdon Ormerod Road Quarr Korpo Gorga Golmo Karaj Fukui Tendring Sudbury Coronation Thursday Norwich City FC Broadgreen Edenham Rhinestone Kakapo Newton Hall Bower Mount Jairu Wales William Penny Brookes Niccolite Peasholme Green FILO Honiton Quintessential Tintagel Castle Abergavenny Castle Cheesden Lubcloud Morgannwg Ely Ruthenium Droue Hedgerley St Brannock's Gem Potassium Cheshire Mercury Old Bell Tuao Temta Sylt Tabas Catesby Reverse Raunds JJLG Iesah The Moon Duomilia Chicksand Hunters Bar Krishna Yuca Isurium Camborne Almar Guildford Kingston Brecknockshire Oddington Freehouse Abbott Jordan Quatt Lendal Tahoe Muse Aardwolf Carol Crayford Yalu Ilford Nuneaton Fishergate Bar Fryerning Westminster Allendale Pentonville Road Roundway Grimsthorpe Kakadu Ecclesall Battlefield Millfield Branksome Bruxelles Oadby Yapton Marsh Freasley Vaguely Tuffley Lenchwick Bedwardine Oswaldtwistle Flixton Marple Thropton Cotwardine Kirkham All Fools' Loki's Gate Birstwith North Mymms Zigadenus Onnum Roghadel Jarvis Brook Xceat Netherfield Chandlers Lount Cymru Snibston Fosse Whorlton Brickyard Cottage Isla Merrylees Cheshunt University ofheffield Accrington Zabini Bedford Shincliffe Market Deeping Whitwell Griffydam Aragog Tonbridge Ashington Quarry Hill Clenchwarton Its D. Peacock's Madrid Lisbon Stokesay Castle Walkden Palgrave High Ham Bright Old Cleeve Barnsbury Derwent Banwell Selhurst Samarium St Ives Wendy Kingsteignton Meerut Dale Bar Hill Stockholm Quirrel St Leonard's Tower Cumwhinton Earthium Jimwell Turnberry Nettleton Longcot Village Abbey Meads Hollowgate Shalford Fountains Abbey Lindinis Saharanpur Harston Merioneth Speightstown Northomerset Handsworth Tarvin deenlis Anniversary Amberley Chisleu Clothworkers Chafy Crescent Zillertal Aire Deira Sailly-Saillisel Beddington Elmodesham Woodall Hughenden Wraxall Amersham Skywalker Rook and Gaskill Grayshott Quapergate Hookey Richeldis de Faverches Alton Ludlow Castle Llandovery Castle Duketreet Zagorsk Oxford Circus Heighington Acton Town Aberafan Rothley Cazenove Grays Tingdene Galleywood Fulton August Alwalton Newtown Linford Hook Norton Manor Nomphlab Over Brierfield Aintree Seaview Michaelmas Radwell Henlow Gorhambury Risingun Victory Actinon Nicholas Matthew Dilithium Bitton Vatican City Hove Kingstanding Battersea Blisworth Peverel Grossular Cat's-Eye Iodine Frog Island Rochelle Evington Lewes Nickhill Queen Camel Kalium Elul Cyprus Wheathampstead Silsden Ben Lomond Huguenot Djelibeybi Air Muonium Moorfield Wyrardisbury Edgbaston Scary Canary Holme Lacy Atecotti Abravannus Frensham Pond Yitu Ratae Coritanorum Innamincka Northfleet Hinckley Cartimandua Vriconium Armagh Segelocum Durovigutum Sulloniacae Onna Cinnuis Pitman Longovicium Grassington Loidis Cambridge Blue Sulfur Ripon Harptree Buckingham Kaapstad Dubris Tomar Tori Astcote Vanbrugh College Esplanade Pemba Peixe Carnonacae Singapore Banewella Dyngalyng Sin Hill Tang Hall Botley Ystradyfodwg Yanley Ozone St Wulfram Xeranthemum Ashley Ovingdean Isca Dumnoviorum Sabrina Chorley Brancepeth Oakville Batesville Promethium Feering Beniston Meppershall Eckland Huntsham Barmouth Penmaenpool Montgomery Castle Smeeton Westerby Timelessness Catherine Natrium Grampian Tripleport Dairylea Triangles Craven The University of York Shepton Beauchamp Zoons Court Briga Alloa Porton Fulney Godmanchester Almondsbury Dymchurch Longstanton Stepney Daedalus Eyeworth Mendip Dewdrop Midhurst Whitetar Nosleepno Stateline Scotswood Haydon Wick Wau Liski Valiant Monkey Town Murca Drayton Skive Quy Tiger's-eye Zuider Zee Falmouth Hart Vendredi Treize Edlesborough Newnham-on-Severn Tallinn Dullingham Brislington Technetium Chatham Mustique Leo Foulridge Surrey Tiptree Zeitz Vyru Zarqa Jiul Wour Velp Wotho Vyatka Upolu Truk Sfax Trn Ishan Suhl Sabha Surat Serov Ravar Prai Okkak Poso Iasi Nara Nules Narva Okha Nauta Vich Urga Sturt's Desert Pea Grahamston Belvedere Loose End Pisa Xenolite Boreham Xenon Luffield Amber Onyx Strensall Oatlands Park Ruddington Mack Hailsham Bailey Achurch Kingsley Dunmore Ardotalia Gobannium Gwent Philip Xi Aquae Calidae Boresti Larkins Wednesday Yubi Mier Pevek Eide Theux Jalq Yunsi Minbu Pilos Elko Thur Janze Northumberland Whitley Letocetum Gubernaculis Lemare Sorviodunum Regni Barnes Wallis Cancer Galava Tactical Straker's Passage Umbria Round Bush Misbourne Davies Moho Lehi Mogu Hansi Luni Kovno Lugoj Ledo Kazvin January Xadier Sapcote Zhemchug Crendon Juba Hariq Joux Goor Gozo Harib Galag Farsi Cali Fafa Fyn Bam Buea Afula Mapia Iola Miska Jau Looc Hunza Hyogo Lomza Hubli Cotehill Hopetreet Windium Easterly Tattersalls Viru Lajes Ocos Birq Sami Gwa Vilna Ladiz Obo Bic Salta Guna Broxbourne Sapphire Winnie the Pooh June Taunton Westray Islay Andorra La Vella Didymium Eatonville East Pennard Menzies Wallsend Quags Corner Othorpe Crich Pearl Yaffle Johnston Morning Dew Zopf Radyr Iceland Thurncourt Mororlessness Jelly Legs Kakorrhaphiophobia Teign Valley Raglan Castle Tigger Machynlleth Stannum Kirk Yetholm Harlech Hooch Turgid Strata Florida Uri Bovey Tracey Charter Llanthony Abbey Beaune Lathkill Turramurra Wham Bar Grandpont Morganite Elcombe Hall Stretton Frithelstock Dunston Allenheads Ponteland Cotherstone Vanderlin Rotherham Platinum Mogador Glamorgan Anne's New Kettle Millcroft Bluntisham Kunzite Windsor Castle Iolite Brereton Ijork Melbourn Jade Braunton Antipodes Lindrick Dale Abington Batcombe Positronium Double Diamond Openshaw Curium Middlejay Freedom Tryes Sanvey Gate Buckminster Orton Kilby Scandium Kiribati Sivan Thorpe Arnold Jellium Rowell Old Beetley Brent Ulmus Tyringham St Andrews Went Nebard Crayfish Lavatrae Buckden Bahamas Vindolanda Emersons Green Six Nations Heslington East Dashing Erudition Advent Lazyboy Camp Palatine Westmorland Linfield Alisonian Humber Cryers Hill Newbottle Mallerstang Aardvark Kingslandt Michael Eliminator Millhouses Hanslope Weston Bognor Huntingdonshire Wicken Hasketon Fulbourn Bucks Teddington Hatfield Peverel Verlucio Dedworth"
	def main(args: Array[String])
	{
		// Search for Plain Caters methods, no scoring
		//val search = new StandardMethodSearch(9)
		//val params = new NoScoreSearch()
		//search.plainSearch(9, params)

		def printGood(m: NamedMethod) = if (m.potential4Runs>=6) println(m+",P4="+m.potential4Runs+",D="+m.difficulty)

		//MethodLibrary.surpriseLibraries.getLibrary(8).methods.filter(_.lhGroup=="b").filter(_.isRightPlace).filter(_.positiveFCH.size<=1).foreach(printGood)
		//MethodLibrary.surpriseLibraries.getLibrary(8).methods.filter(_.difficulty>70).filter(_.positiveFCH.size<=1).filter(_.potential4Runs>=16).foreach((m)=> println(MethodSearchStats(m).stats))
		//MethodLibrary.surpriseLibraries.getLibrary(8).methods.filter(_.difficulty>70).filter((m)=> best1280_2.contains(m.name)).foreach((m)=> println(MethodSearchStats(m).stats))

		//MethodLibrary.surpriseLibraries.getLibrary(8).methods.filter(_.lead.head==PN("38")).filter(_.isGoodPN).filter(_.potential4Runs>20).filter((m)=> best1280_2.contains(m.name)).foreach((m)=> println(MethodSearchStats(m).stats))

		//MethodLibrary.surpriseLibraries.getLibrary(8).methods.filter((m)=> Set("Yorkshire","Superlative","Bristol","Glasgow","Belfast","Chomolungma").contains(m.name)).foreach((m)=> println(MethodSearchStats(m).stats))
		//MethodLibrary.delightLibraries.getLibrary(8).methods.filter(_.difficulty>=90).foreach((m)=> println(MethodSearchStats(m).stats))
		//val tbMethods = MethodAssessor(8).parseMethods(Source.fromFile("TrebleBobMajor.txt"))
		//tbMethods.sortBy(-_.difficulty).foreach((m)=> println(MethodSearchStats(m).stats))

		// Search for TD Major methods, scored by potential 4-runs and difficulty
		/*
		val search = new StandardMethodSearch(8)
		//val params = new NoScoreSearch()
		val params = new Potential4RunDifficultySearch(1, 14, 0, 90)
		search.tdSearch(params)
		print("Found: "+search.nFound)
		*/

		// Search for TD Major methods, scored by potential 4-runs
		val search = new StandardMethodSearch(8){
			override def acceptPN(pn: PN, treblePos1: Int, treblePos2: Int) =
				acceptRightPlaceAbovePN(pn, treblePos1) && acceptRightPlaceBelowPN(pn, treblePos1)
		}
		//val params = new Potential4RunSearch(0.5, 12)
		val params = new Potential4RunSearch(0.3, 8){
			override def acceptMethod(method: NamedMethod) =
				super.acceptMethod(method) && method.positiveFCH.size<=1 // && method.isRightPlace && method.lhGroup=="b"
		}
		search.tdSearch(params)

		// Search for TD methods containing the plain course of PB at the same stage
		/*
		val dncbm = NamedMethod("DNCB", 8, "-14-36-58-18", "18")
		val dncbmCourse = dncbm.generateFullCourse(Row(8)).toSet
		val nBells = 8
		val search = new StandardMethodSearch(nBells){
			override def requiredScore(nRows: Int, searchParms: MethodSearchParams) = super.requiredScore((nRows/4)*4, searchParms)
			//override def getPNsForPath(treblePath: List[Int]) = super.getPNsForPath(treblePath).map(_.filter(bestPN.contains).filter(_.nPlaces<=2))
			//override def getPNsForPath(treblePath: List[Int]) = PN.parseToList("-58-14.58-58.36.14-14.58-14-18").map(List(_))
		}
		val params = new MethodSearchParams(0.5d, nBells){
			//override def scoreFn(row: Row) = if (row.isPlainBob) 1 else 0
			override def scoreFn(row: Row) = if (dncbmCourse(row)) 1 else 0
			//override def getFalseRows(lastRow: Row, falsePerms: Set[Perm]) = super.getFalseRowsCPS(lastRow, falsePerms)

			override def acceptPN(pn: PN, revPrevPNs: List[PN]) = super.acceptPN(pn, revPrevPNs) && !isKentPlaces(pn, revPrevPNs) /*&&
					(pn.consecutives.isEmpty || revPrevPNs.count(!_.consecutives.isEmpty)<6)*/
		}
		search.tdSearch(params)
		*/

		// Search for Double TD methods with no consecutive places
		/*
		val nBells = 12
		val search = new StandardMethodSearch(nBells){
			override def getPNsForPath(treblePath: List[Int]) = super.getPNsForPath(treblePath).map(_.filter(bestPN.contains).filter(_.nPlaces<=2))
		}
		val params = new NoScoreSearch(){
			override def acceptMethod(method: NamedMethod) = method.isDouble && super.acceptMethod(method)
			// Force Double methods
			override def acceptPN(pn: PN, revPrevPNs: List[PN]) = super.acceptPN(pn, revPrevPNs) &&
				pnIsDouble(pn, revPrevPNs) && niceSections(pn, revPrevPNs)
			def pnIsDouble(pn: PN, revPrevPNs: List[PN]): Boolean =
			{
				val nPrev = revPrevPNs.size
				if (nPrev<nBells-1 || nPrev>=nBells*2-1)
					true
				else if (nPrev==nBells-1)
					pn.reverse(nBells)==pn
				else
					pn.reverse(nBells)==revPrevPNs((nPrev-nBells)*2+1)
			}
			def niceSections(pn: PN, revPrevPNs: List[PN]): Boolean =
			{
				val nPrev = revPrevPNs.size
				if (nPrev==0 || (nPrev+1)%4<2)
					true
				else if (pn.isCross)
					!revPrevPNs.head.isCross
				else
					revPrevPNs.head.isCross
			}
		}
		search.tdSearch(params)
		*/

		// Search for TD Maximus methods, scored by course-4
		/*
		val search = new StandardMethodSearch(12)
		val params = new Course4Search(5.0, 240) {
			override def acceptMethod(method: NamedMethod) =
				super.acceptMethod(method) && method.difficulty>=120 && method.longestRunInSameDodgingPosition<=10
		}
		search.tdSearch(params)
		*/

		// Search for Delight Maximus methods with Bristol/Avon start, scored by course-4
		/*
		val search = new StandardMethodSearch(12)
		val pn1t = PN("1T")
		val params = new Course4Search(8.5, 240) {
			override def getFalseRows(lastRow: Row, falsePerms: Set[Perm]) = super.getFalseRowsCPS(lastRow, falsePerms)

			override def acceptPN(pn: PN, revPrevPNs: List[PN]) = super.acceptPN(pn, revPrevPNs) && {
				if (!search.bestPN.contains(pn))
					revPrevPNs.count(!search.bestPN.contains(_))<3
				else if (revPrevPNs.size==19)
					pn==pn1t || revPrevPNs.contains(pn1t)
				else
					true
			}

			override def acceptMethod(method: NamedMethod) =
				super.acceptMethod(method) && method.difficulty>=120 && method.longestRunInSameDodgingPosition<=8
		}
		//val pns = PN.parse("-5T-14.5T-5T").toList.map(List(_)) ++ search.getPNsForPath(search.genTDPath()).slice(7, 24)
		//search.doSearch(pns, params)
		search.tdSearch(params)
		*/

		// Search for TD Maximus methods, scored by difficulty and course-4
		//val search = new PathTrackingSearch(12)
		//val params = new Course4DifficultySearch(6.4, 208, 12, 220)
		//val params = new Course4DifficultySearch(6.6, 208, 10, 220)
		//val params = new Course4DifficultySearch(8.0, 224, 4, 220)
		//val params = new Course4DifficultySearch(8.0, 220, 6, 220)
		//search.tdSearch(params)

		// Search for TD Maximus methods with 90ET off the front ala Red Dressing Gown, scored by difficulty and course-4
		//val search = new PathTrackingSearch(12)
		//val params = new Course4DifficultySearch(7.5, 208, 4, 220)
		//val pns = PN.parse("3T-5T.14-5T-1T-14").toList.map(List(_)) ++ search.getPNsForPath(search.genTDPath()).slice(10, 24)
		//search.doSearch(pns, params)

		// Overwork search for TD Maximus methods, scored by difficulty and course-4
		/*
		val existingMethod = NamedMethod("To Replace", 12, " -3T-14.9T.1270.5T.10.347T.90-1T-12-1450.38.16.38.1T.70-18.ET", "1T")
		val search = new PathTrackingSearch(12){
			override def tryNext(nextRow: Row, newMaskedRows: Map[String, Set[Row]], newScore: Int, searchVars: PathBasedSearchVars) =
			{
				val n = searchVars.nRows+1
				val treblePassPos = n/2+2
				val isTreblePassRow = (n+1)/2%2==0 && treblePassPos<=nbells
				if (isTreblePassRow && nextRow.bellAt(treblePassPos)!=existingMethod.firstLead(n).bellAt(treblePassPos))
					None
				else
					super.tryNext(nextRow, newMaskedRows, newScore, searchVars)
			}
		}
		val pns = replaceOverwork(existingMethod, search)
		val nicePns = search.forceDecentOpeningPNs(pns)
		val params = new Course4DifficultySearch(3.2, 208, 2, 200)
		//val methodPns = existingMethod.lead.slice(0, 24).toList.map(List(_))
		search.doSearch(nicePns, params)
		*/

		def printIfTricky(m: NamedMethod) = if (m.difficulty>200 && m.coursingFourScore()>2) println(m+" C4="+m.coursingFourScore()+" D="+m.difficulty+" OP="+m.oddPointCount)
		def printIfGood(m: NamedMethod) = if (m.coursingFourScore()>=224) println(m+" C4="+m.coursingFourScore()+" D="+m.difficulty+" OP="+m.oddPointCount)
		//MethodLibrary.surpriseLibraries.getLibrary(12).methods.foreach(printIfGood)
		//MethodLibrary.delightLibraries.getLibrary(12).methods.foreach(printIfGood)
	}

	def replaceOverwork(method: NamedMethod, search: MethodSearchBase): List[List[PN]] =
	{
		val treblePositions = method.treblePositions.slice(0, method.leadLength/2)
		val allPNs = search.getPNsForPath(treblePositions)

		def pnMatchesUnderwork(pn: PN, row: Int) =
		{
			val treblePos = treblePositions(row)
			pn.placesBelow(treblePos) == method.lead(row).placesBelow(treblePos)
		}

		allPNs.zipWithIndex.map((p)=> p._1.filter(pnMatchesUnderwork(_, p._2)))
	}

}