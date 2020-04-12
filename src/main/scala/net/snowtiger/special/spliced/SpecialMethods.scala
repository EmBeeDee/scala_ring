package net.snowtiger.special.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.MethodAssessor

import scala.io.Source

/**
 * @author mark
 */

class SpecialMethods
{
	val methodAssessor = MethodAssessor(12)
	val goodSurprise = methodAssessor.parseMethods(Source.fromFile("SurpriseMax.txt")).filter(methodAssessor.isGoodRoyal)
	val goodDelight = methodAssessor.parseMethods(Source.fromFile("DelightMax.txt")).filter(methodAssessor.isGoodRoyal)
	val goodAlliance = methodAssessor.parseMethods(Source.fromFile("AllianceMax.txt")).filter(methodAssessor.isGoodRoyal)
	val libraryMethods = goodSurprise++goodDelight++goodAlliance

	//val ariel = NamedMethod("a Ariel", 12, "x5Tx14.5Tx12.3T.14x12.5T.14x369T.70.18x18.9T.18x18.ET", "1T") // k1
	val ariel = NamedMethod("Ariel", 12, "x5Tx14.5Tx12.3T.14x12.5T.14x369T.70.18x18.9T.18x18.ET", "1T") // k1
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T") // mx
	val bowyer = NamedMethod("b Bowyer", 12, "3Tx3T.14x12x3T.14x14.5T.14x36.7T.18x18.9Tx18x9T", "12") // f
	//var barford = NamedMethod("b Barford", 12, "3x3.4x2x3.4x4.5.6x6.7.58x6.9.6x6.9", "12") // f
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T") // j
	val cantuar = NamedMethod("Cantuar", 12, "3T-3T.14-12-3T.14-14.5T.16-16.7T.58-18.9T.18-18-18.9T", "12")	//f
	val counters = NamedMethod("c Counter's Creek", 12, "x5x4.5x2.3x2x1x6x7.6x6.7x6x7", "1T")	// k1
	//val deira = NamedMethod("d Deira", 12, "34x5.4x5x36.4x2.3x2.36.1x2.36.1x2.36.1", "1T")	// mx
	val deira = NamedMethod("Deira", 12, "34x5.4x5x36.4x2.3x2.36.1x2.36.1x2.36.1", "1T")	// mx
	val deimos = NamedMethod("Deimos", 12, "34-5T.16-56-36.7T.34-1T-70.18.3T.10.3T", "1T")	//g
	val doubleDublinA = NamedMethod("u Double Dublin", 12, "x56x4.56x5.36.47.58.69.70.8x78.9x78x1", "1T")	// j
	val jabberwock = NamedMethod("JabberWock", 12, "36x56.4.5x5.6x78x8x6.9.70.6x8.9x8xE", "1T")	// j1
	val littleport = NamedMethod("Littleport", 12, "-5T-14.5T-5T.36.14-14.5T-14-18", "1T")	// mx
	val maypole = NamedMethod("Maypole", 12, "-5T-14.5T-5T.36.147T.58.169T.70.18-18.9T-18-1T", "1T")	//j
	val neptune = NamedMethod("Neptune", 12, "3x5.4x5x36.2x7.58.6x2.7.2x8.9x8x9", "12")	// c
	val parsons = NamedMethod("p Parsons Pleasure", 12, "3Tx5T.14x5Tx3Tx34x5Tx16x7T.16x16.9T.18x10.ET", "12") // d2
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T") // l
	val snowtiger = NamedMethod("Snow Tiger", 12, "3Tx5T.14x5Tx3Tx12x1TxETx10x18x9T.18x10.ET", "12") // f
	val strawberry = NamedMethod("s Strawberry", 12, "36x56.4.5x5.6x34x25x6x7.6x6.7.6x6.7", "1T")	//
	val yorkshire = NamedMethod("Yorkshire", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "12") // b
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T") // j2

	// Some le/lh variants
	val horsleydown = NamedMethod("Horsleydown", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "12") // c1
	val premiere = NamedMethod("Premiere", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18xET", "12") // c2
	val parkway = NamedMethod("Parkway", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18xET", "1T") // j1
	val woodstock = NamedMethod("Woodstock", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "1T") // g
	val belgrave = NamedMethod("Belgrave", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18x1T", "12") // a
	val indesit = NamedMethod("Indesit", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "12") // d2
	val asaphHall = NamedMethod("Asaph Hall", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "12") // f

	val goodMethods: List[NamedMethod] = List(avon, bristol, snowtiger, yorkshire, zanussi,
		ariel, bowyer, phobos, cantuar, littleport, maypole, deimos,
		deira, strawberry, neptune, doubleDublinA, counters, jabberwock)
	//val methods: List[NamedMethod] = List(bristol, snowtiger, zanussi, phobos, cantuar, maypole, deimos)

	val knownMethods: Set[String] = Set(avon, bristol, snowtiger, yorkshire, zanussi,
		ariel, phobos, cantuar, littleport, maypole, deimos, deira,
		horsleydown, premiere, parkway, woodstock, belgrave, indesit, asaphHall).map{_.namePlusClass} ++
		Set("Strathclyde S") //, "Via Gellia S")
		//Set("Strathclyde S", "Counter's Creek D", "Fallen Angel S", "Neptune S")
		//Set("Strathclyde S", "Uppington S")
		//splitString("Strathclyde S,Counter's Creek D,Strawberry S,Effingham S,Uppington S,Via Gellia S,Double Dubin S")
		//Set("Mississippi D", "Strawberry S", "Jabberwock S")
		//Set("Alnitak S", "Ariel S", "Broadgate A", "Deimos A", "Deira D", "Renfrew S", "Strathclyde S", "Thornige S", "Thurnby S", "Vax D")
		//splitString("Asaph Hall S, Ashton S, Azura D, Ballymena S, Bowcliffe S, Bristol S, Broadgate A, Centenary A, Chatteris Fen S, Clydeside S, Conisbrough S, Counter's Creek D, Counting House S, Deira D, Dereham S, Dokkumer Nieuwe Zijlen A, Double Dublin A, Dyson S, Effingham S, Enfield S, Fallen Angel S, Farndon Little Little S, Feering S, Fleet S, Fulford S, Gedney Fen S, Glastol S, Gracechurch D, Huddersfield Little Little S, Huddersfield S, Ickleton S, Jabberwock S, Jakarta S, Kimberworth S, Londin A, Lutterworth S, Micklebring S, Miele S, Neptune S, Old West River S, Palatino S, Provost A, Redland S, Riverhead S, Roaring Meg S, Sawley S, Selly Oak D, Shoreditch A, Spirit of Birmingham A, Strawberry S, Summer Lane A, Tottnam S, Trafalgar S, Uppington S, Uspenski S, Uttoxeter D, Via Gellia S, Warwickshire S, Wembley S, Whaplode Fen S, Yenisei A")
		//splitString("Deira D,Weaverham S,Neptune S,Clydeside S,Glastol S,Dyson S,Broadgate A,Asaph Hall S,Chatteris Fen S,Counter's Creek D,Miele S,Strawberry S,Dyson S,Tottnam S,Uppington S,Riverhead S")

	def splitString(s: String) = s.split(',').map{_.trim}.toSet

	/*
	val knownMethods = Set("Asaph Hall S", "Azura D", "Broadgate A", "Chatteris Fen S", "Clydeside S", "Counter's Creek D",
		"Deira D", "Dyson S", "Effingham S", "Glastol S", "Neptune S", "Tottnam S", "Uppington S", "Weaverham S", "Adventurers' Fen S",
		"Asaph Hall S", "Ashton S", "Azura D", "Ballymena S", "Bowcliffe S", "Bowyer S", "Bristol S", "Broadgate A", "Chatteris Fen S",
		"Clerkenwell S", "Clydeside S", "Conisbrough S", "Counter's Creek D", "Counting House S", "Deimos A", "Deira D", "Dereham S",
		"Dokkumer Nieuwe Zijlen A", "Double Dublin A", "Dyson S", "Enfield S", "Fallen Angel S", "Farndon Little Little S",
		"Feering S", "Fleet S", "Fulford S", "Gedney Fen S", "Glastol S", "Gracechurch D", "Huddersfield Little Little S",
		"Huddersfield S", "Ickleton S", "Jabberwock S", "Jakarta S", "Jewellery Quarter S", "Kimberworth S", "Londin A",
		"Lutterworth S", "Micklebring S", "Miele S", "Mississippi D", "Neptune S", "Old West River S", "Painswick S", "Palatino S",
		"Provost A", "Renfrew S", "Riverhead S", "Roaring Meg S", "Royal Maundy A", "Sawley S", "Selly Oak D", "Shiraz S",
		"Spirit of Birmingham A", "Strathclyde S", "Strawberry S", "Thurnby S", "Tottnam S", "Trafalgar S", "Uppington S",
		"Uspenski S", "Uttoxeter D", "Via Gellia S", "Warwickshire S", "Wembley S", "Whaplode Fen S", "Yenisei A", "Young Man's Fen S")
	*/

	/*
	for (method <- knownMethods)
	{
		val altPN = if (method.leadheadPN==PN("12")) "1T" else "12"
		val altMethod = NamedMethod("Opp "+method.name, 12, PN.output(method.firstHalfPN), altPN)
		libraryMethods.find{_==altMethod} match {
			case Some(method) => println(method.namePlusClass)
			case None => println("No library method for "+altMethod.namePlusClass)
		}
	}
		*/

}