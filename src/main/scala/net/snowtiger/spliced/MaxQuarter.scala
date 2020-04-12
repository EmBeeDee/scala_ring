package net.snowtiger.spliced

import net.snowtiger.ringing.NamedMethod
import net.snowtiger.spliced.composition.Composition
import net.snowtiger.spliced.generator.SplicedGenerator
import net.snowtiger.spliced.score.ScoreFactory
import net.snowtiger.spliced.search.SearchDefinitionBase

/**
 * @author mark
 */

object MaxQuarter extends SplicedGenerator with SearchDefinitionBase
{
	val avon = NamedMethod("Avon", 12, "x5Tx14.5Tx5T.30.14x70.1T.36x9T.30.18x18.9Tx18x1T", "1T")							// mx
	val bristol = NamedMethod("Bristol", 12, "x5Tx14.5Tx5T.36.14x7T.58.16x9T.70.18x18.9Tx18x1T", "1T")				// j
	var barford = NamedMethod("b Barford", 12, "3Tx3T.14x12x3T.14x14.5T.16x16.7T.58x16.9T.16x16.9T", "12")		// f
	val bowyer = NamedMethod("o Bowyer", 12, "3Tx3T.14x12x3T.14x14.5T.14x36.7T.18x18.9Tx18x9T", "12")					// f
	val cantuar = NamedMethod("Cantuar A", 12, "3Tx3T.14x12x3T.14x14.5T.16x16.7T.58x18.9T.18x18x18.9T", "12")		// f
	val halloween = NamedMethod("Halloween A", 12, "x3Tx14x125Tx36.14.7T.12.58.36.9T.14.70.58x16.9T.70x18xET", "12")	// b
	val kentd = NamedMethod("Kent D", 12, "34x34.1Tx12x1Tx14x3Tx16x5Tx18x7Tx10x9T", "1T")											// mx
	val littleport = NamedMethod("LittlePort", 12, "x5Tx14.5Tx5T.36.14x14.5Tx14x18", "1T")										// mx
	val lincolnshire = NamedMethod("N Lincolnshire", 12, "x3Tx14x5Tx16x7Tx18x9Tx10x18x9Tx70xET", "12")				// b
	val phobos = NamedMethod("Phobos", 12, "x3Tx14x12.5T.16x34x5Tx16x7T.16x16.7T.16x16.7T", "1T")							// l
	val rigel = NamedMethod("Rigel", 12, "36x7T.18x9T.50.36.14x1470.5T.14.36.9T.10.58x16.7T.16.70.16.ET", "1T")	// l
	val snowtiger = NamedMethod("s Snow Tiger", 12, "3Tx5T.14x5Tx3Tx12x1TxETx10x18x9T.18x10.ET", "12")				// f
	val superlative = NamedMethod("Superlative", 12, "x30x14x5Tx36x1470x58x369Tx70x18x9Tx30xET", "12")				// b
	val worcester = NamedMethod("Worcester", 12, "x5Tx14.5Tx5T.36.14x14.5Tx14x7Tx78x7Tx78x1T", "1T")					// mx
	val yorkshire = NamedMethod("Yorkshire", 12, "x3Tx14x5Tx16x127Tx38x149Tx50x16x7Tx18xET", "12")						// b
	val zanussi = NamedMethod("Zanussi", 12, "x5Tx14.5Tx12.3T.14x12.5T.16x16.7T.58x18.9Tx18x9T", "1T")				// j2

	val calling1 = "WH BxW MWH"
	val calling2 = "W'H' BxW MW'H'"
	val calling = calling2

	val methods = List(zanussi, bristol, yorkshire)
	//val methods = List(snowtiger, barford, lincolnshire, zanussi, superlative, bristol, yorkshire)


	def generate() = tunnel(this)

	def scoreFn(comp: Composition) = musicScore(comp)

	def musicScore(comp: Composition) =
	{
		comp.methodsUsed.size*1000000 +
				-10*preferMethodIncidence(comp, zanussi, 2) +
				1*ScoreFactory.balanceScore(comp) +
				(2*comp.music(0)+4*(comp.music(1)+comp.music(2))) +
				10*comp.leadsWithoutMusic +
				-comp.longestNoComRun*comp.longestNoComRun +
				strictLenScore(comp)
	}

	def preferMethodIncidence(comp: Composition, method: NamedMethod, desiredLeads: Int) =
	{
		val nLeads = comp.methodCounts(List(method))(0)
		val diff = nLeads-desiredLeads
		diff*diff
	}

	val maxLen = 1344

	def strictLenScore(comp: Composition) =
	{
		val length = comp.length
		if (length<1250)
			(length-1260)/2
		else if (length>maxLen)
			(maxLen-length)/2
		else
			0
	}


}
