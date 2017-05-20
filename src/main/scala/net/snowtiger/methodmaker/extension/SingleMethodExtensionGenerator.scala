package net.snowtiger.methodmaker.extension

import net.snowtiger.ringing.NamedMethod

/**
 * Simple test runner, uses {@link ExtensionGenerator} to find and print all extensions for a given
 * parent method. The parent method is currently hardwired in code.
 *
 * @author MBD
 */
object SingleMethodExtensionGenerator
{
	val childwallMinor = NamedMethod("Childwall", 6, "-16-36.12.36", "12")
	val cambridgeMinor = NamedMethod("Cambridge", 6, "-36-14-12-36-14-56", "12")
	val londonMinor = NamedMethod("London", 6, "36-36.14-12-36.14-14.36", "12")
	val annablesLondonMinor = NamedMethod("Annable's London Minor", 6, "-34-14-12-36-14-36", "16")
	val bristolMajor = NamedMethod("Bristol", 8, "-58-14.58-58.36.14-14.58-14-18", "18")
	val londonMajor = NamedMethod("London", 8, "38-38.14-12-38.14-14.58.16-16.58", "12")
	val sedlescombeMinor = NamedMethod("Sedlescombe", 6, "-34-14-12-1236-14-56","12")
	val bitteswellMajor = NamedMethod("Bitteswell", 8, "-38-14-12.58.36.14-14.38.16-16.58", "12")
	val kingsditchMajor = NamedMethod("Kingsditch", 8, "-58-1456-12-1236-14-3458.12-14.58", "12")

	val nethersealeMinor = MethodLibrary.surpriseLibraries.getLibrary(6).nameToMethod("Netherseale")

	def main(args: Array[String]): Unit =
	{
		val t = System.currentTimeMillis()
		val ef = ExtensionGenerator(nethersealeMinor, MethodLibrary.surpriseLibraries)
		//val ef = ExtensionFinder(childwallMinor, plainLibraries)
		//val ef = ExtensionFinder(cambridgeMinor, surpriseLibraries)

		val series = ef.findExtensions(1)
		val dt = System.currentTimeMillis() - t;
		println("Took " + dt + "ms")

		val stats = new ExtensionStats
		val analysedMethod = ef.analyseExtensions(series, stats)
		stats.print()
	}

}