package net.snowtiger.ringing

/**
 * @author mark
 */

class NamedMethod(val name: String, val abbrev: String, method: Method) extends Method(method) with MethodAnalysis with FCHCalculator with Ordered[NamedMethod]
{
	/** Map entry: abbrev->NamedMethod */
	def me = abbrev->this

	override def toString = abbrev

	/** E.g. "Lessness (E)" instead of "E Lessness" */
	def description =
	{
		if (name.length>2 && name(1)==' ')
			name.substring(2)+" ("+name(0)+")"
		else
			name
	}

	def nameAndAbbrev = name+" ("+abbrev+")"
	def namePlusClass = name+" "*(shortClassification.size.min(1))+shortClassification
	def stage = firstLeadHead.stage

	/** Hmm - in conflict with equals and hashcode, which are based on place notation */
	def compare(that: NamedMethod) = toString.compare(that.toString)

	def toXML = <NamedMethod>
		<Name>{name}</Name>
		<Class>{longClassification}</Class>
		<ShortClass>{shortClassification}</ShortClass>
		{if (abbrev!="" && abbrev!=name) <Abbreviation>{abbrev}</Abbreviation>}
		<NBells>{nbells}</NBells>
		<Stage>{stage}</Stage>
		<FullName>{name} {shortClassification} {stage}</FullName>
		<LeadHead>{firstLeadHead}</LeadHead>
		<LHGroup>{lhGroup}</LHGroup>
		<PN>{outputPN()}</PN>
	</NamedMethod>
}

object NamedMethod
{
	/** Asymmetric methods: must supply full PN. Abbrev assumed to be first char of name */
	def apply(name: String, nbells: Int, pn: Seq[PN]): NamedMethod =
	{
		new NamedMethod(name, name.substring(0, 1), new Method(nbells, pn))
	}

	/** Asymmetric methods: must supply full PN. Abbrev assumed to be first char of name */
	def apply(name: String, nbells: Int, pn: String): NamedMethod =
	{
		new NamedMethod(name, name.substring(0, 1), new Method(nbells, pn))
	}

	/** Symmetric methods: supply halflead PN plus leadhead. Abbrev assumed to be first char of name */
	def apply(name: String, nbells: Int, pn: String, leadhead: String): NamedMethod =
	{
		new NamedMethod(name, name.substring(0, 1), new Method(nbells, pn, leadhead))
	}

	/** Symmetric methods: supply halflead PN plus leadhead. Abbrev assumed to be first char of name */
	def apply(name: String, nbells: Int, pn: Seq[PN], leadhead: PN): NamedMethod =
	{
		new NamedMethod(name, name.substring(0, 1), new Method(nbells, pn, leadhead))
	}

	def main(args: Array[String]): Unit =
	{
		val m = NamedMethod("Test", 10, "-30-14-12.70.36-14.70.58.16-16.50.18-18.70", "12")
		println(m.lhGroup)
	}
}
