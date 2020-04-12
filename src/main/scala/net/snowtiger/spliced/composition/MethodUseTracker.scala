package net.snowtiger.spliced.composition

import net.snowtiger.ringing.{NamedMethod, Row}
import net.snowtiger.spliced.tables.{Lead, Node}

/**
 * @author mark
 */

case class MethodUseTracker(val methodUse: Map[NamedMethod,Int], val methodAtw: Map[NamedMethod, ImmutableFullyCountedAtw])
		extends Addable[MethodUseTracker]
{
	def this() = this(Map(), Map())

	lazy val atwScore = methodAtw.values.map(_.atwScore).sum
	/** TODO assumes normal treble-dominated methods only - but so does ImmutableFullyCountedAtw */
	lazy val nWorkingBells = if (methodUse.isEmpty) 0 else methodUse.head._1.nbells-1
	/** TODO assumes all methods have the same number of working bells */
	lazy val isAtw = atwScore==nMethods*nWorkingBells*nWorkingBells

	def nMethods = methodUse.keys.size

	override def +(other: MethodUseTracker): MethodUseTracker =
	{
		var newMethodUse = methodUse
		for ((meth,count) <- other.methodUse)
			if (count>0)
				newMethodUse+= meth -> (methodUse.getOrElse(meth,0) + count)
		var newMethodAtw = methodAtw
		for ((meth,atw) <- other.methodAtw)
			newMethodAtw+= meth -> (methodAtw.get(meth) match
			{
				case Some(existingAtw) => existingAtw + atw
				case None => atw
			})
		MethodUseTracker(newMethodUse, newMethodAtw)
	}

	def -(other: MethodUseTracker): MethodUseTracker =
	{
		var newMethodUse = methodUse
		for ((meth,count) <- other.methodUse)
			if (count>0)
				methodUse.get(meth) match
				{
					case Some(existingCount) => {
						val newCount = existingCount-count
						// Remove method from Map if count falls to zero
						if (newCount<=0)
							newMethodUse-= meth
						else
							newMethodUse+= meth->newCount
					}
					case None => throw new RuntimeException("Subtraction from MethodUseTracker results in negative method counts")
				}
		var newMethodAtw = methodAtw
		for ((meth,atw) <- other.methodAtw)
			newMethodAtw+= meth -> (methodAtw.get(meth) match
			{
				case Some(existingAtw) => existingAtw - atw
				case None => throw new RuntimeException("Subtraction from MethodUseTracker results in negative ATW")
			})
		MethodUseTracker(newMethodUse, newMethodAtw)
	}
}

object MethodUseTracker
{
	def apply(): MethodUseTracker = new MethodUseTracker()

	def apply(nodes: List[Node]): MethodUseTracker =
	{
		var mut = MethodUseTracker()
		for (node <- nodes)
			mut+= node.mut
		mut
	}

	def apply(nodes: List[Node], excludedLeads: Set[Lead]): MethodUseTracker =
	{
		var mut = MethodUseTracker(nodes)
		for (lead <- excludedLeads)
			mut+= MethodUseTracker(lead)
		mut
	}

	def apply(lead: Lead): MethodUseTracker =
	{
		var atw = ImmutableFullyCountedAtw(lead.method, lead.startLH)
		MethodUseTracker(Map(lead.method->1), Map(lead.method->atw))
	}

	def apply(method: NamedMethod, lh: Row): MethodUseTracker =
	{
		var atw = ImmutableFullyCountedAtw(method, lh)
		MethodUseTracker(Map(method->1), Map(method->atw))
	}
}