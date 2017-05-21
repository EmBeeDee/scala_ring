/**
 * The Scala code in this package has been designed to generate and analyse method extensions, generally in accordance
 * with the algorithm described in Central Council Decision G:
 * <a href="http://www.methods.org.uk/ccdecs.htm">http://www.methods.org.uk/ccdecs.htm</a>.
 * The generation process is described in the Javadoc for the central class {@link net.snowtiger.methodmaker.extension.ExtensionGenerator},
 * but see also classes {@link net.snowtiger.methodmaker.extension.SingleMethodExtensionGenerator} and
 * {@link net.snowtiger.methodmaker.extension.LibraryAnalyser}, which provide main() methods suitable for looking at
 * the extensions of a single parent method, or analysing an entire method library.
 * <p>
 * I use a slightly different glossary of terms to the Central Council document. Here is a definition of my terms:
 * <ul>
 *   <li>Half extension - a potential extension of either the above or below work of a method. It is necessary to
 *   marry together two half-extensions, one for the above work and one for the below, to generate a "full" extension.
 *   <li>Extension series - a series of extensions (or half-extensions), all of the same type, from the parent method
 *   up through all stages to the maximum stage we consider (normally 24 bells).</li>
 *   <li>Path element - a change from a method's half-lead, including details of its place notation, treble position
 *   and treble section number. This is the basic data structure on which extension operates. An extension consists
 *   of a number of "copied" path elements, which are identical to those in the parent method, composed with
 *   a list of "expanded" path elements, taken from the parent method but with place notation expanded and
 *   treble moved up. There is an overlap in the two, with one block of path elements - normally two treble sections -
 *   both copied and expanded, in order to generate the longer lead of the higher-stage method.</li>
 *   <li>Stage offset - specifies the number of stages which an extension increases by. A "1AB" type extension
 *   will have a stage offset of 2, but an extension such as 1ABCD can move up 4 at a time, and higher stage
 *   offsets are possible.</li>
 *   <li>"repeatFrom" value - specifies the treble section number from which we will start repeating and expanding
 *   path elements. A value of 1 with a stage offset of 2 corresponds to an "AB" extension; 2 correspond to "BC" and
 *   so on. To follow the Central Council algorithm, we currently start at 3 ("CD").</li>
 *   <li>"shiftAbove" value - specifies the minimum place above which we will shift place notations, when expanding
 *   path elements. For example, a shiftAbove value of 3 would ensure that places at lead, 2nds and 3rds are not affected
 *   by an expansion; but above that e.g. 4ths would expand to 6ths. It corresponds to the "mode" in the Central Council
 *   algorithm</li>
 * </ul>
 * <p>
 * Note the code in this package depends on classes in my Scala Ring project, in particular {@link net.snowtiger.ringing.Row},
 * {@link net.snowtiger.ringing.Perm}, {@link net.snowtiger.ringing.PN}, {@link net.snowtiger.ringing.Method} and
 * {@link net.snowtiger.ringing.NamedMethod}.
 *
 * @author MBD
 */
package net.snowtiger.methodmaker.extension;