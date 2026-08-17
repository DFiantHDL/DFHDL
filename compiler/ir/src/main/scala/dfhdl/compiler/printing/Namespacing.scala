package dfhdl.compiler.printing
import dfhdl.compiler.ir.*

/** The namespace placement rules of the packages feature: where a named type / global constant /
  * global static function lands, relative to the TOP design's namespace.
  *
  * A declaration whose namespace equals the top's, or is an ancestor package of it (the root
  * namespace "" included), stays in the general global defs file, so designs under a dedicated
  * package do not reference a separate package for types declared alongside or above them. Anything
  * else is emitted into a package of its own.
  *
  * The emitted package name is the declaration's namespace RELATIVE to the top design's namespace:
  * the longest common package prefix is dropped and the remaining segments are joined with `_`
  * (e.g. top `veer` with types in `veer.veer_types` -> `veer_types`; types in
  * `dfhdl.lib.crypto.aes` under an unrelated top -> `dfhdl_lib_crypto_aes`). Distinct namespaces
  * map to distinct names, except a `top.x` vs root-level `x` clash, which the emission must detect
  * and reject.
  */
object Namespacing:
  /** Does `ns` belong in the general global defs file under a top design of `topNs`? */
  def isGlobalPlaced(ns: String, topNs: String): Boolean =
    ns.isEmpty || ns == topNs || topNs.startsWith(s"$ns.")

  /** The emitted package name for a non-global-placed `ns` under a top of `topNs`. */
  def packageNameOf(ns: String, topNs: String): String =
    val nsParts = ns.split('.')
    val topParts = topNs.split('.')
    val common = nsParts.lazyZip(topParts).takeWhile(_ == _).size
    nsParts.drop(common).mkString("_")

  /** Placement of one namespace: `None` for the global defs file, `Some(packageName)` for a
    * dedicated package.
    */
  def placementOf(ns: String, topNs: String): Option[String] =
    if (isGlobalPlaced(ns, topNs)) None
    else Some(packageNameOf(ns, topNs))

  /** Placement of a named type. Shared by the printers (`Printer.typePlacementOf`, which first
    * checks that the backend has packages at all) and by the `DropPackages` stage, so the magnet
    * exclusion below cannot drift between them.
    */
  def typePlacementOf(dfType: NamedDFType, topNs: String): Option[String] =
    dfType match
      // Clk/Rst/Magnet opaques are language-level (the DFHDL printer shows them as builtins and
      // the backends drop them), so they are never packaged even though their declaring namespace
      // is a DFHDL-internal one
      case t: DFOpaque if t.isMagnet => None
      case _                         => placementOf(dfType.meta.namespace, topNs)

  /** The name a packaged declaration takes when the backend has no packages and everything
    * collapses into the general global defs file: its package name and its own name, joined with
    * `_` (`typespkg1` + `PkgEnum` -> `typespkg1_PkgEnum`). This mirrors what a qualified reference
    * shows in a package-bearing backend (`typespkg1::PkgEnum`), so the same declaration is
    * recognizable across dialects. Applied by the `DropPackages` stage.
    */
  def flattenedNameOf(pkgName: String, name: String): String = s"${pkgName}_$name"
end Namespacing
