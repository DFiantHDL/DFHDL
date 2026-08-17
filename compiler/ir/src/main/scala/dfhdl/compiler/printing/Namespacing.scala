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
end Namespacing
