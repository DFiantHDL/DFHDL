package dfhdl.plugin

import dotty.tools.dotc.*
import plugins.*
import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Decorators.*
import ast.tpd
import Types.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.control.NonFatal

/** ~~~ the compile-time half of the elaboration code digest ~~~
  *
  * The sub-design disk cache keys an entry by the code identity of the design's declaring class:
  * two runs may share an entry only if the code that produced it did not change. Computing that
  * identity at RUNTIME means walking the class's whole reference closure (thousands of classes) and
  * hashing every class file behind it, which costs far more, per design class, than elaborating the
  * design (measured: ~1.5s per class).
  *
  * The compiler already knows the answer. This phase writes, beside each top-level class it
  * compiles, a small record of that class's OWN code identity plus the top-level classes its code
  * REACHES:
  * {{{
  *   dfhdl-digest 1
  *   own <sha-256 of the class's typed tree>
  *   dep some.pkg.OtherClass
  *   dep some.pkg.Helper$
  * }}}
  * and `dfhdl.internals.CodeDigest` (the runtime half) composes a design's digest by folding `own`
  * hashes over the transitive `deps`, reading only records.
  *
  * The composition MUST stay at runtime. A digest COMPOSED at compile time would go stale exactly
  * where zinc does not recompile: change a helper's body and its dependents keep their class files,
  * so their composed digests would still describe the old helper. Folding at runtime reads each
  * class's CURRENT `own`, so a rebuilt helper invalidates every design that reaches it.
  *
  * The record is written from the typed tree BEFORE the DFHDL phases rewrite it (this phase sits
  * right after `PureCheck`), so `own` is a hash of the user's own code, position-insensitive and
  * free of the absolute source paths the meta-context phases plant later.
  */
class CodeDigestPhase(setting: Setting) extends CommonPhase:
  import tpd.*

  val phaseName = "CodeDigest"

  // the digested tree is the user's typed code: after the purity analysis (which is part of a
  // design's meaning) and before the DFHDL rewrites
  override val runsAfter = Set("PureCheck")
  override val runsBefore = Set("MetaContextPlacer")

  // the record format, shared by hand with the reader (`dfhdl.internals.CodeDigest`): the plugin
  // runs inside the compiler's own class loader and cannot call into the DFHDL library
  private val recordExt = ".dfdigest"
  private val formatHeader = "dfhdl-digest 1"

  // classes whose code identity is not tracked: the platform and the Scala library are pinned by
  // the toolchain, not by the build output, and they dominate any reference closure
  private val untrackedPrefixes = List("java.", "javax.", "jdk.", "sun.", "scala.")

  /** The `own` code-identity hash of a top-level class's typed tree.
    *
    * This used to be `sha256(pluginStamp + "\n" + tree.show)`. `tree.show`
    * pretty-prints the ENTIRE typed tree to a string, which for DFHDL code means
    * materializing the giant inferred dependent types (`ExactOp2Aux[...]#Out`,
    * Check condition types, etc.) as text on every top-level class. Profiling
    * showed this single call dominated the CodeDigest phase (~17 s of a ~260 s
    * compile of the compiler_stages tests).
    *
    * Instead we fold the tree's code identity directly into the digest by
    * traversing it once: node kind + referenced symbol full-names + literal
    * constants + the identity-bearing parts of each carried type (named-type
    * symbols, constants, refinement names). This captures the same
    * meaning-affecting information as the source rendering while never building
    * the multi-kilobyte type strings. It is position-insensitive (no spans are
    * read) and source-path-free, exactly as the old rendering was.
    */
  private def ownHash(td: TypeDef)(using Context): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val sep = Array[Byte](0)
    def upd(s: String): Unit =
      digest.update(s.getBytes(StandardCharsets.UTF_8)); digest.update(sep)
    upd(pluginStamp)
    def hashType(tpe: Type): Unit =
      tpe.foreachPart { part =>
        // every part folds its structural kind, so `A & B`, `A | B` and `(A, B)`
        // (which share the same leaf symbols) can never collide
        upd(part.getClass.getSimpleName)
        part match
          case tp: NamedType    => upd(tp.symbol.fullName.mangledString)
          case tp: ConstantType => upd(tp.value.toString)
          case tp: RefinedType  => upd(tp.refinedName.mangledString)
          case _                => // structural parts are still recursed into by foreachPart
      }
    val traverser = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        tree match
          case tt: TypeTree => upd("T"); hashType(tt.tpe)
          case lit: Literal => upd("L"); upd(lit.const.value.toString)
          case _ =>
            upd(tree.getClass.getSimpleName)
            val sym = tree.symbol
            if (sym.exists) upd(sym.fullName.mangledString)
        traverseChildren(tree)
    try traverser.traverse(td)
    // an un-walkable (cyclic/erroneous) tree still yields a stable-per-build
    // hash of whatever was folded before the failure; correctness of caching is
    // unaffected because such a class simply gets a conservative identity
    catch case NonFatal(_) => ()
    digest.digest().map("%02x".format(_)).mkString
  end ownHash

  /** The identity of the PLUGIN itself, folded into every `own` hash it writes.
    *
    * The plugin is what a design's code MEANS: the same source, compiled by a different plugin,
    * elaborates differently. It is invisible to the record's dependency closure, though, because it
    * is a compile-time artifact and not a class the design reaches (a plugin change need not
    * recompile a single class of the DFHDL runtime). Without this, a cached elaboration produced by
    * an older plugin stays "valid" and is adopted by a run whose plugin no longer agrees with it.
    *
    * A plugin change does force every plugin-compiled source to recompile (the build passes the
    * plugin jar's timestamp as a scalac option for exactly that reason), so stamping the records as
    * they are rewritten is enough to retire every entry keyed on the old plugin.
    */
  private lazy val pluginStamp: String =
    try
      val location = java.nio.file.Paths.get(
        getClass.getProtectionDomain.getCodeSource.getLocation.toURI
      )
      // By CONTENT, not by file stamp: this build republishes the plugin jar under a new
      // (timestamped) name on every sbt session, so a path/mtime stamp would retire every cached
      // design once a session, which is the whole cache. The manifest is skipped for the same
      // reason (it carries the build version); the class files are what the plugin IS.
      val jar = new java.util.jar.JarFile(location.toFile)
      try
        val entries = jar.entries().asScala.toList
          .filter(e => !e.isDirectory && e.getName.endsWith(".class"))
          .sortBy(_.getName)
        val digest = MessageDigest.getInstance("SHA-256")
        entries.foreach { e =>
          digest.update(e.getName.getBytes(StandardCharsets.UTF_8))
          val in = jar.getInputStream(e)
          try digest.update(in.readAllBytes())
          finally in.close()
        }
        digest.digest().map("%02x".format(_)).mkString
      finally jar.close()
    // an unplaceable plugin (or one running from a directory) cannot be stamped this way; the
    // records then key on the code alone
    catch case NonFatal(_) => "<unstamped>"

  /** The JVM binary name of a TOP-LEVEL class (`pkg.Foo`, `pkg.Foo$` for a module,
    * `pkg.Foo$package$` for a file's top-level definitions), which is how the runtime knows it
    * (`Class.getName`) and where its class file (and so its record) sits.
    */
  private def binaryNameOf(cls: Symbol)(using Context): String = cls.fullName.mangledString

  private def isTopLevelClass(sym: Symbol)(using Context): Boolean =
    sym.isClass && sym.owner.is(Package)

  private def isTracked(binaryName: String): Boolean =
    !untrackedPrefixes.exists(binaryName.startsWith)

  /** Every top-level class the class's typed code reaches: the owning top-level class of each
    * symbol the trees refer to and of each part of the types they carry. The plugin sees TYPED
    * trees, so this is what the code actually reaches, not every class the bytecode happens to
    * mention.
    */
  private def depsOf(td: TypeDef, self: String)(using Context): List[String] =
    val deps = mutable.TreeSet.empty[String]
    def addSym(sym: Symbol): Unit =
      if (sym.exists && !sym.is(Package))
        val top = sym.topLevelClass
        if (top.exists && !top.is(Package))
          val name = binaryNameOf(top)
          if (name != self && isTracked(name)) deps += name
    def addType(tpe: Type): Unit =
      tpe.foreachPart {
        case tp: NamedType => addSym(tp.symbol)
        case _             => // structural parts carry no class identity of their own
      }
    val traverser = new TreeTraverser:
      def traverse(tree: Tree)(using Context): Unit =
        tree match
          case tt: TypeTree => addType(tt.tpe)
          case _            => addSym(tree.symbol)
        traverseChildren(tree)
    try traverser.traverse(td)
    // a type that cannot be walked (cyclic/erroneous) costs precision, not correctness: the
    // classes it would have named stay out of the closure
    catch case NonFatal(_) => ()
    deps.toList
  end depsOf

  private def outputDirOf(using Context): Option[Path] =
    // a virtual or jar output has no place to put records beside the class files: the class is
    // then simply not digestible, and the design falls back to a live elaboration
    Option(ctx.settings.outputDir.value.jpath)

  // synthetic top-level classes are NOT skipped: a file's top-level definitions live in exactly
  // one of them (the `<file>$package` object), and a design def declared there anchors its cache
  // entries on it
  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    val sym = tree.symbol
    if (isTopLevelClass(sym))
      val name = binaryNameOf(sym)
      if (isTracked(name))
        outputDirOf.foreach { outDir =>
          try
            val record =
              (s"$formatHeader" :: s"own ${ownHash(tree)}" ::
                depsOf(tree, name).map(dep => s"dep $dep")).mkString("", "\n", "\n")
            val file = outDir.resolve(name.replace('.', '/') + recordExt)
            Files.createDirectories(file.getParent)
            Files.write(file, record.getBytes(StandardCharsets.UTF_8))
          // a record that cannot be written is a design that cannot be disk-cached, not an error
          catch case NonFatal(_) => ()
        }
    tree
  end transformTypeDef
end CodeDigestPhase
