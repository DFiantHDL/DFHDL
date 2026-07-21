package dfhdl.sim

import java.io.{ByteArrayOutputStream, OutputStream}
import java.net.URI
import javax.tools.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Codegen tier: runtime code generation — generate straight-line Java from the netlist, compile it
  * in-memory with the JDK compiler, and hot-load it as a [[SimKernel]] over the same signal array
  * as the interpreter tier.
  *
  * This is the JDK-17-portable mockup of the planned bytecode backend (ClassFile API / ASM): the
  * shape of the emitted code is identical to what the bytecode generator would produce —
  * combinational values in method locals, state reads/writes through the signal array, constants
  * folded into the instruction stream, and bodies split into sub-methods (with cross-chunk values
  * spilled to the signal array) to stay under the JIT's 8000-bytecode inlining/compile thresholds.
  */
object Codegen:
  private var counter = 0

  def compile(
      nl: Netlist,
      maxOpsPerMethod: Int = 250,
      dumpSource: Boolean = false,
      observed: Set[Int] = Set.empty
  ): SimKernel =
    val className = synchronized {
      counter += 1
      s"dfhdl.sim.gen.Kernel$counter"
    }
    val source = generateJava(nl, className, maxOpsPerMethod, observed)
    if dumpSource then println(source)
    val cls = compileJava(className, source)
    cls.getDeclaredConstructor().newInstance().asInstanceOf[SimKernel]

  private def generateJava(
      nl: Netlist,
      className: String,
      maxOpsPerMethod: Int,
      observed: Set[Int]
  ): String =
    val comb = nl.combNodeIds
    val chunks = comb.grouped(maxOpsPerMethod).toVector
    val single = chunks.sizeIs <= 1
    val chunkOf: Map[Int, Int] =
      if single then Map.empty
      else (for (c, ci) <- chunks.zipWithIndex; id <- c yield id -> ci).toMap
    // Values that must be materialized in the signal array: externally observed (peek), consumed
    // across chunk boundaries, or consumed by the register commit outside the producing method.
    val spill = mutable.Set.empty[Int]
    def isCombNode(id: Int): Boolean =
      nl.opcodes(id) != Op.REG && nl.opcodes(id) != Op.CONST
    spill ++= observed.filter(isCombNode)
    if !single then
      for id <- comb; in <- nl.nodeInputs(id) do
        if isCombNode(in) && chunkOf(in) != chunkOf(id) then spill += in
      for n <- nl.regNextIds do if isCombNode(n) then spill += n

    def hexL(v: Long): String = "0x%XL".format(v)
    // Read a node's value from within chunk `ci` (-1 = the run method, outside all chunks)
    def rd(id: Int, ci: Int): String = nl.opcodes(id) match
      case Op.CONST => hexL(nl.initVals(id))
      case Op.REG   => s"sig[$id]"
      case _        => if single || (ci >= 0 && chunkOf(id) == ci) then s"v$id" else s"sig[$id]"

    def emitOp(id: Int, ci: Int): String =
      val w = nl.widths(id)
      val m = hexL(nl.maskOf(id))
      val a = nl.inA(id)
      val b = nl.inB(id)
      val c = nl.inC(id)
      val expr = nl.opcodes(id) match
        case Op.ADD    => s"(${rd(a, ci)} + ${rd(b, ci)}) & $m"
        case Op.XOR    => s"${rd(a, ci)} ^ ${rd(b, ci)}"
        case Op.AND    => s"${rd(a, ci)} & ${rd(b, ci)}"
        case Op.OR     => s"${rd(a, ci)} | ${rd(b, ci)}"
        case Op.NOT    => s"(~${rd(a, ci)}) & $m"
        case Op.ROTR   => s"((${rd(a, ci)} >>> $b) | (${rd(a, ci)} << ${w - b})) & $m"
        case Op.SHL    => s"(${rd(a, ci)} << $b) & $m"
        case Op.SHR    => s"${rd(a, ci)} >>> $b"
        case Op.MUX    => s"(${rd(a, ci)} != 0L ? ${rd(b, ci)} : ${rd(c, ci)})"
        case Op.ROM    => s"ROM$b[(int) (${rd(a, ci)} & ${nl.romTables(b).length - 1}L)]"
        case Op.RESIZE => s"${rd(a, ci)} & $m"
        case Op.EQ     => s"(${rd(a, ci)} == ${rd(b, ci)} ? 1L : 0L)"
        case Op.NEQ    => s"(${rd(a, ci)} != ${rd(b, ci)} ? 1L : 0L)"
        case Op.MOV    => rd(a, ci)
        case Op.SUB    => s"(${rd(a, ci)} - ${rd(b, ci)}) & $m"
        case Op.SHLV   =>
          s"(${rd(b, ci)} >= 64L ? 0L : (${rd(a, ci)} << ${rd(b, ci)}) & $m)"
        case Op.SHRV =>
          s"(${rd(b, ci)} >= 64L ? 0L : ${rd(a, ci)} >>> ${rd(b, ci)})"
        case Op.SRAV =>
          val s = 64 - w
          s"((((${rd(a, ci)} << $s) >> $s) >> Math.min(${rd(b, ci)}, 63L)) & $m)"
        case Op.ULT =>
          s"(Long.compareUnsigned(${rd(a, ci)}, ${rd(b, ci)}) < 0 ? 1L : 0L)"
        case Op.SLT =>
          val s = 64 - nl.widths(a)
          s"(((${rd(a, ci)} << $s) >> $s) < ((${rd(b, ci)} << $s) >> $s) ? 1L : 0L)"
        case Op.MUL  => s"(${rd(a, ci)} * ${rd(b, ci)}) & $m"
        case Op.UDIV =>
          s"(${rd(b, ci)} == 0L ? 0L : Long.divideUnsigned(${rd(a, ci)}, ${rd(b, ci)}))"
        case Op.SDIV =>
          val s = 64 - w
          s"(((${rd(b, ci)} << $s) >> $s) == 0L ? 0L : " +
            s"(((${rd(a, ci)} << $s) >> $s) / ((${rd(b, ci)} << $s) >> $s)) & $m)"
        case Op.UREM =>
          s"(${rd(b, ci)} == 0L ? 0L : Long.remainderUnsigned(${rd(a, ci)}, ${rd(b, ci)}))"
        case Op.SREM =>
          val s = 64 - w
          s"(((${rd(b, ci)} << $s) >> $s) == 0L ? 0L : " +
            s"(((${rd(a, ci)} << $s) >> $s) % ((${rd(b, ci)} << $s) >> $s)) & $m)"
        case Op.REV => s"Long.reverse(${rd(a, ci)}) >>> ${64 - w}"
        case other  => throw new IllegalStateException(s"bad opcode $other")
      val store = if spill.contains(id) then s" sig[$id] = v$id;" else ""
      s"      long v$id = $expr;$store"
    end emitOp

    val simpleName = className.split('.').last
    val pkg = className.stripSuffix("." + simpleName)
    val sb = new StringBuilder
    sb ++= s"package $pkg;\n\n"
    sb ++= s"public final class $simpleName implements dfhdl.sim.SimKernel {\n"
    for (table, i) <- nl.romTables.zipWithIndex do
      sb ++= s"  private static final long[] ROM$i = { ${table.map(hexL).mkString(", ")} };\n"
    sb ++= "\n  public void run(long[] sig, long cycles) {\n"
    sb ++= "    for (long cyc = 0L; cyc < cycles; cyc++) {\n"
    val commitCi = if single then 0 else -1
    if single then
      for id <- comb do
        sb ++= emitOp(id, 0)
        sb += '\n'
    else for ci <- chunks.indices do sb ++= s"      chunk$ci(sig);\n"
    // two-phase commit: read every next value before any register slot is overwritten,
    // otherwise register-to-register chains (shift registers) cascade within one cycle
    for (n, i) <- nl.regNextIds.zipWithIndex do
      sb ++= s"      long nxt$i = ${rd(n, commitCi)};\n"
    for (r, i) <- nl.regIds.zipWithIndex do
      sb ++= s"      sig[$r] = nxt$i;\n"
    sb ++= "    }\n  }\n"
    // comb-only sweep (no register commit) for settle-on-peek
    sb ++= "\n  public void settle(long[] sig) {\n"
    if single then
      for id <- comb do
        sb ++= emitOp(id, 0)
        sb += '\n'
    else for ci <- chunks.indices do sb ++= s"    chunk$ci(sig);\n"
    sb ++= "  }\n"
    // one tracked-commit cycle for the scheduler's quiescence probe
    sb ++= "\n  public boolean stepDirty(long[] sig) {\n"
    val probeCi = if single then 0 else -1
    if single then
      for id <- comb do
        sb ++= emitOp(id, 0)
        sb += '\n'
    else for ci <- chunks.indices do sb ++= s"    chunk$ci(sig);\n"
    sb ++= "    boolean dirty = false;\n"
    for (n, i) <- nl.regNextIds.zipWithIndex do
      sb ++= s"    long nxt$i = ${rd(n, probeCi)};\n"
    for ((r, i) <- nl.regIds.zipWithIndex) do
      if nl.regTracked(i) then sb ++= s"    if (sig[$r] != nxt$i) dirty = true;\n"
      sb ++= s"    sig[$r] = nxt$i;\n"
    sb ++= "    return dirty;\n  }\n"
    if !single then
      for (chunk, ci) <- chunks.zipWithIndex do
        sb ++= s"\n  private static void chunk$ci(long[] sig) {\n"
        for id <- chunk do
          sb ++= emitOp(id, ci)
          sb += '\n'
        sb ++= "  }\n"
    sb ++= "}\n"
    sb.result()
  end generateJava

  private def compileJava(className: String, source: String): Class[?] =
    val compiler = ToolProvider.getSystemJavaCompiler
    require(compiler ne null, "JDK javac not available (running on a JRE?)")
    val diags = new DiagnosticCollector[JavaFileObject]
    val stdFm = compiler.getStandardFileManager(diags, null, null)
    val outputs = mutable.Map.empty[String, ByteArrayOutputStream]
    val fm = new ForwardingJavaFileManager[JavaFileManager](stdFm):
      override def getJavaFileForOutput(
          location: JavaFileManager.Location,
          name: String,
          kind: JavaFileObject.Kind,
          sibling: FileObject
      ): JavaFileObject =
        val bos = new ByteArrayOutputStream
        outputs(name) = bos
        new SimpleJavaFileObject(
          URI.create("mem:///" + name.replace('.', '/') + ".class"),
          JavaFileObject.Kind.CLASS
        ):
          override def openOutputStream(): OutputStream = bos
    val srcObj = new SimpleJavaFileObject(
      URI.create("string:///" + className.replace('.', '/') + ".java"),
      JavaFileObject.Kind.SOURCE
    ):
      override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = source
    // The generated class references the SimKernel interface — point javac at its classes dir
    val selfCp = java.nio.file.Paths
      .get(classOf[SimKernel].getProtectionDomain.getCodeSource.getLocation.toURI).toString
    val cp = selfCp + java.io.File.pathSeparator + System.getProperty("java.class.path")
    val task =
      compiler.getTask(
        null,
        fm,
        diags,
        java.util.List.of("-classpath", cp),
        null,
        java.util.List.of(srcObj)
      )
    if !task.call().booleanValue() then
      val msgs = diags.getDiagnostics.asScala.mkString("\n")
      throw new RuntimeException(s"javac failed:\n$msgs\n----- source -----\n$source")
    val loader = new ClassLoader(classOf[SimKernel].getClassLoader):
      override protected def findClass(name: String): Class[?] =
        outputs.get(name) match
          case Some(bos) =>
            val bytes = bos.toByteArray
            defineClass(name, bytes, 0, bytes.length)
          case None => throw new ClassNotFoundException(name)
    loader.loadClass(className)
  end compileJava
end Codegen
