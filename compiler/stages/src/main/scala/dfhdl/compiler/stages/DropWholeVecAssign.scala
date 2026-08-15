package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.core.DFType.asFE
import dfhdl.core.{DFTypeAny, asValAny, cloneAnonValueAndDepsHere}
import DFVal.Func.Op as FuncOp

//format: off
/** Lowers a WHOLE-vector constant drive into an element-wise one, because the pre-SystemVerilog
  * Verilog dialects have no unpacked-array assignment at all: a vector literal there is a PACKED
  * replication/concatenation (`{4{8'h00}}`), which every tool either rejects outright or, worse,
  * reads as an assignment pattern that initializes element 0 only (issue #492).
  *
  * Runs for `verilog.v2001` / `verilog.v95`, and for any backend when the `dropWholeVecAssign`
  * compiler option is set. Rule 1 is Verilog-only: VHDL has no `initial` construct, and its
  * aggregate initialization needs no lowering.
  *
  * A drive is lowered only into a declaration whose vector type actually reaches the backend (see
  * `keepsVectorType`). An anonymous COMPOSITION (`all(x)` / `x.repeat(n)`, or a `Vector(a, b, c)`
  * concatenation) is taken apart into its own operands; any other source is taken apart by
  * selecting each cell out of it, which requires it to be constant, so a plain vector-to-vector
  * drive is left alone. Only the
  * declaration's own (outermost) dimension is unrolled: a cell-level whole-vector drive is legal
  * in SystemVerilog, and under the older dialects `DropStructsVecs` has already flattened the cell
  * type to `Bits`, making it a plain bit-vector drive.
  *
  * ==Rule 1: A declaration's `init` becomes an `initial` block==
  * {{{
  * // Before
  * val mem = Bits(8) X 4 <> VAR init all(h"00")
  *
  * // After
  * val mem = Bits(8) X 4 <> VAR
  * val mem_init = initial:
  *   for (mem_i <- 0 until 4) mem(mem_i) := h"00"
  * }}}
  *
  * ==Rule 2: A connection becomes per-cell connections==
  * {{{
  * // Before
  * val con = Bits(8) X 4 <> VAR
  * con <> all(h"00")
  *
  * // After
  * val con = Bits(8) X 4 <> VAR
  * con(0) <> h"00"
  * con(1) <> h"00"
  * con(2) <> h"00"
  * con(3) <> h"00"
  * }}}
  *
  * A connection is concurrent, so it cannot be wrapped in a procedural loop: the cells are
  * unrolled even for a uniform source. Driving them from a `process(all)` instead would produce a
  * process with an EMPTY sensitivity list (the only sources lowered here are constants, so the
  * body reads nothing), which never triggers.
  *
  * ==Rule 3: An assignment is unrolled in place==
  *
  * The loop takes the assignment's own place, inside whatever process or conditional branch it
  * sits in, and keeps the assignment's operator.
  * {{{
  * // Before
  * process(clk.rising):
  *   if (rst == 1) mem :== all(h"00")
  *
  * // After
  * process(clk.rising):
  *   if (rst == 1)
  *     for (mem_i <- 0 until 4) mem(mem_i) :== h"00"
  * }}}
  *
  * ==Source shapes==
  *
  * A uniform source (`all(x)`, `x.repeat(n)`) drives every cell from one expression, so it becomes
  * a `for` loop. A per-cell source (a `Vector(a, b, c)` concatenation) unrolls into one assignment
  * per cell, since its cells differ. Any other constant source (a named constant vector, a
  * `Bits`-to-vector cast, a folded vector literal) unrolls into a cell selection per cell.
  *
  * The operands need not be constant: `vec :== all(x)` becomes `for (i) vec(i) :== x`, which reads
  * `x` exactly where the whole-vector form did. Only Rule 1 insists on a constant, since an
  * `initial` block runs once, at time zero. An unrolled drive of a non-constant ANONYMOUS
  * expression clones that expression per cell (an anonymous value may be read exactly once);
  * a named operand is simply referenced by each cell.
  */
//format: on
case object DropWholeVecAssign extends HierarchyStage:
  def dependencies: List[Stage] = List(ToED)
  def nullifies: Set[Stage] = Set(DropUnreferencedAnons)
  override def runCondition(using co: CompilerOptions): Boolean =
    co.dropWholeVecAssign ||
      (co.backend match
        case be: dfhdl.backends.verilog =>
          be.dialect match
            case VerilogDialect.v95 | VerilogDialect.v2001 => true
            case _                                         => false
        case _ => false)

  // The declaration must still be a vector by the time the backend prints it. `DropStructsVecs`
  // flattens every vector but a block-ram variable into `Bits`, and a flattened whole-vector drive
  // is a plain (legal) bit-vector drive; unrolling it first would instead leave cell selections
  // that the flattening turns into variable-bound part-selects, which the old dialects reject.
  private def keepsVectorType(dcl: DFVal.Dcl)(using MemberGetSet, CompilerOptions): Boolean =
    !DropStructsVecs.runCondition || BlockRamVar.unapply(dcl)

  // How the source supplies the declaration's cells.
  private enum VecSource:
    // one expression for every cell (`all(x)` / `x.repeat(n)`), driven by a loop over the
    // declaration's own element-count parameter, so a parametric length stays parametric
    case Uniform(elem: DFVal)
    // one expression per cell (a `Vector(a, b, c)` concatenation)
    case PerCell(elems: List[DFVal])
    // any other vector value, selected cell by cell
    case CellSelect(src: DFVal, vecLength: Int)
  // `constOnly` is set where the drive is applied once, at time zero (a declaration's `init`),
  // and a non-constant operand would therefore be read at the wrong time.
  private def vecSourceOf(src: DFVal, vecLengthOpt: Option[Int], constOnly: Boolean)(using
      MemberGetSet
  ): Option[VecSource] =
    src match
      case _ if constOnly && !src.isConst                                         => None
      case DFVal.Func(op = FuncOp.repeat, args = elemRef :: _) if src.isAnonymous =>
        Some(VecSource.Uniform(elemRef.get))
      case DFVal.Func(op = FuncOp.`++`, args = argRefs)
          if src.isAnonymous && vecLengthOpt.forall(argRefs.lengthIs == _) =>
        Some(VecSource.PerCell(argRefs.map(_.get)))
      // any other shape is taken apart by selecting each cell out of the source itself, which
      // needs the source to be CONSTANT: a named non-constant vector is a plain vector-to-vector
      // drive (left alone), and an anonymous non-constant one would be duplicated per cell
      case _ if src.isConst => vecLengthOpt.map(VecSource.CellSelect(src, _))
      case _                => None

  // The whole-vector drive candidate: the target declaration, its vector type and the decomposed
  // source.
  private def candidate(toVal: DFVal, fromVal: DFVal, constOnly: Boolean)(using
      MemberGetSet,
      CompilerOptions
  ): Option[(DFVal.Dcl, DFVector, VecSource)] =
    toVal match
      case dcl: DFVal.Dcl =>
        dcl.dfType match
          case vecType: DFVector if keepsVectorType(dcl) =>
            vecSourceOf(fromVal, vecType.lengthIntOpt, constOnly).map((dcl, vecType, _))
          case _ => None
      case _ => None

  // A cell drive `dcl(idx) <op> rhs`, emitted in the current meta-design context.
  private def cellDrive(
      dcl: DFVal.Dcl,
      cellType: DFType,
      idx: dfhdl.core.DFValOf[dfhdl.core.DFInt32],
      rhs: ir.DFVal,
      op: DFNet.Op,
      netMeta: Meta
  )(using dfc: dfhdl.core.DFC): Unit =
    import dfhdl.core.{refTW, addMember}
    val lhs = dfhdl.core.DFVal.Alias.ApplyIdx(
      cellType.asFE[DFTypeAny],
      dcl.asValAny,
      idx
    )(using dfc.anonymize).asIR
    ir.DFNet(
      lhs.refTW[ir.DFNet],
      op,
      rhs.refTW[ir.DFNet],
      dfc.ownerOrEmptyRef,
      netMeta,
      ir.DFTags.empty
    ).addMember
  end cellDrive

  // A `for` loop over the whole declaration is emitted only for a uniform source, and only where
  // a procedural loop is legal: a connection is concurrent, so its cells are unrolled instead.
  private def unrollsUniform(op: DFNet.Op): Boolean = op == DFNet.Op.Connection

  // The element-wise drive of the whole declaration, emitted in the current meta-design context:
  // a `for` loop for a uniform source, an unrolled sequence of cell drives otherwise.
  private def elemDrive(
      dcl: DFVal.Dcl,
      vecType: DFVector,
      source: VecSource,
      op: DFNet.Op,
      netMeta: Meta
  )(using dfc: dfhdl.core.DFC): Unit =
    given MemberGetSet = dfc.getSet
    def constIdx(i: Int): dfhdl.core.DFValOf[dfhdl.core.DFInt32] =
      dfhdl.core.DFConstInt32(i)(using dfc.anonymize)
    def unrolledCellDrive(elems: Int => ir.DFVal, vecLength: Int): Unit =
      (0 until vecLength).foreach(i =>
        cellDrive(dcl, vecType.cellType, constIdx(i), elems(i), op, netMeta)
      )
    source match
      case VecSource.Uniform(elem) if unrollsUniform(op) =>
        // one clone of the source per cell: an anonymous value may be read exactly once
        unrolledCellDrive(
          _ => elem.cloneAnonValueAndDepsHere(using dfc.anonymize),
          vecType.lengthUNSAFE
        )
      case VecSource.Uniform(elem) =>
        // `get` on an `IntParamRef` is a core extension; imported here so the file's other
        // `.get` uses (on `DFRef`, through `MemberGetSet`) stay unambiguous
        import dfhdl.core.get
        val iter =
          dfhdl.core.DFVal.Dcl.iterator(using dfc.setName(s"${dcl.getName}_i"))
        // the loop bound is the declaration's OWN element-count parameter, so a parametric
        // length stays parametric in the generated loop
        val vecLength = vecType.cellDimParamRefs.head.get(using dfc.anonymize)
          .cloneAnonValueAndDepsHere(using dfc.anonymize).toDFConst(using dfc.anonymize)
        val range = dfhdl.core.DFRange(using dfc.anonymize)(
          constIdx(0),
          vecLength,
          ir.DFRange.Op.Until
        )
        val forBlock = dfhdl.core.DFFor.Block(iter, range)(using dfc.anonymize)
        dfc.enterOwner(forBlock)
        cellDrive(
          dcl,
          vecType.cellType,
          iter,
          elem.cloneAnonValueAndDepsHere(using dfc.anonymize),
          op,
          netMeta
        )
        dfc.exitOwner()
      case VecSource.PerCell(elems) =>
        unrolledCellDrive(
          i => elems(i).cloneAnonValueAndDepsHere(using dfc.anonymize),
          elems.length
        )
      case VecSource.CellSelect(src, vecLength) =>
        unrolledCellDrive(
          i =>
            dfhdl.core.DFVal.Alias.ApplyIdx(
              vecType.cellType.asFE[DFTypeAny],
              // an ANONYMOUS source is read once per cell, so each cell selects out of its own
              // clone; a named one is simply referenced by all of them
              src.cloneAnonValueAndDepsHere(using dfc.anonymize).asValAny,
              constIdx(i)
            )(using dfc.anonymize).asIR,
          vecLength
        )
    end match
  end elemDrive

  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val patchList: List[(DFMember, Patch)] = subDB.members.flatMap {
      // Rule 1: a declaration's whole-vector constant `init` becomes an `initial` block.
      // Skipped under VHDL, which has no `initial` construct (`DropInitialBlocks`, which lowers
      // them, has long since run by now) and whose aggregate initialization is legal anyway.
      case dcl @ DclVar()
          if dcl.initRefList.sizeIs == 1 && !summon[CompilerOptions].backend.isVHDL =>
        candidate(dcl, dcl.initRefList.head.get, constOnly = true).map { (dcl, vecType, source) =>
          val dsn = new MetaDesign(dcl, Patch.Add.Config.After, dfhdl.core.DomainType.ED):
            val block = dfhdl.core.Process.Block.initial(using
              dfc.setName(s"${dcl.getName}_init")
            )
            dfc.enterOwner(block)
            elemDrive(dcl, vecType, source, DFNet.Op.Assignment, dcl.meta.anonymize)(using dfc)
            dfc.exitOwner()
          List(
            dcl -> Patch.Replace(
              dcl.copy(initRefList = Nil),
              Patch.Replace.Config.FullReplacement
            ),
            dsn.patch
          )
        }.getOrElse(Nil)
      // Rule 2: a whole-vector constant connection becomes per-cell connections
      // (a plain connection only: a via/lazy connection carries a link this rewrite would drop)
      case net @ DFNet.Connection(toVal, fromVal, _) if net.op == DFNet.Op.Connection =>
        candidate(toVal, fromVal, constOnly = false).filter { (_, vecType, _) =>
          // an unrolled drive needs a statically known length
          vecType.lengthIntOpt.isDefined
        }.map { (dcl, vecType, source) =>
          val dsn = new MetaDesign(net, Patch.Add.Config.Before, dfhdl.core.DomainType.ED):
            elemDrive(dcl, vecType, source, DFNet.Op.Connection, net.meta)(using dfc)
          List(dsn.patch, net -> Patch.Remove())
        }.getOrElse(Nil)
      // Rule 3: a whole-vector constant assignment is unrolled where it stands
      case net @ DFNet.Assignment(toVal, fromVal) =>
        candidate(toVal, fromVal, constOnly = false).map { (dcl, vecType, source) =>
          val dsn = new MetaDesign(net, Patch.Add.Config.Before, dfhdl.core.DomainType.ED):
            elemDrive(dcl, vecType, source, net.op, net.meta)(using dfc)
          List(dsn.patch, net -> Patch.Remove())
        }.getOrElse(Nil)
      case _ => Nil
    }
    subDB.patch(patchList)
  end transformSubDB
end DropWholeVecAssign

extension [T: HasDB](t: T)
  def dropWholeVecAssign(using CompilerOptions): DB =
    StageRunner.run(DropWholeVecAssign)(t.db)
