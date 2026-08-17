package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DFType.asFE
import dfhdl.compiler.stages.vhdl.VHDLDialect
import dfhdl.compiler.stages.verilog.VerilogDialect
import dfhdl.compiler.ir.ProcessBlock.Sensitivity
import scala.collection.immutable.ListSet
import dfhdl.compiler.ir.DFConditional.DFMatchHeader
import dfhdl.compiler.ir.DFConditional.DFCaseBlock
import dfhdl.compiler.ir.DFConditional.DFIfElseBlock
import scala.collection.mutable

/** This stage drops process(all) by transforming it to a process with explicit sensitivity list
  */
case object DropProcessAll extends HierarchyStage:
  override def dependencies: List[Stage] = List(ToED, DropLocalDcls)
  override def nullifies: Set[Stage] = Set()
  override def runCondition(using co: CompilerOptions): Boolean =
    // the very old dialects do not support a "wildcard" all sensitivity list at all;
    // newer VHDL dialects still need this stage for processes calling phantom-carrying
    // ED methods (see below)
    co.backend match
      case be: dfhdl.backends.vhdl    => true
      case be: dfhdl.backends.verilog =>
        be.dialect match
          case VerilogDialect.v95 => true
          case _                  => false
  def transformSubDB(rootDB: DB)(using MemberGetSet, CompilerOptions, RefGen): DB =
    val co = summon[CompilerOptions]
    // under VHDL-2008/2019, `process(all)` is sensitive only to signals read within the
    // process TEXT — signals read inside a called impure function's body (ED method
    // phantom captures) are missed. Such processes get an explicit list instead.
    // (SystemVerilog is unaffected: `always_comb` is sensitive to function contents.)
    val dropAllProcesses = co.backend match
      case be: dfhdl.backends.vhdl =>
        be.dialect match
          case VHDLDialect.v93 => true
          case _               => false
      case _ => true // verilog v95 (the only verilog dialect passing runCondition)
    // see `sensitivityItems`: only Verilog cannot name an array in an event control
    val expandArrayItems = co.backend.isVerilog
    // A CONSTANT-index cell selection of an array declaration. It is exactly what the process is
    // sensitive to, so it is preferred over the array it selects from: precise (an array read at
    // one fixed index does not sensitize the process to the other cells), and directly nameable
    // in a Verilog event control, which the array itself is not. VHDL can name the array, and
    // does, so this only narrows the Verilog lists.
    object ConstArrayCell:
      def unapply(dfVal: DFVal)(using MemberGetSet): Option[DFVal.Dcl] =
        if (!expandArrayItems) None
        else
          dfVal match
            case applyIdx: DFVal.Alias.ApplyIdx if applyIdx.relIdx.get.isConst =>
              applyIdx.relValRef.get match
                case dcl: DFVal.Dcl if dcl.dfType.isInstanceOf[DFVector] => Some(dcl)
                case _                                                   => None
            case _ => None
    def hasPhantomCall(pb: ProcessBlock): Boolean =
      pb.members(MemberView.Flattened).exists {
        case DFVal.Func.Call(_, key) =>
          getSet.designDB.designHasPhantoms(key.getDesignBlock)
        case _ => false
      }
    val patchList: List[(DFMember, Patch)] =
      subDB.members
        // patching all process(all) blocks
        .collect {
          case pb @ ProcessBlock(sensitivity = Sensitivity.All)
              if dropAllProcesses || hasPhantomCall(pb) =>
            // recursively through value dependents. A constant-index array cell selection is
            // where the walk STOPS: the cell alone is what the process is sensitive to, and
            // continuing would pull in the whole array (see `ConstArrayCell`).
            def getDFValDependents(dfVal: DFVal): collection.View[DFVal] =
              dfVal match
                case ConstArrayCell(_) => collection.View(dfVal)
                case _                 =>
                  dfVal.getRefs.view.filterNot(_.isInstanceOf[DFRef.TypeRef]).map(_.get).collect {
                    case dfVal: DFVal => dfVal
                  }.flatMap(getDFValDependents).++(Some(dfVal))
            // recursively through internal conditional block members
            def getBlockDependents(block: DFBlock): collection.View[DFVal] =
              val members = subDB.blockMemberTable(block)
              members.view.flatMap {
                case DFNet.Assignment(_, fromVal) => Some(fromVal)
                // a procedural (Unit-return) method call statement reads its args
                // (explicit and phantom-captured alike); value-returning calls are reached
                // through the assignments that consume them
                case DFVal.Func.Call(call, _) if call.dfType == DFUnit =>
                  call.args.view.map(_.get)
                case mh: DFMatchHeader => Some(mh.selectorRef.get)
                // a text output reads its assertion guard and every message argument
                case textOut: TextOut =>
                  textOut.getRefs.view.map(_.get).collect { case dfVal: DFVal => dfVal }
                case cb: DFConditional.Block => getBlockDependents(cb) ++ cb.getGuardOption
                // a loop body's statements are the process's statements too, so what they read
                // belongs in the sensitivity list — as does what decides how often the loop
                // runs (a `while` guard, a `for` range)
                case wb: DFLoop.DFWhileBlock =>
                  getBlockDependents(wb) ++ Some(wb.guardRef.get)
                case fb: DFLoop.DFForBlock =>
                  val range = fb.rangeRef.get
                  getBlockDependents(fb) ++
                    List(range.startRef.get, range.endRef.get, range.stepRef.get)
                case _ => None
              }.flatMap(getDFValDependents)
            end getBlockDependents
            // memoization of added port-by-name
            val addedCPs = mutable.Set.empty[ConnectPoint]
            // Each sensitivity item, paired with the declaration it ultimately reads. They differ
            // only for an array cell selection, where the item is the cell and the declaration is
            // the array: the filters below are about WHERE that declaration lives, never about
            // where the selection expression itself sits.
            val dcls =
              ListSet.from(getBlockDependents(pb).flatMap {
                case cell @ ConstArrayCell(arrayDcl) => Some(cell -> (arrayDcl: DFVal))
                case dfVal => dfVal.departialPBNS.map(root => (root._1: DFVal) -> (root._1: DFVal))
              })
                // filter out local variables, but keep port-by-name which may be inside the process,
                // but refer to vias outside of it. we also need to account that different PBNS are
                // considered to be different values, so we use `addedCPs` to only add one port-by-name per connect point.
                .view.filter { (_, root) =>
                  root match
                    // HDL method call ports are not signals — the call's actual reads are
                    // collected through the call's input connections instead
                    case pbns: DFVal.PortByNameSelect
                        if pbns.getDesignInst.getDesignBlock.isHDLMethod =>
                      false
                    case pbns: DFVal.PortByNameSelect =>
                      val cp = ConnectPoint.Via(pbns)
                      if (addedCPs.contains(cp)) false
                      else
                        addedCPs += cp
                        true
                    case v => !v.isInsideOwner(pb)
                }.map(_._1).toList
            val dsn = new MetaDesign(
              pb,
              Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.FullReplacement)
            ):
              // An array read at a NON-constant index (or read whole) leaves the array itself as
              // the item, and a Verilog event control cannot name one: it takes expressions, and
              // an array name is not one — with no `@*` in v95 to fall back on. Such an item is
              // listed cell by cell, `@(mem[0] or mem[1] or ...)`, the form synthesis has always
              // required, and sensitizing to every cell is right whatever index is read. A VHDL
              // sensitivity list takes the array signal itself, so it is left whole there.
              //
              // The cells are never arrays in turn: every dialect needing this stage has had
              // `DropStructsVecs` flatten the cell type to `Bits` already.
              def sensitivityItems(dcl: DFVal.Dcl): List[dfhdl.core.DFValAny] =
                dcl.dfType match
                  case vecType: DFVector if expandArrayItems =>
                    vecType.lengthIntOpt match
                      case Some(vecLength) =>
                        List.tabulate(vecLength) { i =>
                          dfhdl.core.DFVal.Alias.ApplyIdx(
                            vecType.cellType.asFE[dfhdl.core.DFTypeAny],
                            dcl.asValAny,
                            dfhdl.core.DFConstInt32(i)(using dfc.anonymize)
                          )(using dfc.anonymize)
                        }
                      case None => List(dcl.asValAny)
                  case _ => List(dcl.asValAny)
              // only a NAMED declaration expands: each cell selection references it again, and
              // an anonymous value may be read exactly once
              val updatedDcls = dcls.flatMap {
                case dcl: DFVal.Dcl => sensitivityItems(dcl)
                case other          => List(other.cloneAnonValueAndDepsHere.asValAny)
              }
              dfhdl.core.Process.Block.list(updatedDcls)(using dfc.setMeta(pb.meta))

            dsn.patch
        }
    subDB.patch(patchList)
  end transformSubDB
end DropProcessAll

extension [T: HasDB](t: T)
  def dropProcessAll(using co: CompilerOptions): DB =
    StageRunner.run(DropProcessAll)(t.db)
