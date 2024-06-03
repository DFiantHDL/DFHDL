package dfhdl.compiler.patching
import dfhdl.core.*
import dfhdl.compiler.ir
import Patch.Add.Config as AddCfg
import scala.annotation.unchecked.uncheckedVariance

type MetaDesignAny = MetaDesign[DomainType]
abstract class MetaDesign[+D <: DomainType](
    positionMember: ir.DFMember,
    addCfg: AddCfg,
    domainType: D = DomainType.DF
)(using
    getSet: ir.MemberGetSet
) extends Design
    with reflect.Selectable:
  lazy val patch = positionMember -> Patch.Add(this, addCfg)
  lazy val injectedOwner: ir.DFOwner = addCfg match
    case AddCfg.InsideFirst | AddCfg.InsideLast =>
      positionMember match
        case positionOwner: ir.DFOwner => positionOwner
        case _ => throw new IllegalArgumentException("Expecting owner member for AddInside config.")
    case _ => positionMember.getOwner
  final override private[dfhdl] def initOwner: Design.Block =
    dfc.mutableDB.addMember(injectedOwner)
    injectedOwner.getThisOrOwnerDesign.asFE
  injectedOwner match
    case design: ir.DFDesignBlock => // do nothing
    case _ =>
      dfc.enterOwner(injectedOwner.asFE)

  dfc.mutableDB.setMetaGetSet(getSet)
  final type TDomain = D @uncheckedVariance
  final protected given TDomain = domainType
  // we don't really care about the IR domain type of a meta design, since it is removed.
  // only the compile-time domain type is important for meta-programming with the relevant constraints.
  final lazy val __domainType: ir.DomainType = ir.DomainType.DF

  final def plantMember[T <: ir.DFMember](member: T): T =
    dfc.mutableDB.plantMember(dfc.owner.asIR, member)
  final def applyBlock(owner: ir.DFOwner)(block: => Unit): Unit =
    dfc.mutableDB.OwnershipContext.enter(owner)
    block
    dfc.mutableDB.OwnershipContext.exit()
  // meta designs may be intermediate erroneous designs
  final override private[dfhdl] def skipChecks: Boolean = true

  export dfhdl.hdl.{RTDomainCfg => _, ClkCfg => _, RstCfg => _, *}
  export dfhdl.core.{asValAny, asVarAny, asDclAny, asConstAny}
  extension (dfVal: ir.DFVal)
    def cloneAnonValueAndDepsHere: ir.DFVal =
      if (dfVal.isAnonymous)
        val dfcForClone = dfc.setMeta(dfVal.meta)
        val dfType = dfVal.dfType.asFE[DFTypeAny]
        val cloned = locally {
          given DFC = dfcForClone
          dfVal match
            case const: ir.DFVal.Const =>
              DFVal.Const.forced(const.dfType.asFE[DFTypeAny], const.data)
            case func: ir.DFVal.Func =>
              val clonedArgs = func.args.map(_.get.cloneAnonValueAndDepsHere)
              DFVal.Func(func.dfType.asFE[DFTypeAny], func.op, clonedArgs)
            case alias: ir.DFVal.Alias.Partial =>
              val clonedRelValIR = alias.relValRef.get.cloneAnonValueAndDepsHere
              val clonedRelVal = clonedRelValIR.asValAny
              alias match
                case alias: ir.DFVal.Alias.AsIs =>
                  DFVal.Alias.AsIs(dfType, clonedRelVal, forceNewAlias = true)
                case alias: ir.DFVal.Alias.ApplyRange =>
                  DFVal.Alias.ApplyRange(
                    clonedRelVal.asValOf[Bits[Int]],
                    alias.relBitHigh,
                    alias.relBitLow
                  )
                case alias: ir.DFVal.Alias.ApplyIdx =>
                  val clonedIdx = alias.relIdx.get.cloneAnonValueAndDepsHere.asValOf[DFUInt[Int]]
                  DFVal.Alias.ApplyIdx(dfType, clonedRelVal, clonedIdx)
                case alias: ir.DFVal.Alias.SelectField =>
                  DFVal.Alias.SelectField(clonedRelVal, alias.fieldName)
            case _ => throw new IllegalArgumentException(s"Unsupported cloning for: $dfVal")
          end match
        }
        cloned.asIR.setTags(_ => dfVal.tags)
      else dfVal
  extension [T <: DFTypeAny, A, C, I, P](dfVal: DFVal[T, Modifier[A, C, I, P]])
    def asInitialized: DFVal[T, Modifier[A, C, Modifier.Initialized, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initialized, P]]]
    def asUninitialized: DFVal[T, Modifier[A, C, Modifier.Initializable, P]] =
      dfVal.asInstanceOf[DFVal[T, Modifier[A, C, Modifier.Initializable, P]]]
end MetaDesign
