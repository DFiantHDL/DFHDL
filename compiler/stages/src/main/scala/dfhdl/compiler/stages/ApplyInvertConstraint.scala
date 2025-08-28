package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions
import dfhdl.core.DFValAny
import dfhdl.core.DFTypeAny
import dfhdl.core.DFType.asFE
import scala.collection.mutable
import dfhdl.internals.BitVector

/** This stage applies the invertActiveState constraint by inverting the port it's attached to. It
  * creates an intermediate variable called `{portName}_inverted` and connects it to the port.
  *   - For output ports, the inverted intermediate variable is connected to the port.
  *   - For input ports, the inverted input port is connected to the intermediate variable.
  *   - All references to the port will point to the intermediate variable.
  *   - The `invertActiveState` constraint is removed from the port.
  * For example,
  * ```
  * @io(invertActiveState = true)
  * val x1 = Bit <> IN
  * val y1 = Bit <> OUT
  * y1 <> x1
  * val x2 = Bit <> IN
  * @io(invertActiveState = true)
  * val y2 = Bit <> OUT
  * y2 <> x2
  * ```
  * will be transformed to
  * ```
  * val x1 = Bit <> IN
  * val x1_inverted = Bit <> VAR
  * x1_inverted <> !x1
  * val y1 = Bit <> OUT
  * y1 <> x1_inverted
  * val x2 = Bit <> IN
  * val y2 = Bit <> OUT
  * val y2_inverted = Bit <> VAR
  * y2 <> !y2_inverted
  * y2_inverted <> x2
  * ```
  */
case object ApplyInvertConstraint extends Stage:
  override def dependencies: List[Stage] = List(ToED)
  override def nullifies: Set[Stage] = Set()
  def transform(designDB: DB)(using getSet: MemberGetSet, co: CompilerOptions): DB =
    given RefGen = RefGen.fromGetSet

    val patchList: List[(DFMember, Patch)] = designDB.members.flatMap { member =>
      member match
        case dcl: DFVal.Dcl if dcl.isPort =>
          val invertBitSet = mutable.BitSet.empty
          // Check if this port has the invertActiveState constraint
          dcl.meta.annotations.foreach {
            case constraints.IO(bitIdx = bitIdx, invertActiveState = true) =>
              bitIdx match
                case bitIdx: Int => invertBitSet += bitIdx
                case _           => invertBitSet ++= (0 until dcl.width)
            case _ =>
          }

          if (invertBitSet.nonEmpty)
            val portName = dcl.getName
            val invertedVarName = s"${portName}_inverted"
            val updatedPortAnnots = dcl.meta.annotations.map {
              case ioConstraint: constraints.IO =>
                ioConstraint.copy(invertActiveState = None)
              case other => other
            }
            val updatedPortMeta = dcl.meta.copy(annotations = updatedPortAnnots)
            val updatedPort = dcl.copy(meta = updatedPortMeta)

            // Create a MetaDesign to add the new members
            val dsn = new MetaDesign(
              dcl,
              Patch.Add.Config.ReplaceWithMemberN(1, Patch.Replace.Config.ChangeRefAndRemove)
            ):
              plantMember(updatedPort)
              // Create the inverted intermediate variable
              val inversionVar =
                dcl.dfType.asFE[DFTypeAny].<>(VAR)(using dfc.setName(invertedVarName))

              def invert(dfVal: DFValAny): DFValAny = dfVal.asIR.dfType match
                case _: DFBoolOrBit => !dfVal.asValOf[dfhdl.core.DFBit]
                case dfType: DFBits =>
                  // all bits are inverted
                  if (dfType.width == invertBitSet.size) ~dfVal.asValOf[dfhdl.core.DFBits[Int]]
                  // otherwise, we need to use a mask
                  else
                    val maskStr =
                      (for (i <- dfType.width - 1 to 0 by -1)
                        yield
                          if (invertBitSet.contains(i)) "1"
                          else "0").mkString
                    val mask = b"$maskStr"
                    dfVal.asValOf[dfhdl.core.DFBits[Int]] ^ mask
                case _ => throw new RuntimeException(
                    "Current implementation only supports constraint inversion for bits and booleans"
                  )

              // Create the inversion and connection based on port direction
              if (dcl.isPortIn)
                inversionVar.asDclAny <> invert(updatedPort.asValAny)
              else
                updatedPort.asDclAny <> invert(inversionVar)
            List(
              dsn.patch
            )
          else Nil
          end if
        case _ => Nil
    }

    designDB.patch(patchList)
  end transform
end ApplyInvertConstraint

extension [T: HasDB](t: T)
  def applyInvertConstraint(using co: CompilerOptions): DB =
    StageRunner.run(ApplyInvertConstraint)(t.db)
