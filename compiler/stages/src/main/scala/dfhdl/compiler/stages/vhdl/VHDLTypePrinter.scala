package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

import scala.collection.mutable
import scala.collection.immutable.{ListSet, ListMap}
protected trait VHDLTypePrinter extends AbstractTypePrinter:
  type TPrinter <: VHDLPrinter
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String = dfType match
    case DFBool => "boolean"
    case DFBit  => "std_logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    s"std_logic_vector(${dfType.widthParamRef.uboundCS} downto 0)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) => s"unsigned(${widthParamRef.uboundCS} downto 0)"
      case (true, 0) =>
        if (dfType.isDFInt32) "integer"
        else s"signed(${widthParamRef.uboundCS} downto 0)"
      case (false, _) => ???
      case (true, _)  => ???

  def csDFEnumTypeName(dfType: DFEnum): String = s"t_enum_${dfType.getName}"
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) => s"${enumName}_$n")
        .mkString(", ")
        .hindent
    s"type ${csDFEnumTypeName(dfType)} is (\n$entries\n);"
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = csDFEnumTypeName(dfType)
  private lazy val vectorCellTypes: ListMap[DFType, Option[DFDesignBlock]] =
    def flatten(dfType: DFType): ListSet[DFType] =
      dfType match
        case dt: DFStruct =>
          ListSet.from(dt.fieldMap.values.flatMap(flatten))
        case dt: DFOpaque =>
          flatten(dt.actualType)
        case dt: DFVector =>
          flatten(dt.cellType) + dt.cellType
        case _ => ListSet.empty
    getSet.designDB.members.foldLeft(ListMap.empty[DFType, Option[DFDesignBlock]]) {
      case (vectorCellTypeMap, dfVal: DFVal) =>
        val dfTypes = flatten(dfVal.dfType)
        if (dfTypes.isEmpty) vectorCellTypeMap
        else if (dfVal.isPort)
          vectorCellTypeMap ++ dfTypes.map(t => (t -> None)) // IO means a global vector type
        else
          dfTypes.foldLeft(vectorCellTypeMap) { case (vectorCellTypeMap, dfType) =>
            vectorCellTypeMap.get(dfType) match
              case Some(Some(owner)) => // named type already found
                if (owner == dfVal.getOwnerDesign)
                  vectorCellTypeMap // same design block -> nothing to do
                else
                  vectorCellTypeMap + (dfType -> None) // used in more than one block -> global named type
              case Some(None) => vectorCellTypeMap // known to be a global type
              // found new named type
              case None =>
                // if referenced by a global member -> global named type
                if (dfVal.isGlobal)
                  vectorCellTypeMap + (dfType -> None)
                else
                  vectorCellTypeMap + (dfType -> Some(dfVal.getOwnerDesign))
          }
      case (vectorCellTypeMap, _) => vectorCellTypeMap // not a value
    }
  end vectorCellTypes
  private val vectorTypeIdx = mutable.Map.empty[DFVector, Int]
  def csDFVectorDclName(dfType: DFVector): String =
    s"t_array_elem${vectorTypeIdx(dfType)}"
  def csDFVectorDcl(dfType: DFVector): String =
    s"type ${csDFVectorDclName(dfType)} is array (natural range <>) of ${csDFType(dfType.cellType, false)}"
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    import dfType.*
    val idx = vectorTypeIdx.get(dfType) match
      case Some(idx) => idx
      case _ =>
        val idx = vectorTypeIdx.size
        vectorTypeIdx += dfType -> idx
        idx
    s"${csDFVectorDclName(dfType)}(0 to ${cellDims.head} - 1)"
  def csDFOpaqueTypeName(dfType: DFOpaque): String = s"t_opaque_${dfType.getName}"
  def csDFOpaqueDcl(dfType: DFOpaque): String =
    s"subtype ${csDFOpaqueTypeName(dfType)} is ${csDFType(dfType.actualType)};"
  def csDFOpaque(dfType: DFOpaque, typeCS: Boolean): String = csDFOpaqueTypeName(dfType)
  def csDFStructTypeName(dfType: DFStruct): String = s"t_struct_${dfType.getName}"
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n} : ${csDFType(t)};")
      .mkString("\n")
      .hindent(1)
    s"type ${csDFStructTypeName(dfType)} is record\n$fields\nend record;"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String = csDFStructTypeName(dfType)
  def csDFStructConvFuncsDcl(dfType: DFStruct): String =
    val typeName = csDFStructTypeName(dfType)
    s"""|function get_length(A: ${typeName}) return integer;
        |function to_slv(A: ${typeName}) return std_logic_vector;
        |function to_${typeName}(A: std_logic_vector) return ${typeName};""".stripMargin
  def csDFStructConvFuncsBody(dfType: DFStruct): String =
    val typeName = csDFStructTypeName(dfType)
    def getLengthFunc(dfType: DFType, csArg: String): String = dfType match
      case DFBits(_) | DFUInt(_) | DFSInt(_) => s"$csArg'length"
      case _: DFBoolOrBit                    => "1"
      case DFVector(cellType, cellDims) =>
        s"${cellDims.head} * ${getLengthFunc(cellType, s"$csArg(0)")}"
      case dfType: DFStruct => s"get_length($csArg)"
      case dfType: DFOpaque => getLengthFunc(dfType.actualType, csArg)
      case _                => printer.unsupported
    def to_slv(fromType: DFType, csArg: String): String = fromType match
      case DFBits(_) => csArg
      case _         => s"to_slv($csArg)"
    val fieldLengths = dfType.fieldMap.map { (n, t) =>
      s"len := len + ${getLengthFunc(t, s"A.$n")};"
    }.mkString("\n  ")
    val vecAssignments = dfType.fieldMap.map { (n, t) =>
      s"lo := hi + 1; hi := lo + ${getLengthFunc(t, s"A.$n")} - 1; ret(hi downto lo) := ${to_slv(t, s"A.$n")};"
    }.mkString("\n  ")
    val fieldAssignments = dfType.fieldMap.map { (n, t) =>
      s"lo := hi + 1; hi := lo + ${getLengthFunc(t, s"ret.$n")} - 1; ret.$n := ${printer.csBitsToType(t, "A(hi downto lo)")};"
    }.mkString("\n  ")
    s"""|function get_length(A: ${typeName}) return integer is
        |  variable len : integer;
        |begin
        |  len := 0;
        |  ${fieldLengths}
        |  return len;
        |end;
        |function to_slv(A: ${typeName}) return std_logic_vector is
        |  variable hi : integer;
        |  variable lo : integer;
        |  variable ret : std_logic_vector(get_length(A) - 1 downto 0);
        |begin
        |  hi := -1;
        |  ${vecAssignments}
        |  return ret;
        |end;
        |function to_${typeName}(A: std_logic_vector) return ${typeName} is
        |  variable hi : integer;
        |  variable lo : integer;
        |  variable ret : ${typeName};
        |begin
        |  hi := -1;
        |  ${fieldAssignments}
        |  return ret;
        |end;""".stripMargin
  end csDFStructConvFuncsBody
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VHDLTypePrinter
