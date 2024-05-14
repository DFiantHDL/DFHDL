package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

import scala.collection.mutable
import scala.collection.immutable.{ListSet, ListMap}
import scala.annotation.tailrec
protected trait VHDLTypePrinter extends AbstractTypePrinter:
  type TPrinter <: VHDLPrinter
  def csDFBoolOrBit(dfType: DFBoolOrBit, typeCS: Boolean): String =
    dfType match
      case DFBool => "boolean"
      case DFBit  => "std_logic"
  def csDFBits(dfType: DFBits, typeCS: Boolean): String =
    if (typeCS) "std_logic_vector"
    else s"std_logic_vector(${dfType.widthParamRef.uboundCS} downto 0)"
  def csDFDecimal(dfType: DFDecimal, typeCS: Boolean): String =
    import dfType.*
    (signed, fractionWidth) match
      case (false, 0) =>
        if (typeCS) "unsigned"
        else s"unsigned(${widthParamRef.uboundCS} downto 0)"
      case (true, 0) =>
        if (dfType.isDFInt32) "integer"
        else if (typeCS) "signed"
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
  def getVectorTypes(dcls: Iterable[DFVal]): ListMap[String, Int] =
    def flatten(dfType: DFType): List[DFVector] =
      dfType match
        case dt: DFStruct =>
          dt.fieldMap.values.flatMap(flatten).toList
        case dt: DFOpaque =>
          flatten(dt.actualType)
        case dt: DFVector =>
          dt :: flatten(dt.cellType)
        case _ => Nil
    ListMap.from(
      dcls.flatMap(dcl => flatten(dcl.dfType).reverse)
        .map(getVecDepthAndCellTypeName)
        .foldLeft(ListMap.empty[String, Int]) { case (listMap, (cellTypeName, depth)) =>
          listMap.updatedWith(cellTypeName)(prevDepthOpt =>
            Some(prevDepthOpt.getOrElse(0).max(depth))
          )
        }
    )
  end getVectorTypes
  lazy val globalVectorTypes: ListMap[String, Int] =
    getVectorTypes(
      getSet.designDB.members.view.collect {
        case port @ DclPort()                     => port
        case const @ DclConst() if const.isGlobal => const
      }
    )
  def getLocalVectorTypes(design: DFDesignBlock): ListMap[String, Int] =
    getVectorTypes(
      getSet.designDB.designMemberTable(design).view.collect {
        case localVar @ DclVar()     => localVar
        case localConst @ DclConst() => localConst
      }
    )
  @tailrec private def getVecDepthAndCellTypeName(dfType: DFVector, depth: Int): (String, Int) =
    dfType.cellType match
      case dfType: DFVector => getVecDepthAndCellTypeName(dfType, depth + 1)
      case cellType         => (csDFType(cellType, true), depth)
  def getVecDepthAndCellTypeName(dfType: DFVector): (String, Int) =
    getVecDepthAndCellTypeName(dfType, 1)

  def csDFVectorDclName(cellTypeName: String, depth: Int): String =
    s"t_vecX${depth}_${cellTypeName}"
  def csDFVectorDclName(dfType: DFVector): String =
    val (cellTypeName, depth) = getVecDepthAndCellTypeName(dfType)
    csDFVectorDclName(cellTypeName, depth)
  def csDFVectorDcl(cellTypeName: String, depth: Int): String =
    val ofTypeName = if (depth == 1) cellTypeName else csDFVectorDclName(cellTypeName, depth - 1)
    s"type ${csDFVectorDclName(cellTypeName, depth)} is array (natural range <>) of $ofTypeName;"
  def csDFVectorDcls(cellTypeName: String, depth: Int, start: Int): String =
    (for (i <- start to depth) yield csDFVectorDcl(cellTypeName, i))
      .mkString("\n")
  def csDFVectorDclsGlobal(cellTypeName: String, depth: Int): String =
    csDFVectorDcls(cellTypeName, depth, 1)
  def csDFVectorDclsLocal(cellTypeName: String, depth: Int): String =
    csDFVectorDcls(cellTypeName, depth, globalVectorTypes.getOrElse(cellTypeName, 0) + 1)
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    if (typeCS) csDFVectorDclName(dfType)
    else
      var loopType: DFType = dfType
      var desc: String = csDFVectorDclName(dfType)
      var inVector: Boolean = true
      while (inVector)
        loopType match
          case dfType: DFVector =>
            desc = desc + s"(0 to ${dfType.cellDims.head} - 1)"
            loopType = dfType.cellType
          case cellType =>
            val finale = cellType match
              case DFBits(width) => s"(${width.uboundCS} downto 0)"
              case DFUInt(width) => s"(${width.uboundCS} downto 0)"
              case DFSInt(width) => s"(${width.uboundCS} downto 0)"
              case _             => ""
            desc = desc + finale
            inVector = false
      desc
  end csDFVector
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
    s"""|function bitWidth(A: ${typeName}) return integer;
        |function to_slv(A: ${typeName}) return std_logic_vector;
        |function to_${typeName}(A: std_logic_vector) return ${typeName};""".stripMargin
  def csDFStructConvFuncsBody(dfType: DFStruct): String =
    val typeName = csDFStructTypeName(dfType)
    def bitWidthFunc(dfType: DFType, csArg: String): String = dfType match
      case DFBits(_) | DFUInt(_) | DFSInt(_) => s"$csArg'length"
      case _: DFBoolOrBit                    => "1"
      case DFVector(cellType, cellDims) =>
        s"${cellDims.head} * ${bitWidthFunc(cellType, s"$csArg(0)")}"
      case dfType: DFStruct => s"bitWidth($csArg)"
      case dfType: DFOpaque => bitWidthFunc(dfType.actualType, csArg)
      case _                => printer.unsupported
    def to_slv(fromType: DFType, csArg: String): String = fromType match
      case DFBits(_) => csArg
      case _         => s"to_slv($csArg)"
    val fieldLengths = dfType.fieldMap.map { (n, t) =>
      s"len := len + ${bitWidthFunc(t, s"A.$n")};"
    }.mkString("\n  ")
    val vecAssignments = dfType.fieldMap.map { (n, t) =>
      s"hi := lo - 1; lo := hi - ${bitWidthFunc(t, s"A.$n")} + 1; ret(hi downto lo) := ${to_slv(t, s"A.$n")};"
    }.mkString("\n  ")
    val fieldAssignments = dfType.fieldMap.map { (n, t) =>
      s"hi := lo - 1; lo := hi - ${bitWidthFunc(t, s"ret.$n")} + 1; ret.$n := ${printer.csBitsToType(t, "A(hi downto lo)")};"
    }.mkString("\n  ")
    s"""|function bitWidth(A : ${typeName}) return integer is
        |  variable len : integer;
        |begin
        |  len := 0;
        |  ${fieldLengths}
        |  return len;
        |end;
        |function to_slv(A : ${typeName}) return std_logic_vector is
        |  variable hi : integer;
        |  variable lo : integer;
        |  variable ret : std_logic_vector(bitWidth(A) - 1 downto 0);
        |begin
        |  lo := bitWidth(A);
        |  ${vecAssignments}
        |  return ret;
        |end;
        |function to_${typeName}(A : std_logic_vector) return ${typeName} is
        |  variable hi : integer;
        |  variable lo : integer;
        |  variable ret : ${typeName};
        |begin
        |  lo := bitWidth(A);
        |  ${fieldAssignments}
        |  return ret;
        |end;""".stripMargin
  end csDFStructConvFuncsBody
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VHDLTypePrinter
