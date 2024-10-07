package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*

import scala.collection.mutable
import scala.collection.immutable.{ListSet, ListMap}
import scala.annotation.tailrec

enum DclScope derives CanEqual:
  case TypeOnly, Pkg, PkgBody, ArchBody
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

  def csNamedDFTypeConvFuncsDcl(dfType: NamedDFType): String =
    val typeName = dfType match
      case dt: DFEnum   => csDFEnumTypeName(dt)
      case dt: DFStruct => csDFStructTypeName(dt)
      case dt: DFOpaque => csDFOpaqueTypeName(dt)
    val bitWidth = s"function bitWidth(A: ${typeName}) return integer;"
    val to_slv = s"function to_slv(A: ${typeName}) return std_logic_vector;"
    val to_typeName = s"function to_${typeName}(A: std_logic_vector) return ${typeName};"
    val bool_sel =
      s"function bool_sel(C : boolean; T : ${typeName}; F : ${typeName}) return ${typeName};"
    dfType match
      // since working on a VHDL subtype, opaques require only `to_typeName` function,
      // and `bitWidth` and `to_slv` of the actual type are applicable
      case dt: DFOpaque => to_typeName
      case _            => s"$bitWidth\n$to_slv\n$to_typeName\n$bool_sel"
  end csNamedDFTypeConvFuncsDcl
  def csNamedDFTypeConvFuncsBody(dfType: NamedDFType): String =
    dfType match
      case dt: DFEnum   => csDFEnumConvFuncsBody(dt)
      case dt: DFStruct => csDFStructConvFuncsBody(dt)
      case dt: DFOpaque => csDFOpaqueConvFuncsBody(dt)
  def csDFEnumTypeName(dfType: DFEnum): String = s"t_enum_${dfType.getName}"
  def csDFEnumDcl(dfType: DFEnum, global: Boolean): String =
    val enumName = dfType.getName
    val entries =
      dfType.entries.view
        .map((n, v) => s"${enumName}_$n")
        .mkString(", ")
        .hindent
    s"type ${csDFEnumTypeName(dfType)} is (\n$entries\n);"
  def csDFEnumConvFuncsBody(dfType: DFEnum): String =
    val enumName = dfType.getName
    val typeName = csDFEnumTypeName(dfType)
    val to_slv_cases = dfType.entries.map((e, v) => s"when ${enumName}_$e => int_val := $v;")
    val from_slv_cases = dfType.entries.map((e, v) => s"when $v => return ${enumName}_$e;")
    s"""|function bitWidth(A : ${typeName}) return integer is
        |begin
        |  return ${dfType.width};
        |end;
        |function to_slv(A : ${typeName}) return std_logic_vector is
        |  variable int_val : integer;
        |begin
        |  case A is
        |${to_slv_cases.mkString("\n").hindent(2)}
        |  end case;
        |  return resize(to_slv(int_val), ${dfType.width});
        |end;
        |function to_${typeName}(A : std_logic_vector) return ${typeName} is
        |begin
        |  case to_integer(unsigned(A)) is
        |${from_slv_cases.mkString("\n").hindent(2)}
        |    when others => 
        |      assert false report "Unknown state detected!" severity error;
        |      return ${enumName}_${dfType.entries.head._1};
        |  end case;
        |end;
        |function bool_sel(C : boolean; T : ${typeName}; F : ${typeName}) return ${typeName} is
        |begin
        |  if C then
        |    return T;
        |  else
        |    return F;
        |  end if;
        |end;""".stripMargin
  end csDFEnumConvFuncsBody
  def csDFEnum(dfType: DFEnum, typeCS: Boolean): String = csDFEnumTypeName(dfType)
  val supportUnconstrainedArrays: Boolean =
    printer.dialect match
      case VHDLDialect.v93 => false
      case _               => true
  def getVectorTypes(dcls: Iterable[DFVal]): ListMap[String, (DFVector, Int)] =
    ListMap.from(
      dcls.view.flatMap(dcl => dcl.dfType.decompose { case dt: DFVector => dt })
        .map(getVecDepthAndCellTypeName)
        .foldLeft(ListMap.empty[String, (DFVector, Int)]) {
          case (listMap, (cellTypeName, (vecType, depth))) =>
            listMap.updatedWith(cellTypeName) {
              case Some((vecType, prevDepth)) => Some((vecType, prevDepth.max(depth)))
              case None                       => Some((vecType, depth))
            }
        }
    )
  end getVectorTypes
  lazy val globalVectorTypes: ListMap[String, (DFVector, Int)] =
    getVectorTypes(
      getSet.designDB.members.view.collect {
        case port @ DclPort()                     => port
        case const @ DclConst() if const.isGlobal => const
      }
    )
  def getLocalVectorTypes(design: DFDesignBlock): ListMap[String, (DFVector, Int)] =
    getVectorTypes(
      getSet.designDB.designMemberTable(design).view.collect {
        case localVar @ DclVar()     => localVar
        case localConst @ DclConst() => localConst
      }
    ).filter { case (tpName, _) =>
      // if not supporting unconstrained arrays, then there is no `depth` dependency,
      // and therefore all the global types should be filtered out from the local types
      supportUnconstrainedArrays || !globalVectorTypes.contains(tpName)
    }
  @tailrec private def getVecDepthAndCellTypeName(
      dfType: DFVector,
      depth: Int
  ): (String, (DFVector, Int)) =
    dfType.cellType match
      case dfType: DFVector => getVecDepthAndCellTypeName(dfType, depth + 1)
      case cellType         => (csDFType(cellType, true), (dfType, depth))
  def getVecDepthAndCellTypeName(dfType: DFVector): (String, (DFVector, Int)) =
    if (supportUnconstrainedArrays) getVecDepthAndCellTypeName(dfType, 1)
    else
      val cellTypeName = dfType.cellType match
        case DFBit              => "sl"
        case DFBool             => "boolean"
        case DFBits(Int(width)) => s"slv${width}"
        case DFUInt(Int(width)) => s"unsigned${width}"
        case DFSInt(Int(width)) => s"signed${width}"
        case dt: DFOpaque       => csDFOpaqueTypeName(dt)
        case dt: DFVector       => csDFVectorDclName(dt)
        case dt: DFStruct       => csDFStructTypeName(dt)
        case _                  => printer.unsupported
      (cellTypeName, (dfType, 1))

  // Wrapper used to uniquely track parameter values and index them
  class ParamWrapper(val dfVal: DFVal):
    override def equals(that: Any): Boolean =
      that.asInstanceOf[ParamWrapper].dfVal =~ dfVal
    override def hashCode(): Int = dfVal.codeString.hashCode()
  val paramIdxCache = mutable.Map.empty[ParamWrapper, Int]
  var paramIdxLatest: Int = 0
  def csDFVectorDclName(cellTypeName: String, depth: Int): String =
    if (depth == 0) cellTypeName
    else s"t_vecX${depth}_${cellTypeName}"
  def csDFVectorDclName(dfType: DFVector): String =
    val (cellTypeName, (vecType, depth)) = getVecDepthAndCellTypeName(dfType)
    if (supportUnconstrainedArrays) csDFVectorDclName(cellTypeName, depth)
    else
      val cellDim = dfType.cellDimParamRefs.head
      // literal length vector types are named according to their length
      if (cellDim.isInt) s"t_vecX${dfType.length}_${cellTypeName}"
      // parameterized vector types are named with a unique index for
      // each unique parameter
      else
        val wrapper = ParamWrapper(cellDim.getRef.get.get)
        val paramIdx = paramIdxCache.getOrElseUpdate(
          wrapper, {
            paramIdxLatest = paramIdxLatest + 1
            paramIdxLatest
          }
        )
        s"t_vecXP${paramIdx}_${cellTypeName}"
    end if
  end csDFVectorDclName

  def csDFVectorDcl(
      dclScope: DclScope
  )(cellTypeName: String, vecTypeOrDepth: DFVector | Int): String =
    def act(vt: DFVector => String, d: Int => String): String =
      vecTypeOrDepth match
        case vecType: DFVector => vt(vecType)
        case depth: Int        => d(depth)
    val ofTypeName = act(
      vecType => csDFType(vecType.cellType),
      depth => csDFVectorDclName(cellTypeName, depth - 1)
    )
    val typeName = act(
      csDFVectorDclName(_),
      csDFVectorDclName(cellTypeName, _)
    )
    val arrRange = act(
      vecType => s"0 to ${vecType.length - 1}",
      _ => "natural range <>"
    )
    val typeDcl = s"type $typeName is array ($arrRange) of $ofTypeName;"
    val dimArgs = act(
      _ => "",
      depth => (depth to 1 by -1).map(i => s"D$i : integer").mkString("; ", "; ", "")
    )
    val cellDimArg = cellTypeName match
      case "std_logic_vector" | "unsigned" | "signed" => "; D0 : integer"
      case _                                          => ""
    val funcDcl =
      s"""|function bitWidth(A : ${typeName}) return integer;
          |function to_slv(A : ${typeName}) return std_logic_vector;
          |function to_${typeName}(A : std_logic_vector$dimArgs$cellDimArg) return ${typeName};
          |function bool_sel(C : boolean; T : ${typeName}; F : ${typeName}) return ${typeName};""".stripMargin
    val toSLV = if (ofTypeName.startsWith("std_logic_vector")) "A(i)" else "to_slv(A(i))"
    val dims = act(
      _ => "",
      depth => (depth to 1 by -1).map(i => s"(0 to D$i - 1)").mkString
    )
    val cellDim = cellDimArg.emptyOr(_ => "(D0 - 1 downto 0)")
    val argSel = "A(hi downto lo)"
    val toCellConv = act(
      vecType =>
        vecType.cellType match
          case DFBits(_) => argSel
          case DFBit     => s"to_sl($argSel)"
          case DFBool    => s"to_bool($argSel)"
          case DFUInt(_) => s"unsigned($argSel)"
          case DFSInt(_) => s"signed($argSel)"
          case _         => s"to_${cellTypeName}($argSel)",
      depth =>
        if (depth == 1)
          cellTypeName match
            case "std_logic_vector"    => argSel
            case "std_logic"           => s"to_sl($argSel)"
            case "boolean"             => s"to_bool($argSel)"
            case "unsigned" | "signed" => s"$cellTypeName($argSel)"
            case _                     => s"to_${cellTypeName}($argSel)"
        else
          val dimArgsApply = (depth - 1 to 1 by -1).map(i => s"D$i").mkString(", ", ", ", "")
          val cellDimArgApply = cellDimArg.emptyOr(_ => ", D0")
          s"to_${csDFVectorDclName(cellTypeName, depth - 1)}($argSel$dimArgsApply$cellDimArgApply)"
    )
    val funcBody =
      s"""|function bitWidth(A : ${typeName}) return integer is
          |begin
          |  return A'length * bitWidth(A(0));
          |end;
          |function to_slv(A : ${typeName}) return std_logic_vector is
          |  variable hi : integer;
          |  variable lo : integer;
          |  variable cellBitWidth: integer;
          |  variable ret : std_logic_vector(bitWidth(A) - 1 downto 0);
          |begin
          |  cellBitWidth := bitWidth(A(0));
          |  lo := bitWidth(A);
          |  for i in 0 to A'length-1 loop
          |    hi := lo - 1; lo := hi - cellBitWidth + 1;
          |    ret(hi downto lo) := $toSLV;
          |  end loop;
          |  return ret;
          |end;
          |function to_${typeName}(A : std_logic_vector$dimArgs$cellDimArg) return ${typeName} is
          |  variable hi : integer;
          |  variable lo : integer;
          |  variable cellBitWidth: integer;
          |  variable ret : ${typeName}$dims$cellDim;
          |begin
          |  cellBitWidth := bitWidth(ret(0));
          |  lo := A'length;
          |  for i in 0 to ret'length - 1 loop
          |    hi := lo - 1; lo := hi - cellBitWidth + 1;
          |    ret(i) := $toCellConv;
          |  end loop;
          |  return ret;
          |end;
          |function bool_sel(C : boolean; T : ${typeName}; F : ${typeName}) return ${typeName} is
          |begin
          |  if C then
          |    return T;
          |  else
          |    return F;
          |  end if;
          |end;""".stripMargin
    dclScope match
      case DclScope.TypeOnly => typeDcl
      case DclScope.Pkg      => s"$typeDcl\n$funcDcl"
      case DclScope.PkgBody  => funcBody
      case DclScope.ArchBody => s"$typeDcl\n$funcBody"
  end csDFVectorDcl

  def csDFVectorDcls(
      dclScope: DclScope
  )(cellTypeName: String, depth: Int, start: Int): String =
    (for (i <- start to depth) yield csDFVectorDcl(dclScope)(cellTypeName, i))
      .mkString("\n")
  def csDFVectorDclsGlobal(dclScope: DclScope)(cellTypeName: String, depth: Int): String =
    csDFVectorDcls(dclScope)(cellTypeName, depth, 1)
  def csDFVectorDclsLocal(dclScope: DclScope)(cellTypeName: String, depth: Int): String =
    csDFVectorDcls(dclScope)(
      cellTypeName,
      depth,
      globalVectorTypes.get(cellTypeName).map(_._2).getOrElse(0) + 1
    )
  def csDFVector(dfType: DFVector, typeCS: Boolean): String =
    if (typeCS) csDFVectorDclName(dfType)
    else
      var loopType: DFType = dfType
      var desc: String = csDFVectorDclName(dfType)
      // starting the adding dimension only when unconstrained arrays are supported
      var inVector: Boolean = supportUnconstrainedArrays
      // add array dimensions
      while (inVector)
        loopType match
          case dfType: DFVector =>
            desc = desc + s"(0 to ${dfType.cellDimParamRefs.head.uboundCS})"
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
  def csDFOpaqueConvFuncsBody(dfType: DFOpaque): String =
    val typeName = csDFOpaqueTypeName(dfType)
    s"""|function to_${typeName}(A : std_logic_vector) return ${typeName} is
        |begin
        |  return ${printer.csBitsToType(dfType.actualType, "A")};
        |end;""".stripMargin
  def csDFStructTypeName(dfType: DFStruct): String = s"t_struct_${dfType.getName}"
  def csDFStructDcl(dfType: DFStruct): String =
    val fields = dfType.fieldMap.view
      .map((n, t) => s"${n} : ${csDFType(t)};")
      .mkString("\n")
      .hindent(1)
    s"type ${csDFStructTypeName(dfType)} is record\n$fields\nend record;"
  def csDFStruct(dfType: DFStruct, typeCS: Boolean): String = csDFStructTypeName(dfType)
  def csDFStructConvFuncsBody(dfType: DFStruct): String =
    val typeName = csDFStructTypeName(dfType)
    def to_slv(fromType: DFType, csArg: String): String = fromType match
      case DFBits(_) => csArg
      case _         => s"to_slv($csArg)"
    val fieldLengths = dfType.fieldMap.map { (n, t) =>
      s"width := width + bitWidth(A.$n);"
    }.mkString("\n  ")
    val vecAssignments = dfType.fieldMap.map { (n, t) =>
      s"hi := lo - 1; lo := hi - bitWidth(A.$n) + 1; ret(hi downto lo) := ${to_slv(t, s"A.$n")};"
    }.mkString("\n  ")
    val fieldAssignments = dfType.fieldMap.map { (n, t) =>
      s"hi := lo - 1; lo := hi - bitWidth(ret.$n) + 1; ret.$n := ${printer.csBitsToType(t, "A(hi downto lo)")};"
    }.mkString("\n  ")
    s"""|function bitWidth(A : ${typeName}) return integer is
        |  variable width : integer;
        |begin
        |  width := 0;
        |  ${fieldLengths}
        |  return width;
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
        |  lo := A'length;
        |  ${fieldAssignments}
        |  return ret;
        |end;
        |function bool_sel(C : boolean; T : ${typeName}; F : ${typeName}) return ${typeName} is
        |begin
        |  if C then
        |    return T;
        |  else
        |    return F;
        |  end if;
        |end;""".stripMargin
  end csDFStructConvFuncsBody
  def csDFUnit(dfType: DFUnit, typeCS: Boolean): String = printer.unsupported
  def csDFTuple(fieldList: List[DFType], typeCS: Boolean): String = printer.unsupported
end VHDLTypePrinter
