package dfhdl.compiler.stages.vhdl
import dfhdl.compiler.printing.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.analysis.*
import dfhdl.internals.*
import dfhdl.options.PrinterOptions
import scala.collection.mutable
import scala.collection.immutable.ListSet
class VHDLPrinter(using val getSet: MemberGetSet, val printerOptions: PrinterOptions)
    extends Printer,
      VHDLTypePrinter,
      VHDLDataPrinter,
      VHDLValPrinter,
      VHDLOwnerPrinter:
  type TPrinter = VHDLPrinter
  given printer: TPrinter = this
  def unsupported: Nothing = throw new IllegalArgumentException(
    "Unsupported member for this RTPrinter."
  )
  val tupleSupportEnable: Boolean = false
  def csViaConnectionSep: String = ","
  def csAssignment(lhsStr: String, rhsStr: String, shared: Boolean): String =
    s"$lhsStr := $rhsStr;"
  def csNBAssignment(lhsStr: String, rhsStr: String): String =
    s"$lhsStr <= $rhsStr;"
  def csConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr <= $rhsStr;"
  def csViaConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    s"$lhsStr => $rhsStr"
  def csLazyConnection(lhsStr: String, rhsStr: String, directionStr: String): String =
    unsupported
  final val normalizeViaConnection: Boolean = true
  final val normalizeConnection: Boolean = true
  def csOpenKeyWord: String = "open"
  def csCommentInline(comment: String): String =
    if (comment.contains('\n'))
      s"""/*
         |${comment.hindent}
         |*/""".stripMargin
    else s"/*$comment*/"
  def csCommentEOL(comment: String): String = s"-- $comment"
  def csDocString(doc: String): String = doc.linesIterator.mkString("--", "\n--", "")
  def csAnnotations(meta: Meta): String = ""
  def csTimer(timer: Timer): String = unsupported
  def globalFileName: String = s"${printer.packageName}.vhd"
  def designFileName(designName: String): String = s"$designName.vhd"
  override def csGlobalFileContent: String =
    // In VHDL the vectors need to be named, and put in dependency order of other named types.
    // So first we prepare the vector type declarations in a mutable map and later we remove
    // entries that were already placed in the final type printing.
    val vectorTypeDcls = mutable.Map.from(
      printer.globalVectorTypes.view.map((tpName, depth) =>
        tpName -> printer.csDFVectorDclsGlobal(DclScope.Pkg)(tpName, depth)
      )
    )
    // The body declarations can be in any order, as long as it's consistent between compilations.
    val vectorTypeDclsBody =
      printer.globalVectorTypes.view.map(printer.csDFVectorDclsGlobal(DclScope.PkgBody))
        .mkString("\n").emptyOr(x => s"$x\n")
    // collect the global named types, including vectors
    val namedDFTypes = ListSet.from(getSet.designDB.members.view.collect {
      case port @ DclPort()                     => port.dfType
      case const @ DclConst() if const.isGlobal => const.dfType
    }.flatMap(_.decompose { case dt: (DFVector | NamedDFType) => dt }))
    // declarations of the types and relevant functions
    val namedTypeConvFuncsDcl = namedDFTypes.view
      .flatMap {
        // vector types can have different dimensions, but we only need the declaration once
        case dfType: DFVector =>
          val tpName = printer.getVecDepthAndCellTypeName(dfType)._1
          vectorTypeDcls.get(tpName) match
            case Some(desc) =>
              vectorTypeDcls -= tpName
              Some(desc)
            case None => None
        case dfType: NamedDFType =>
          List(
            printer.csNamedDFTypeDcl(dfType, global = true),
            printer.csNamedDFTypeConvFuncsDcl(dfType)
          )
      }
      .mkString("\n").emptyOr(x => s"$x\n")
    val namedTypeConvFuncsBody =
      getSet.designDB.getGlobalNamedDFTypes.view
        .collect { case dfType: NamedDFType => printer.csNamedDFTypeConvFuncsBody(dfType) }
        .mkString("\n").emptyOr(x => s"$x\n")
    s"""library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |
       |package ${printer.packageName} is
       |${csGlobalConstDcls + namedTypeConvFuncsDcl}
       |function cadd(A, B : unsigned) return unsigned;
       |function cadd(A, B : signed) return signed;
       |function csub(A, B : unsigned) return unsigned;
       |function csub(A, B : signed) return signed;
       |function clog2(n : natural) return natural;
       |function to_slv(A : unsigned) return std_logic_vector;
       |function to_slv(A : signed) return std_logic_vector;
       |function to_slv(A : integer) return std_logic_vector;
       |function to_slv(A : boolean) return std_logic_vector;
       |function to_slv(A : std_logic) return std_logic_vector;
       |function to_sl(A : boolean) return std_logic;
       |function to_sl(A : std_logic_vector(0 downto 0)) return std_logic;
       |function to_bool(A : std_logic) return boolean;
       |function to_bool(A : std_logic_vector(0 downto 0)) return boolean;
       |function bitWidth(A : std_logic_vector) return integer;
       |function bitWidth(A : unsigned) return integer;
       |function bitWidth(A : signed) return integer;
       |function bitWidth(A : integer) return integer;
       |function bitWidth(A : boolean) return integer;
       |function bitWidth(A : std_logic) return integer;
       |function resize(A : std_logic_vector; new_length : integer) return std_logic_vector;
       |function slv_sll(slv : std_logic_vector; num_shifts : integer) return std_logic_vector;
       |function slv_srl(slv : std_logic_vector; num_shifts : integer) return std_logic_vector;
       |function signed_sra(A : signed; num_shifts : integer) return signed;
       |end package ${printer.packageName};
       |
       |package body ${printer.packageName} is
       |${namedTypeConvFuncsBody + vectorTypeDclsBody}
       |function cadd(A, B : unsigned) return unsigned is
       |begin
       |    return unsigned('0' & A) + unsigned('0' & B);
       |end function;
       |function cadd(A, B : signed) return signed is
       |begin
       |    return signed(A(A'left) & A) + signed(B(B'left) & B);
       |end function;
       |function csub(A, B : unsigned) return unsigned is
       |begin
       |    return unsigned('0' & A) - unsigned('0' & B);
       |end function;
       |function csub(A, B : signed) return signed is
       |begin
       |    return signed(A(A'left) & A) - signed(B(B'left) & B);
       |end function;
       |function clog2(n : natural) return natural is
       |  variable result : natural := 0;
       |  variable val : natural := n - 1; 
       |begin
       |  while val > 0 loop
       |    val := val / 2;
       |    result := result + 1;
       |  end loop;
       |  return result;
       |end function;
       |function to_slv(A : unsigned) return std_logic_vector is
       |begin
       |  return std_logic_vector(A);
       |end;
       |function to_slv(A : signed) return std_logic_vector is
       |begin
       |  return std_logic_vector(A);
       |end;
       |function to_slv(A : integer) return std_logic_vector is
       |begin
       |  return std_logic_vector(to_signed(A, 32));
       |end;
       |function to_slv(A : boolean) return std_logic_vector is
       |begin
       |  if A then 
       |    return "1";
       |  else
       |    return "0";
       |  end if;
       |end;
       |function to_slv(A : std_logic) return std_logic_vector is
       |begin
       |  if A = '1' then 
       |    return "1";
       |  else
       |    return "0";
       |  end if;
       |end;
       |function to_sl(A : boolean) return std_logic is
       |begin
       |  if (A) then
       |    return '1';
       |  else
       |    return '0';
       |  end if;
       |end;
       |function to_sl(A : std_logic_vector(0 downto 0)) return std_logic is
       |begin
       |  if (A = "1") then
       |    return '1';
       |  else
       |    return '0';
       |  end if;
       |end;
       |function to_bool(A : std_logic) return boolean is
       |begin
       |  if (A = '1') then
       |    return true;
       |  else
       |    return false;
       |  end if;
       |end;
       |function to_bool(A : std_logic_vector(0 downto 0)) return boolean is
       |begin
       |  if (A = "1") then
       |    return true;
       |  else
       |    return false;
       |  end if;
       |end;
       |function bitWidth(A : std_logic_vector) return integer is
       |begin
       |  return A'length;
       |end;
       |function bitWidth(A : unsigned) return integer is
       |begin
       |  return A'length;
       |end;
       |function bitWidth(A : signed) return integer is
       |begin
       |  return A'length;
       |end;
       |function bitWidth(A : integer) return integer is
       |begin
       |  return 32;
       |end;
       |function bitWidth(A : boolean) return integer is
       |begin
       |  return 1;
       |end;
       |function bitWidth(A : std_logic) return integer is
       |begin
       |  return 1;
       |end;
       |function resize(A : std_logic_vector; new_length : integer) return std_logic_vector is
       |begin
       |  if new_length > A'length then
       |    return (new_length - A'length - 1 downto 0 => '0') & A(A'length - 1 downto 0);
       |  elsif new_length < A'length then
       |    return A(A'length - 1 downto A'length - new_length);
       |  else
       |    return A;
       |  end if;
       |end;
       |function slv_sll(slv : std_logic_vector; num_shifts : integer) return std_logic_vector is
       |begin
       |    return to_slv(unsigned(slv) sll num_shifts);
       |end;
       |function slv_srl(slv : std_logic_vector; num_shifts : integer) return std_logic_vector is
       |begin
       |    return to_slv(unsigned(slv) srl num_shifts);
       |end;
       |function signed_sra(A : signed; num_shifts : integer) return signed is
       |begin
       |    return shift_right(A, num_shifts);
       |end;
       |end package body ${printer.packageName};
       |""".stripMargin
  end csGlobalFileContent
  def alignCode(cs: String): String =
    cs
      .align(".*", ":", "[ ]*(?:in|out|inout) .*")
      .align(".*:[ ]*(?:in|out|inout)", " ", ".*")
      .align("[ ]*(?:signal|variable|constant) .*", ": ", ".*")
      .align("[ ]*[a-zA-Z0-9_.]+[ ]*", ":=|<=", ".*")
  val vhdlKW: Set[String] = reservedKeywords
  val vhdlOps: Set[String] = Set(":=", "<=")
  val vhdlTypes: Set[String] =
    Set("std_logic", "std_logic_vector", "integer", "natural", "positive", "ieee", "numeric_std",
      "std_logic_1164", "work", "signed", "unsigned", "'left")
  def colorCode(cs: String): String =
    cs
      .colorWords(vhdlKW, keywordColor)
      .colorOps(vhdlOps, keywordColor)
      .colorWords(vhdlTypes, typeColor)
end VHDLPrinter
