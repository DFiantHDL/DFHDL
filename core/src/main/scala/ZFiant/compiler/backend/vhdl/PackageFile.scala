package ZFiant
package compiler.backend.vhdl

object PackageFile {
  def apply()(implicit printer: Printer) : String = {
    import printer.config._
    import formatter._
    val kwWords = Set("library", "use", "package", "end", "begin", "package", "is", "body", "all", "function",
    "return", "for", "loop", "if", "else", "elsif", "then", "variable", "in", "downto", "type")
    val tpWords = Set("std_logic_vector", "std_logic", "boolean", "unsigned", "signed", "ieee",
      "std_logic_1164", "numeric_std", "low", "high", "length")
    val fnWords = Set("bit_reverse", "to_sl", "to_slv")
    val name = Name()
    s"""library ieee;
       |use ieee.std_logic_1164.all;
       |use ieee.numeric_std.all;
       |$EMPTY
       |package $name is
       |${helperFunctions.delim()}
       |${enumDcl.delim()}
       |end package $name;
       |$EMPTY
       |package body $name is
       |${helperFunctionsBody.delim()}
       |end package body $name;
       |""".stripMargin.colorWords(kwWords, KW).colorWords(tpWords, TP).colorWords(fnWords, FN).formatted
  }
  def Name()(implicit printer: Printer) : String = s"${printer.getSet.designDB.top.designType}_pkg"

  private def enumDcl()(implicit printer: Printer) : String = {
    val enumTypes = printer.getSet.designDB.members.collect {
      case DFEnum(enumType) => enumType
    }.distinct
    enumTypes.map { enumType =>
      val typeList = enumType.entries.toList.sortBy(x => x._1).map(x => s"E_${enumType.name}_${x._2.name}".toUpperCase)
      s"type ${enumType.name}_type is (${typeList.mkString(", ")});"
    }.mkString("\n")
  }
  private def helperFunctions()(implicit printer: Printer) : String = {
    import printer.config._
    s"""function bit_reverse(s : std_logic_vector) return std_logic_vector;
       |function resize(arg : std_logic_vector; size : integer) return std_logic_vector;
       |function to_sl(b : boolean) return std_logic;
       |function to_sl(arg : std_logic_vector) return std_logic;
       |function to_slv(arg : std_logic) return std_logic_vector;
       |function to_slv(arg : unsigned) return std_logic_vector;
       |function to_slv(arg : signed) return std_logic_vector;
       |function to_slv(arg : boolean) return std_logic_vector;""".stripMargin
  }

  private def helperFunctionsBody()(implicit printer: Printer) : String = {
    import printer.config._
    s"""function bit_reverse(s : std_logic_vector) return std_logic_vector is
       |   variable v_s : std_logic_vector(s'high downto s'low);
       |begin
       |  for i in s'high downto s'low loop
       |    v_s(i) := s(s'high - i);
       |  end loop;
       |  return v_s;
       |end bit_reverse;
       |function resize(arg : std_logic_vector; size : integer) return std_logic_vector is
       |begin
       |  return to_slv(resize(unsigned(arg), size));
       |end resize;
       |function to_sl(b : boolean) return std_logic is
       |begin
       |  if (b) then
       |    return '1';
       |  else
       |    return '0';
       |  end if;
       |end to_sl;
       |function to_sl(arg : std_logic_vector) return std_logic is
       |begin
       |  return arg(arg'low);
       |end to_sl;
       |function to_slv(arg : std_logic) return std_logic_vector is
       |begin
       |  if (arg = '1') then
       |    return "1";
       |  else
       |    return "0";
       |  end if;
       |end to_slv;
       |function to_slv(arg : unsigned) return std_logic_vector is
       |  variable slv : std_logic_vector(arg'length-${LIT}1 downto $LIT 0);
       |begin
       |  slv := std_logic_vector(arg);
       |  return slv;
       |end to_slv;
       |function to_slv(arg : signed) return std_logic_vector is
       |  variable slv : std_logic_vector(arg'length-${LIT}1 downto $LIT 0);
       |begin
       |  slv := std_logic_vector(arg);
       |  return slv;
       |end to_slv;
       |function to_slv(arg : boolean) return std_logic_vector is
       |begin
       |  if (arg) then
       |    return "1";
       |  else
       |    return "0";
       |  end if;
       |end to_slv;""".stripMargin
  }
}
