package dfhdl.app
import dfhdl.options.CompilerOptions

given SingleValueConverter[CompilerOptions.Backend] with
  def parse(
      arg: String
  ): Either[String, Option[CompilerOptions.Backend]] =
    arg.split("\\.").toList match
      case lang :: Nil =>
        lang match
          case "verilog" => Right(Some(dfhdl.backends.verilog))
          case "vhdl"    => Right(Some(dfhdl.backends.vhdl))
          case _         => Left(s"Invalid backend language: $lang")
      case lang :: dialect :: Nil =>
        lang match
          case "verilog" =>
            dialect match
              case "v2001"  => Right(Some(dfhdl.backends.verilog.v2001))
              case "sv2005" => Right(Some(dfhdl.backends.verilog.sv2005))
              case "sv2012" => Right(Some(dfhdl.backends.verilog.sv2012))
              case "sv2017" => Right(Some(dfhdl.backends.verilog.sv2017))
              case _        => Left(s"Invalid Verilog/SystemVerilog backend dialect: $dialect")
          case "vhdl" =>
            dialect match
              case "v93"   => Right(Some(dfhdl.backends.vhdl.v93))
              case "v2008" => Right(Some(dfhdl.backends.vhdl.v2008))
              case "v2019" => Right(Some(dfhdl.backends.vhdl.v2019))
              case _       => Left(s"Invalid VHDL backend dialect: $dialect")
          case _ => Left(s"Invalid backend language: $lang")
      case _ => Left("Invalid backend syntax. Found too many separator colons.")
    end match
  end parse
end given
