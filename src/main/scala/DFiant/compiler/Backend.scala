package DFiant.compiler
import DFiant._
import internals._

import scala.collection.mutable.ListBuffer

abstract class Backend(design : DFDesign) {
}

object Backend {
  case class VHDL(design : DFDesign) extends Backend(design) {
    private val delim = "  "
    private def legalVHDLName(member : DFAnyMember) : String = member match { //TODO: fix name
      case _ : DFAny.Port[_,_] => member.name.capitalize
      case _ => member.name
    }

    private object library {
      override def toString : String =
        s"""
           |library ieee;
           |use ieee_std_logic_1164.all;
           |use ieee.numeric_std.all;
           |""".stripMargin
    }

    private def typeCodeString(dfVal : DFAny) : String = dfVal match {
      case x : DFBits[_] => s"std_logic_vector(${x.width-1} downto 0)"
      case x : DFUInt[_] => s"unsigned(${x.width-1} downto 0)"
      case x : DFSInt[_] => s"signed(${x.width-1} downto 0)"
      case x : DFBool => s"std_logic"
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${dfVal.fullName} has type ${dfVal.typeName}")
    }
    private def valueCodeString(dfVal : DFAny) : String = if (!dfVal.isConstant) legalVHDLName(dfVal) else dfVal match {
      case x : DFBits[_] => dfVal.constLB.get.codeString
      case x : DFUInt[_] => dfVal.constLB.get.codeString
      case x : DFSInt[_] => dfVal.constLB.get.codeString
      case x : DFBool => dfVal.constLB.get.codeString
      case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${dfVal.fullName} has type ${dfVal.typeName}")
    }

    private object entity {
      val name : String = legalVHDLName(design)
      object ports {
        object portList {
          case class port(name : String, dir : String, typeS : String) {
            override def toString : String = f"\n$delim$name%-20s : $dir%-3s $typeS"
          }
          object port {
            def apply(dfPort : DFAny.Port[_ <: DFAny,_ <: DFDir]) : port = {
              val name : String = legalVHDLName(dfPort).capitalize
              val dir : String = dfPort.dir.toString.toLowerCase()
              port(name, dir, typeCodeString(dfPort))
            }
          }
          object clkPort extends port("CLK", "in", "std_logic")
          object rstPort extends port("RSTn", "in", "std_logic")
          override def toString : String = (clkPort :: rstPort :: design.ports.map(p => port(p))).mkString(";")
        }
        override def toString : String = s"\nport($portList\n);"
      }
      override def toString : String = s"\nentity $name is$ports\nend $name;"
    }

    private object architecture {
      val name : String = s"${entity.name}_arch"
      object declarations {
        case class signal(name : String, typeS : String) {
          signals.list += this
          override def toString: String = s"\nsignal $name : $typeS"
        }
        object signals {
          val list : ListBuffer[signal] = ListBuffer.empty[signal]
          override def toString: String = list.mkString
        }
        override def toString : String = s"$signals"
      }
      object statements {
        class process {
          case class variable(name : String, typeS : String) {
            variables.list += this
            override def toString: String = s"\n${delim}variable $name : $typeS"
          }
          object variables {
            val list : ListBuffer[variable] = ListBuffer.empty[variable]
            override def toString: String = list.mkString
          }
          class statement {
            steadyStateStatements.list += this
          }
          case class sigport_assignment(dst : String, src : String) extends statement {
            override def toString: String = s"\n$delim$dst <= $src;"
          }
          object sigport_assignment {
            def apply(dstVal : DFAny, srcVal : DFAny) : sigport_assignment =
              sigport_assignment(legalVHDLName(dstVal), valueCodeString(srcVal))
          }
          object steadyStateStatements {
            val list : ListBuffer[statement] = ListBuffer.empty[statement]
            override def toString: String = list.mkString
          }
        }
        object sync_process extends process {
          object resetStatements {
            override def toString: String = s"\n"
          }
          override def toString: String = if (steadyStateStatements.list.isEmpty) "" else
            s"""
               |process (CLK, RSTn)
               |begin
               |  if RSTn = '0' then$resetStatements
               |  elsif rising_edge(CLK) then$steadyStateStatements
               |  end if;
               |end process;
               |""".stripMargin

        }
        object async_process extends process {
          override def toString: String = if (steadyStateStatements.list.isEmpty) "" else
            s"""
               |process (all)
               |begin$steadyStateStatements
               |end process;
               |""".stripMargin

        }
        override def toString : String = s"$sync_process$async_process"
      }
      override def toString : String = s"\narchitecture $name of ${entity.name} is$declarations\nbegin\n$statements\nend $name;"
    }
    def pass : Unit = design.discoveredList.foreach {
      case x : DFAny.Port[_,_] if x.dir.isIn =>
      case x : DFAny.Port[_,_] if x.dir.isOut => architecture.statements.async_process.sigport_assignment(x, x.getDFValue)
      case x : DFAny.Connector =>
      case x =>
        println(x.fullName)

    }

    override def toString : String = s"$library$entity\n$architecture"
    pass
  }

}