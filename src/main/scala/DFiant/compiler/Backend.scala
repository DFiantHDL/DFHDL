package DFiant.compiler
import DFiant.FunctionalLib.Func2Comp
import DFiant._
import internals._

import scala.collection.mutable.ListBuffer

abstract class Backend(design : DFDesign) {
}

object Backend {
  case class VHDL(design : DFDesign) extends Backend(design) {
    private val delim = "  "

    //////////////////////////////////////////////////////////////////////////////////
    // Name
    //////////////////////////////////////////////////////////////////////////////////
    case class Name(value : String) {
      override def toString: String = value
    }
    object Name {
      def apply(member : DFAnyMember) : Name = member match { //TODO: fix name
        case _ : DFAny.Port[_,_] => Name(member.name.capitalize)
        case _ => Name(member.name)
      }
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Value
    //////////////////////////////////////////////////////////////////////////////////
    case class Value private (value : String) {
      override def toString: String = value
    }
    object Value {
      def apply(dfVal : DFAny) : Value = if (!dfVal.isConstant) Value(Name(dfVal).toString) else dfVal match {
        case x: DFBits[_] => Value(dfVal.constLB.get.codeString)
        case x: DFUInt[_] => Value(dfVal.constLB.get.codeString)
        case x: DFSInt[_] => Value(dfVal.constLB.get.codeString)
        case x: DFBool => Value(dfVal.constLB.get.codeString)
        case x: Func2Comp[_,_,_] => Value(s"${Value(x.inLeft)} ${x.opString} ${Value(x.inRight)})")
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${dfVal.fullName} has type ${dfVal.typeName}")
      }
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Type
    //////////////////////////////////////////////////////////////////////////////////
    case class Type (value : String) {
      override def toString: String = value
    }
    object Type {
      def apply(dfVal : DFAny) : Type = dfVal match {
        case x : DFBits[_] => Type(s"std_logic_vector(${x.width-1} downto 0)")
        case x : DFUInt[_] => Type(s"unsigned(${x.width-1} downto 0)")
        case x : DFSInt[_] => Type(s"signed(${x.width-1} downto 0)")
        case x : DFBool => Type(s"std_logic")
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${dfVal.fullName} has type ${dfVal.typeName}")
      }
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Library
    //////////////////////////////////////////////////////////////////////////////////
    private object library {
      override def toString : String =
        s"""
           |library ieee;
           |use ieee_std_logic_1164.all;
           |use ieee.numeric_std.all;
           |""".stripMargin
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////////////
    private object entity {
      val name : Name = Name(design)
      object ports {
        object portList {
          case class port(name : Name, dir : String, typeS : Type) {
            override def toString : String = f"\n$delim$name%-20s : $dir%-3s $typeS"
          }
          object port {
            def apply(dfPort : DFAny.Port[_ <: DFAny,_ <: DFDir]) : port = {
              val dir : String = dfPort.dir.toString.toLowerCase()
              port(Name(dfPort), dir, Type(dfPort))
            }
          }
          object clkPort extends port(Name("CLK"), "in", Type("std_logic"))
          object rstPort extends port(Name("RSTn"), "in", Type("std_logic"))
          override def toString : String = (clkPort :: rstPort :: design.ports.map(p => port(p))).mkString(";")
        }
        override def toString : String = s"\nport($portList\n);"
      }
      override def toString : String = s"\nentity $name is$ports\nend $name;"
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Architecture
    //////////////////////////////////////////////////////////////////////////////////
    private object architecture {
      val name : Name = Name(s"${entity.name}_arch")
      object declarations {
        case class signal(name : Name, typeS : String) {
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
          case class variable(name : Name, typeS : String) {
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
          case class sigport_assignment(dst : Name, src : Value) extends statement {
            override def toString: String = s"\n$delim$dst <= $src;"
          }
          object sigport_assignment {
            def apply(dstVal : DFAny, srcVal : DFAny) : sigport_assignment =
              sigport_assignment(Name(dstVal), Value(srcVal))
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
    //////////////////////////////////////////////////////////////////////////////////

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