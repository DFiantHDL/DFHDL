package DFiant.compiler
import DFiant.FunctionalLib.Func2Comp
import DFiant._
import internals._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

abstract class Backend(design : DFDesign) {
}

object Backend {

  class VHDL(design : DFDesign, owner : VHDL = null) extends Backend(design) { self =>
    private val top : VHDL = if (owner == null) this else owner
    private val db : VHDL.DB = if (owner == null) VHDL.DB() else top.db
    private val delim = "  "

    //////////////////////////////////////////////////////////////////////////////////
    // Name
    //////////////////////////////////////////////////////////////////////////////////
    case class Name(value : String) {
      override def toString: String = value
    }
    object Name {
      def apply(member : DFAnyMember) : Name = member match { //TODO: fix name
        case p : DFAny.Port[_,_] => Name(member.name.capitalize)
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

    class Reference(member : DFAnyMember, val name : Name) {
      References.add(member, this)
    }
    object References {
      private val hashMap : HashMap[DFAnyMember, Reference] = HashMap.empty[DFAnyMember, Reference]
      def print() : Unit = println(hashMap.map(e => s"${e._1.name} -> ${e._2.name}").mkString("\n"))
      def apply(dfVal : DFAny) : Reference = hashMap.getOrElse(dfVal, throw new IllegalArgumentException(s"No reference for ${dfVal.fullName}"))
      def add(member : DFAnyMember, reference : Reference) : Unit = hashMap.update(member, reference)
    }

    //////////////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////////////
    private object entity {
      val name : Name = Name(s"${design.typeName}")
      private def emitPort(name : String, dir : String, typeS : String) : String =
        f"\n$delim$name%-20s : $dir%-3s $typeS"
      case class port(member : DFAny, override val name : Name, dir : String, typeS : Type) extends Reference(member, name) {
        ports.list += this
        override def toString : String = emitPort(name.toString, dir, typeS.toString)
      }
      object port {
        def apply(dfPort : DFAny.Port[_ <: DFAny,_ <: DFDir]) : port = {
          val dir : String = dfPort.dir.toString.toLowerCase()
          port(dfPort, Name(dfPort), dir, Type(dfPort))
        }
      }
      object ports {
        val list : ListBuffer[port] = ListBuffer.empty[port]
        private val clkPort : String = emitPort("CLK", "in", "std_logic")
        private val rstPort : String = emitPort("RSTn", "in", "std_logic")
        def portList : String = (clkPort +: rstPort +: list.map(p => p.toString)).mkString(";")
        override def toString : String = s"\nport($portList\n);"
      }
      def body : String = ports.toString
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Architecture
    //////////////////////////////////////////////////////////////////////////////////
    private object architecture {
      val name : Name = Name(s"${design.typeName}_arch")
      object declarations {
        case class signal(member : DFAny, override val name : Name, typeS : Type) extends Reference(member, name) {
          signals.list += this
          override def toString: String = s"\n${delim}signal $name : $typeS;"
        }
        object signal {
          def apply(dfVal : DFAny) : signal = signal(dfVal, Name(dfVal), Type(dfVal))
          def apply(port : DFAny.Port[_,_]) : signal = signal(port, Name(s"${port.owner.name}_${Name(port)}"), Type(port))
        }
        object signals {
          val list : ListBuffer[signal] = ListBuffer.empty[signal]
          override def toString: String = list.mkString
        }
        override def toString : String = s"$signals"
      }
      object statements {
        class component_instance(member : DFDesign) extends VHDL(member, self) {
          private def emitConnection(portName : String, signalName : String) : String =
            f"\n$delim$portName%-20s => $signalName"
          case class connection(port : DFAny.Port[_ <: DFAny,_ <: DFDir], signal : architecture.declarations.signal) {
            self.References.add(port, signal)
            override def toString: String = emitConnection(port.name, signal.name.toString)
          }
          object ports_map {
            lazy val list : List[connection] = member.ports.map(p => {
              connection(p, architecture.declarations.signal(p))
            })
            private val clkConn = emitConnection("CLK", "CLK")
            private val rstConn = emitConnection("RSTn", "RSTn")
            override def toString: String = (clkConn :: rstConn :: list.map(e => e.toString)).mkString(",")
          }

          components.list += this
          ports_map.list
          override def toString: String = s"\n${member.name} : entity $entityName($archName) port map ($ports_map\n);"
        }
        object components {
          val list : ListBuffer[component_instance] = ListBuffer.empty[component_instance]
          override def toString: String = list.mkString("","\n","\n")
        }

        class process {
          case class variable(name : Name, typeS : Type) {
            variables.list += this
            override def toString: String = s"\n${delim}variable $name : $typeS;"
          }
          object variables {
            val list : ListBuffer[variable] = ListBuffer.empty[variable]
            override def toString: String = list.mkString
          }
          class statement {
            steadyStateStatements.list += this
          }
          case class sigport_assignment(dst : Reference, src : Reference) extends statement {
            override def toString: String = s"\n$delim${dst.name} <= ${src.name};"
          }
          object sigport_assignment {}
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
        override def toString : String = s"$components$sync_process$async_process"
      }
      def body : String = {
        val statementsStr = statements.toString //Must load all statements first because they generate declarations
        val declarationsStr = declarations.toString
        s"$declarationsStr\nbegin\n$statementsStr"
      }
    }
    //////////////////////////////////////////////////////////////////////////////////

    def pass : Unit = design.discoveredList.foreach {
      case x : DFAny.Port[_,_] => entity.port(x)
      case x : DFAny.NewVar[_] =>
        architecture.declarations.signal(x)
//        architecture.statements.async_process.sigport_assignment(x, x.getDFValue)
      case x : DFDesign =>
        new architecture.statements.component_instance(x)
//      case x : Func2Comp[_,_,_] =>
//        architecture.declarations.signal(Name(x), Type(x))
//        architecture.statements.async_process.sigport_assignment(Name(x), Value(x))
      case x : DFAny.Connector =>
        val dstSig = References(x.toPort)
        val srcSig = References(x.fromVal)
        architecture.statements.async_process.sigport_assignment(dstSig, srcSig)
      case x =>
        println(x.fullName)
    }

    def body : Tuple2[String, String] = (entity.body, architecture.body)
    override def toString: String = db.toString

    val entityName : Name = {
      pass
      Name(db.addOwnerBody(design.typeName, body, this))
    }
    val archName : Name = Name(s"${entityName}_arch")
  }

  object VHDL {
    private case class DB() extends DSLOwnerConstruct.DB[VHDL, Tuple2[String, String]] {
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


      def ownerToString(ownerTypeName: String, ownerBody: (String, String)): String = {
        def entity : String = s"\nentity $ownerTypeName is${ownerBody._1}\nend $ownerTypeName;"
        def architecture : String = s"\narchitecture ${ownerTypeName}_arch of $ownerTypeName is${ownerBody._2}\nend ${ownerTypeName}_arch;"
        s"$library$entity\n$architecture"
      }
    }
  }
}