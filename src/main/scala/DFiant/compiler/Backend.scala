package DFiant.compiler
import DFiant.FunctionalLib.Func2Comp
import DFiant._
import internals._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

abstract class Backend(design : DFDesign) {
}

object Backend {

  //////////////////////////////////////////////////////////////////////////////////
  // Name
  //////////////////////////////////////////////////////////////////////////////////
  case class Name(value : String) {
    override def toString: String = value
  }
  object Name {
    def apply(member : DFAnyMember) : Name = member match { //TODO: fix name
      case p : DFAny.Port[_,_] => Name(member.name.toUpperCase)
      case _ => Name(member.name)
    }
  }
  //////////////////////////////////////////////////////////////////////////////////

  class VHDL(design : DFDesign, owner : VHDL = null) extends Backend(design) { self =>
    private val top : VHDL = if (owner == null) this else owner
    private val db : VHDL.DB = if (owner == null) VHDL.DB(design.name) else top.db
    private val delim = "  "

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
        case x : DFEnum[_] => Type(db.Package.declarations.enums(x.enum).name.toString)
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${dfVal.fullName} has type ${dfVal.typeName}")
      }
    }
    //////////////////////////////////////////////////////////////////////////////////

    class Reference(val member : DFAny, val name : Name) {
      val typeS : Type = Type(member)
      References.add(member, this)
    }
    object References {
      private val hashMap : HashMap[DFAny, Reference] = HashMap.empty[DFAny, Reference]
      def print() : Unit = println(hashMap.map(e => s"${e._1.name} -> ${e._2.name}").mkString("\n"))
      def apply(dfVal : DFAny) : Reference = hashMap.getOrElse(dfVal, throw new IllegalArgumentException(s"No reference for ${dfVal.fullName}"))
//      def apply(dfVal : DFAny) : Reference = hashMap.getOrElse(dfVal, architecture.declarations.signal(dfVal))
      def add(member : DFAny, reference : Reference) : Unit = hashMap.update(member, reference)
    }

    class const private (member : DFAny.Const, name : Name) extends Reference(member, name) {
    }
    object const {
      def apply(token : DFAny.Token, member : DFAny) : String = token match {
        case x : DFBits.Token => s""""${x.value.toBin}""""
        case x : DFUInt.Token => s"to_unsigned(${member.width}, ${x.value})"
        case x : DFSInt.Token => s"to_signed(${member.width}, ${x.value})"
        case x : DFBool.Token => {
          val s : String = if (x.value) "'1'" else "'0'"
          s
        }
        case x : DFEnum.Token[_] => db.Package.declarations.enums.entries(x.value).name.toString
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
      }
      def apply(member : DFAny.Const) : const = {
        val valueStr : String = const(member.constLB.get, member)
        new const(member, Name(valueStr))
      }
    }

    //////////////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////////////
    private object entity {
      val name : Name = Name(s"${design.typeName}")
      private def emitPort(name : String, dir : String, typeS : String) : String =
        f"\n$delim$name%-20s : $dir%-3s $typeS"
      class port(member : DFAny, name : Name, dir : String) extends Reference(member, name) {
        ports.list += this
        override def toString : String = emitPort(name.toString, dir, typeS.toString)
      }
      object port {
        def apply(dfPort : DFAny.Port[_ <: DFAny,_ <: DFDir]) : port = {
          val dir : String = dfPort.dir.toString.toLowerCase()
          new port(dfPort, Name(dfPort), dir)
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
        class signal(member : DFAny, name : Name) extends Reference(member, name) {
          signals.list += this
          override def toString: String = f"\n${delim}signal $name%-13s : $typeS;"
        }
        object signal {
          def apply(dfVal : DFAny) : signal = new signal(dfVal, Name(dfVal))
          def apply(port : DFAny.Port[_,_]) : signal = new signal(port, Name(s"${port.owner.name}_${Name(port)}"))
        }
        object signals {
          val list : ListBuffer[signal] = ListBuffer.empty[signal]
          override def toString: String = list.mkString
        }

        class alias(member : DFAny.Alias[_], name : Name) extends signal(member, name)
        object alias {
          private def toBits(member : DFAny) : String = member match {
            case a : DFBits[_] => s"${References(a).name}" //already a bits vector
            case a : DFUInt[_] => s"std_logic_vector(${References(a).name})"
            case a : DFSInt[_] => s"std_logic_vector(${References(a).name})"
            case a : DFBool => s"std_logic_vector(${References(a).name})"
            case a : DFEnum[_] => s"${db.Package.declarations.enums(a.enum)}'POS(${References(a).name})"
            case a => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${a.fullName} has type ${a.typeName}")
          }
          def apply(member : DFAny.Alias[_]) : alias = {
            if (!member.reference.isInstanceOf[DFAny.Alias.Reference.AsIs]) assert(member.aliasedVars.length == 1)
            val aliasStr : String = member.reference match {
              case DFAny.Alias.Reference.BitsWL(relWidth, relBitLow) =>
                s"${toBits(member.aliasedVars.head)}(${relWidth-1} downto $relBitLow)"
              case DFAny.Alias.Reference.BitReverse() =>
                assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
                s"bit_reverse(${member.aliasedVars.head.name})"
              case DFAny.Alias.Reference.Invert() =>
                assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
                s"not ${member.aliasedVars.head.name}"
              case DFAny.Alias.Reference.Prev(step) => ??? //new architecture.declarations.signal(member, Name(s"${member.name}"))
              case DFAny.Alias.Reference.AsIs() =>
                val concat : String = member.aliasedVars.map(a => toBits(a)).mkString(" & ")
                member match {
                  case m : DFBits[_] => concat
                  case m : DFUInt[_] => s"unsigned($concat)"
                  case m : DFSInt[_] => s"signed($concat)"
                  case m : DFBool => s"$concat(0)"
                  case m : DFEnum[_] => s"${db.Package.declarations.enums(m.enum)}'VAL($concat)"
                  case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
                }
            }

            val dst = new alias(member, Name(member.name))
            architecture.statements.async_process.assignment(dst, aliasStr)
            dst
          }
        }

        override def toString : String = s"$signals"
      }
      object statements {
        def func2(member : Func2Comp[_,_,_]) : Unit = {
          val leftStr = {
            val left = References(member.leftArg.asInstanceOf[DFAny])
            if (member.leftArg.asInstanceOf[DFAny].width < member.width) s"resize(${left.name}, ${member.width})"
            else s"${left.name}"
          }
          val right = References(member.rightArg.asInstanceOf[DFAny])
          val result = architecture.declarations.signal(member)
          val op = member.opString match {
            case "&" => "and"
            case "|" => "or"
            case "^" => "xor"
            case "<<" => if (member.leftArg.isInstanceOf[DFSInt[_]]) "sla" else "sll"
            case ">>" => if (member.leftArg.isInstanceOf[DFSInt[_]]) "sra" else "srl"
            case others => others
          }
          architecture.statements.async_process.assignment(result, s"$leftStr $op ${right.name}")
        }

        case class component_instance(member : DFDesign) extends VHDL(member, self) {
          private def emitConnection(portName : String, signalName : String) : String =
            f"\n$delim$portName%-20s => $signalName"
          case class connection(port : DFAny.Port[_ <: DFAny,_ <: DFDir], signal : architecture.declarations.signal) {
            self.References.add(port, signal)
            override def toString: String = emitConnection(port.name.toUpperCase, signal.name.toString) //TODO: use actual port name
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

        class process(val delimCnt : Int) {
          class variable(member : DFAny, name : Name, val sigport : Reference) extends Reference(member, name) {
            variables.list += this
            override def toString: String = f"\n${delim}variable $name%-11s : $typeS;"
          }
          object variable {
            def apply(member : DFAny, name : Name, sigport : Reference) : variable = new variable(member, name, sigport)
          }
          object variables {
            val list : ListBuffer[variable] = ListBuffer.empty[variable]
//            def fromSigPorts : String = list.map(v => assignment(v.sigport))
            def toSigPorts : Unit = list.foreach(v => assignment(v.sigport, v))
            override def toString: String = list.mkString
          }
          class statement {
            steadyStateStatements.list += this
          }
          case class assignment(dst : Reference, src : String) extends statement {
            final val op = dst match {
              case x : variable => ":="
              case _ => "<="
            }
            override def toString: String = f"\n${delim * delimCnt + dst.name}%-22s $op $src;"
          }
          object assignment {
            def apply(dst : Reference, src : Reference) : assignment =
              if (dst.member.width == src.member.width) assignment(dst, src.name.toString)
              else assignment(dst, s""""${"0" * (dst.member.width - src.member.width)}" & ${src.name}""")
          }
          object steadyStateStatements {
            val list : ListBuffer[statement] = ListBuffer.empty[statement]
            override def toString: String = list.mkString
          }
        }
        object sync_process extends process(2) {
          case class resetStatement(dst : Reference, value : String){
            resetStatements.list += this
            override def toString: String = f"\n${delim * delimCnt + dst.name}%-22s <= $value;"
          }
          object resetStatements {
            val list : ListBuffer[resetStatement] = ListBuffer.empty[resetStatement]
            override def toString: String = list.mkString
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
        object async_process extends process(1) {
          override def toString: String = if (steadyStateStatements.list.isEmpty) "" else
            s"""
               |process (all)$variables
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
        if (x.assigned) {
          val dstSig = architecture.declarations.signal(x)
          val dstSigP1 = new architecture.declarations.signal(x, Name(s"${dstSig.name}_prev"))
          val dstVar = architecture.statements.async_process.variable(x, Name(s"v_${dstSig.name}"), dstSig)
          architecture.statements.async_process.assignment(dstVar, dstSigP1)
          if (x.initLB.get.nonEmpty)
            architecture.statements.sync_process.resetStatement(dstSigP1, const(x.initLB.get.head, x))
          architecture.statements.sync_process.assignment(dstSigP1, dstSig)
        }
        else architecture.declarations.signal(x)
      case x : DFAny.Const => const(x)
      case x : DFAny.Alias[_] => architecture.declarations.alias(x)
      case x : Func2Comp[_,_,_] => architecture.statements.func2(x)

      case x : DFDesign => architecture.statements.component_instance(x)
      case x : DFAny.Connector => if (!x.toPort.owner.isInstanceOf[Func2Comp[_,_,_]]) {
        val dstSig = References(x.toPort)
        val srcSig = References(x.fromVal)
        architecture.statements.async_process.assignment(dstSig, srcSig)
      }
      case x : DFAny.Assignment =>
        val dstVar = References(x.toVar)
        val srcSig = References(x.fromVal)
        architecture.statements.async_process.assignment(dstVar, srcSig)
      case x =>
        println(x.fullName)
    }

    def body : Tuple2[String, String] = (entity.body, architecture.body)
    override def toString: String = db.toString

    val entityName : Name = {
      pass
      architecture.statements.async_process.variables.toSigPorts
      Name(db.addOwnerBody(design.typeName, body, this))
    }
    val archName : Name = Name(s"${entityName}_arch")
  }

  object VHDL {
    private case class DB(topName : String) extends DSLOwnerConstruct.DB[VHDL, Tuple2[String, String]] {
      //////////////////////////////////////////////////////////////////////////////////
      // Library
      //////////////////////////////////////////////////////////////////////////////////
      private object Library {
        override def toString : String =
          s"""
             |library ieee;
             |use ieee_std_logic_1164.all;
             |use ieee.numeric_std.all;
             |use work.${topName}_pkg.all;
             |""".stripMargin
      }
      //////////////////////////////////////////////////////////////////////////////////

      object HelperFunctions{
        val bitReverseFunc : String =
          s"""
             |--Taken from http://www.vlsiip.com/intel/vhdlf.html
             |function bit_reverse(s1:std_logic_vector) return std_logic_vector is
             |   variable rr : std_logic_vector(s1'high downto s1'low);
             |begin
             |  for ii in s1'high downto s1'low loop
             |    rr(ii) := s1(s1'high-ii);
             |  end loop;
             |  return rr;
             |end bit_reverse;
         """.stripMargin

        override def toString: String = bitReverseFunc
      }
      //////////////////////////////////////////////////////////////////////////////////
      // Package
      //////////////////////////////////////////////////////////////////////////////////
      object Package {
        val name = Name(topName.toString + "_pkg")
        object declarations {
          case class enum_entry(entry : Enum.Entry, name : Name) {
            enums.entries.hashMap.update(entry, this)
            override def toString: String = s"$name"
          }
          case class enum_type(enumType : Enum, name : Name, entries : List[enum_entry]) {
            enums.hashMap.update(enumType, this)
            def typeList : String = entries.mkString(", ")
            override def toString: String = s"\ntype $name is ($typeList);"
          }
          object enums {
            val hashMap : HashMap[Enum, enum_type] = HashMap.empty[Enum, enum_type]
            def apply(enumType : Enum) : enum_type = hashMap.getOrElse(enumType, {
              val typeName : Name = Name(enumType.name + "_type")
              val entries : List[enum_entry] =
                enumType.entries.toList.map(e => enum_entry(e._2, Name(s"E_${enumType.name}_${e._2.name}".toUpperCase)))
              enum_type(enumType, typeName, entries)
            })
            object entries {
              val hashMap : HashMap[Enum.Entry, enum_entry] = HashMap.empty[Enum.Entry, enum_entry]
              def apply(entry : Enum.Entry) : enum_entry = hashMap(entry)
            }
            override def toString: String = hashMap.values.mkString("\n")
          }
          override def toString: String = s"$enums"
        }
        override def toString: String =
          s"""
             |library ieee;
             |use ieee_std_logic_1164.all;
             |
             |package $name is
             |$HelperFunctions
             |$declarations
             |end package $name;
           """.stripMargin
      }
      //////////////////////////////////////////////////////////////////////////////////

      def ownerToString(ownerTypeName: String, ownerBody: (String, String)): String = {
        def entity : String = s"\nentity $ownerTypeName is${ownerBody._1}\nend $ownerTypeName;"
        def architecture : String = s"\narchitecture ${ownerTypeName}_arch of $ownerTypeName is${ownerBody._2}\nend ${ownerTypeName}_arch;"
        s"$Library$entity\n$architecture"
      }

      override def toString : String = s"$Package\n${super.toString}"
    }
  }
}