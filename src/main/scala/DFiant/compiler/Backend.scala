package DFiant.compiler
import DFiant.SourceTag
import DFiant.FunctionalLib.{CompAlias, Func2Comp}
import DFiant._
import DFiant.internals.{DSLOwnerConstruct, StringExtras, csoIntervalBigInt}

import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

abstract class Backend(design : DFInterface) {
}

object Backend {

  //////////////////////////////////////////////////////////////////////////////////
  // Name
  //////////////////////////////////////////////////////////////////////////////////
  class Name private (val value : String) {
    override def toString: String = value
  }
  final class NameDB {
    val nameTable : HashMap[String, Int] = HashMap.empty[String, Int]
    def getUniqueName(suggestedName : String) : String = {
      val fixedName = suggestedName.replace('.', '_')
      val lcSuggestedName = fixedName.toLowerCase()
      nameTable.get(lcSuggestedName) match {
        case Some(v) =>
          nameTable.update(lcSuggestedName, v + 1)
          fixedName + "_r_" + v //_r_ for RTL indication
        case _ =>
          nameTable.update(lcSuggestedName, 1)
          fixedName
      }
    }
  }
  object Name {
    def apply(value : String)(implicit db : NameDB) : Name = {
      val lcValue = value.toLowerCase
      val nonReserved = if (reservedKeywords.contains(lcValue)) s"${value}0"
      else value
      new Name(db.getUniqueName(nonReserved))
    }
    def apply(member : DFAnyMember)(implicit db : NameDB) : Name = member match { //TODO: fix name
      case p : DFAny.Port[_,_] => Name(member.name.toUpperCase)
      case _ => Name(member.name)
    }
    val reservedKeywords : HashSet[String] = HashSet (
      "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute", "begin",
      "block", "body", "buffer", "bus", "case", "component", "configuration", "constant", "disconnect", "downto",
      "else", "elsif", "end", "entity", "exit", "file", "for", "function", "generate", "generic", "group",
      "guarded", "if", "impure", "in", "inertial", "inout", "is", "label", "library", "linkage", "literal", "loop",
      "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open", "or", "others", "out",
      "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register", "reject",
      "rem", "report", "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sll", "sra",
      "srl", "subtype", "then", "to", "transport", "type", "unaffected", "units", "until", "use", "variable",
      "wait", "when", "while", "with", "xnor", "xor",
    )
  }
  //////////////////////////////////////////////////////////////////////////////////

  class VHDL(design : DFInterface, owner : VHDL = null, simClkPeriodKHz : Option[Int] = None) extends Backend(design) { self =>
    private val top : VHDL = if (owner == null) this else owner
    private implicit val nameDB : NameDB = new NameDB
    private val db : VHDL.DB = if (owner == null) VHDL.DB(design.name.toLowerCase(), design.inSimulation) else top.db
    private val delim = "  "

    private val clkName = Name("CLK")
    private val rstName = Name("RSTn")

    //////////////////////////////////////////////////////////////////////////////////
    // Type
    //////////////////////////////////////////////////////////////////////////////////
    protected sealed trait Type {
      val width : Int
    }
    protected object Type {
      case class std_logic_vector(width : Int) extends Type {
        override def toString: String = s"std_logic_vector(${width-1} downto 0)"
      }
      case class unsigned(width : Int) extends Type {
        override def toString: String = s"unsigned(${width-1} downto 0)"
      }
      case class signed(width : Int) extends Type {
        override def toString: String = s"signed(${width-1} downto 0)"
      }
      case class std_logic() extends Type {
        val width = 1
        override def toString: String = s"std_logic"
      }
      case class enumeration(enum : Enum) extends Type {
        val width = enum.width
        override def toString: String = db.Package.declarations.enums(enum).name.toString
      }

      def apply(member : DFAny) : Type = member match {
        case x : DFBits[_] => std_logic_vector(x.width)
        case x : DFUInt[_] => unsigned(x.width)
        case x : DFSInt[_] => signed(x.width)
        case x : DFBool => std_logic()
        case x : DFEnum[_] => enumeration(x.enum)
        case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
      }
      def apply(value : Value) : Type = value.typeS
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Value
    //////////////////////////////////////////////////////////////////////////////////
    protected trait Value {
      val value : String
      val typeS : Type
      def bits : ValueBits = typeS match {
        case t : Type.std_logic_vector => ValueBits(value, t.width)
        case t : Type.unsigned => ValueBits(s"to_slv($value)", t.width)
        case t : Type.signed => ValueBits(s"to_slv($value)", t.width)
        case t : Type.std_logic => ValueBits(s"to_slv($value)", 1)
        case Type.enumeration(enum) =>
          ValueBits(s"to_slv(to_unsigned(${db.Package.declarations.enums(enum).name}'POS($value), ${enum.width}))", enum.width)
      }
      def bits(width : Int, lsbit : Int) : ValueBits =
        if (width == typeS.width) bits
        else ValueBits(s"$bits(${width+lsbit-1} downto $lsbit)", width)
      def to(that : Type) : Value = (that, this.typeS) match {
        case (dstTpe : Type.std_logic_vector, srcTpe : Type.std_logic_vector) =>
          if (dstTpe.width == srcTpe.width) this
          else Value(s""""${"0" * (dstTpe.width - srcTpe.width)}" & $value""", dstTpe)
        case (dstTpe : Type.unsigned, srcTpe : Type.unsigned) =>
          if (dstTpe.width == srcTpe.width) this
          else Value(s"resize($value, ${dstTpe.width})", dstTpe)
        case (dstTpe : Type.signed, srcTpe : Type.signed) =>
          if (dstTpe.width == srcTpe.width) this
          else Value(s"resize($value, ${dstTpe.width})", dstTpe)
        case (dstTpe : Type.std_logic, srcTpe : Type.std_logic) => this
        case (dstTpe : Type.enumeration, srcTpe : Type.enumeration) if dstTpe.enum == srcTpe.enum => this
        case _ => this.bits.to(that)
      }
      final override def toString: String = value
    }
    protected case class ValueBits(value : String, width : Int) extends Value {
      val typeS = Type.std_logic_vector(width)
      override def to(that: Type) : Value = that match {
        case t : Type.std_logic_vector =>
          if (t.width == width) this
          else Value(s""""${"0" * (t.width - width)}" & $this""", that)
        case t : Type.unsigned =>
          if (t.width == width) Value(s"unsigned($this)", that)
          else Value(s"resize(unsigned($this), ${t.width})", that)
        case t : Type.signed =>
          if (t.width == width) Value(s"signed($this)", that)
          else Value(s"resize(signed($this), ${t.width})", that)
        case t : Type.std_logic => Value(s"to_sl($this)", that)
        case Type.enumeration(enum) =>
          Value(s"${db.Package.declarations.enums(enum)}'VAL($this)", that)
      }
      def replace(relWidth : Int, relBitLow : Int, that : ValueBits) : ValueBits = {
        val rightLSB = 0
        val rightMSB = relBitLow - 1
        val rightWidth = rightMSB - rightLSB + 1
        val midLSB = relBitLow
        val midMSB = relBitLow + relWidth - 1
        val midWidth = relWidth
        val leftLSB = midMSB + 1
        val leftMSB = width - 1
        val leftWidth = leftMSB - leftLSB + 1
        assert(rightWidth >= 0 && midWidth >= 0 && leftWidth >= 0)
        val valueStr : String =
          if (rightWidth == 0 && leftWidth == 0) that.value
          else if (rightWidth == 0 && leftWidth > 0) s"${bits(leftWidth, leftLSB)} & $that" //null-width right part
          else if (rightWidth > 0 && leftWidth == 0) s"$that & ${bits(rightWidth, rightLSB)}" //null-width left part
          else s"${bits(leftWidth, leftLSB)} & $that & ${bits(rightWidth, rightLSB)}"
        ValueBits(valueStr, this.width)
      }
    }
    protected object Value {
      def apply(value_ : String, typeS_ : Type) : Value = new Value {
        override val value: String = value_
        override val typeS: VHDL.this.Type = typeS_
      }
      def apply(member : DFAny, token : DFAny.Token) : Value = {
        val value = token match {
          case x : DFBits.Token => if (x.width % 4 == 0) s"""x"${x.value.toHex}"""" else s""""${x.value.toBin}""""
          case x : DFUInt.Token => s"to_unsigned(${x.value}, ${member.width})"
          case x : DFSInt.Token => s"to_signed(${x.value}, ${member.width})"
          case x : DFBool.Token => if (x.value) "'1'" else "'0'"
          case x : DFEnum.Token[_] => db.Package.declarations.enums.entries(x.value).name.toString
          case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
        }
        Value(value, Type(member))
      }
      def apply(member : DFAny) : Value = member match {
        case x : DFAny.Const[_] => Value(member, member.constLB.get)
        case _ => References(member)
      }
    }
    //////////////////////////////////////////////////////////////////////////////////


    protected object Pattern {
      def apply(pattern : DFAny.Pattern[_]) : String = pattern match {
        case x : DFBits.Pattern => x.patternSet.map(p => s""""${p.toBin}"""").mkString("|")
        case x : DFUInt.Pattern => x.patternSet.map(p => csoIntervalBigInt(p)).mkString("|")
        case x : DFSInt.Pattern => x.patternSet.map(p => csoIntervalBigInt(p)).mkString("|")
        case x : DFBool.Pattern => x.patternSet.map(p => if (p) "'1'" else "'0'").mkString("|")
        case x : DFEnum.Pattern[_] => x.patternSet.map(p => db.Package.declarations.enums.entries(p).name.toString).mkString("|")
        case _ => throw new IllegalArgumentException(s"\nUnsupported pattern type for VHDL compilation: $pattern")
      }
    }

    //////////////////////////////////////////////////////////////////////////////////
    // Reference
    //////////////////////////////////////////////////////////////////////////////////
    protected abstract class Reference(val member : DFAny, val name : Name) extends Value {
      val typeS : Type = Type(member)
      def declare : String
      val sigport : Reference = this
      def assign(src : Value) : Unit = {
        if (!showValueInsteadOfName) architecture.statements.async_process.assignment(this, src)
        assignedValue = src.value
      }
      val refPipe : Int = member.pipeGet
      var maxPrevUse : Int = 0
      var maxPipeUse : Int = 0
      var assignedValue : String = ""
      val showValueInsteadOfName : Boolean = member.isAnonymous || architecture.statements.async_process.condBlock > 0
      def refName : String = name.value
      final def ref(pipe : Int) : String = {
        if (pipe > maxPipeUse) {
          for (i <- maxPipeUse+1 to pipe) {
            val sig = new architecture.declarations.signal(member, Name(s"${refName}_pipe$i"))
            architecture.statements.sync_process.assignment(sig, Value(if (i==1) refName else s"${refName}_pipe${i-1}", typeS))
          }
          maxPipeUse = pipe
        }
        if (pipe > 0) s"${refName}_pipe$pipe" else refName
      }
      lazy val value : String = if (showValueInsteadOfName && assignedValue.nonEmpty) assignedValue else ref(refPipe)
      val addRef : Unit = References.add(member, this, false)
    }
    protected object References {
      private val hashMap : HashMap[DFAny, Reference] = HashMap.empty[DFAny, Reference]
      def print() : Unit = println(hashMap.map(e => s"${e._1.name} -> ${e._2.name}").mkString("\n"))
      def apply(member : DFAny) : Reference = hashMap.getOrElse(member, throw new IllegalArgumentException(s"No reference for ${member.fullName}"))
//      def apply(dfVal : DFAny) : Reference = hashMap.getOrElse(dfVal, architecture.declarations.signal(dfVal))
      def add(member : DFAny, reference : Reference, forceUpdate : Boolean) : Unit =
        if (forceUpdate) hashMap.update(member, reference)
        else hashMap.getOrElseUpdate(member, reference)
    }
    //////////////////////////////////////////////////////////////////////////////////

    class PortsHandler(ifc : DFInterface, indent : Int) {
      private def emitPort(name : String, dir : String, typeS : String) : String =
        f"\n${delim*indent + name}%-22s : $dir%-3s $typeS"
      class port(member : DFAny.Port[_ <: DFAny,_ <: DFDir], name : Name) extends Reference(member, name) {
        ports.list += this
        val dir : String = member.dir.toString.toLowerCase()
        override def declare : String = emitPort(name.toString, dir, typeS.toString)
      }
      object port {
        def apply(dfPort : DFAny.Port[_ <: DFAny,_ <: DFDir]) : port = {
          new port(dfPort, Name(dfPort))
        }
      }
      object ports {
        val list : ListBuffer[port] = ListBuffer.empty[port]
        private val clkPorts : List[String] = ifc match {
          case x : RTComponent => x.clockList.toList.map(c => emitPort(c.name.toUpperCase, "in", "std_logic"))
          case _ => List(emitPort(clkName.value, "in", "std_logic"))
        }
        private val rstPorts : List[String] = ifc match {
          case x : RTComponent => x.resetList.toList.map(r => emitPort(r.name.toUpperCase, "in", "std_logic"))
          case _ => List(emitPort(rstName.value, "in", "std_logic"))
        }
        def portList : String =
          if (hasSyncProcess && simClkPeriodKHz.isEmpty) (clkPorts ++ rstPorts ++ list.map(p => p.declare)).mkString(";")
          else list.map(p => p.declare).mkString(";")
        private val headDelim = delim * (indent-1)
        override def toString : String = if (portList.isEmpty) "" else s"\n${headDelim}port ($portList\n$headDelim);"
      }
    }

    //////////////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////////////
    private object entity extends PortsHandler(design, 1) {
      def body : String = ports.toString
    }
    //////////////////////////////////////////////////////////////////////////////////


    //////////////////////////////////////////////////////////////////////////////////
    // Architecture
    //////////////////////////////////////////////////////////////////////////////////
    private object architecture {
      object declarations {
        class signal(member : DFAny, name : Name) extends Reference(member, name) {
          signals.list += this
          override def declare: String = if (showValueInsteadOfName) "" else f"\n${delim}signal $name%-13s : $typeS;"
        }
        object signal {
          def apply(dfVal : DFAny) : signal = new signal(dfVal, Name(dfVal))
          def apply(port : DFAny.Port[_,_]) : signal = new signal(port, Name(s"${port.owner.name}_${port.name.toUpperCase()}"))
        }
        object signals {
          val list : ListBuffer[signal] = ListBuffer.empty[signal]
          val simClkSig : String = if (simClkPeriodKHz.isDefined) f"\n${delim}signal $clkName%-13s : std_logic := '0';" else ""
          val simRstSig : String = if (simClkPeriodKHz.isDefined) f"\n${delim}signal $rstName%-13s : std_logic := '0';" else ""
          override def toString: String = simClkSig + simRstSig + list.map(s => s.declare).mkString
        }

        class alias(member : DFAny.Alias[_], name : Name) extends signal(member, name) {
          override def assign(src : Value) : Unit = {
            member.reference match {
              case DFAny.Alias.Reference.BitsWL(relWidth, relBitLow) =>
                if (member.aliasedVars.head.isInstanceOf[DFBits[_]])
                  architecture.statements.async_process.assignment(References(member.aliasedVars.head), src, relWidth, relBitLow)
                else
                  References(member.aliasedVars.head).assign(Value(member.aliasedVars.head).bits.replace(relWidth, relBitLow, src.bits))
              case DFAny.Alias.Reference.BitReverse() =>
                assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
                References(member.aliasedVars.head).assign(Value(s"bit_reverse($src)", src.typeS))
              case DFAny.Alias.Reference.Invert() =>
                assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
                References(member.aliasedVars.head).assign(Value(s"(not $src)", src.typeS))
              case DFAny.Alias.Reference.AsIs() =>
                if (member.aliasedVars.length == 1) {
                  References(member.aliasedVars.head).assign(src)
                } else {
                  var pos : Int = member.width-1
                  member.aliasedVars.foreach(a => {
                    References(a).assign(src.bits(a.width, pos - a.width+1))
                    pos = pos - a.width
                  })
                }
              case _ =>
                throw new IllegalArgumentException(s"\nUnexpected assignment to immutable previous value of ${member.fullName}")
            }
          }
          val aliasStr : String = member.reference match {
            case DFAny.Alias.Reference.BitsWL(relWidth, relBitLow) =>
              if ((relWidth == 1) && member.isInstanceOf[DFBool])
                s"${Value(member.aliasedVars.head).bits}($relBitLow)"
              else
                s"${Value(member.aliasedVars.head).bits(relWidth, relBitLow).to(Type(member))}"
            case DFAny.Alias.Reference.BitReverse() =>
              assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
              s"bit_reverse(${Value(member.aliasedVars.head)})"
            case DFAny.Alias.Reference.Invert() =>
//              assert(member.aliasedVars.head.isInstanceOf[DFBits[_]])
              s"(not ${Value(member.aliasedVars.head)})"
            case DFAny.Alias.Reference.Prev(step) =>
              val ref = References(member.aliasedVars.head).sigport
              val refName = ref.name
              val initSeq = ref.member.initLB.get
              if (step > ref.maxPrevUse) {
                for (i <- ref.maxPrevUse+1 to step) {
                  val sig = new architecture.declarations.signal(ref.member, Name(s"${refName}_prev$i"))
                  initSeq.prevInit(i-1).headOption match {
                    case Some(t) if !t.isBubble =>
                      architecture.statements.sync_process.resetStatement(sig, Value(ref.member, t))
                    case _ =>
                  }
                  architecture.statements.sync_process.assignment(sig, Value(if (i==1) s"${refName}" else s"${refName}_prev${i-1}", Type(ref)))
                }
                ref.maxPrevUse = step
              }
              s"${refName}_prev$step"
            case DFAny.Alias.Reference.Pipe(step) =>
              References(member.aliasedVars.head).ref(step)
            case DFAny.Alias.Reference.AsIs() =>
              val concat : String = member.aliasedVars.map{
                case a : DFBits[_] => Value(a)
                case a : DFBool if member.aliasedVars.length > 1 => Value(a)
                case a => Value(a).bits
              }.mkString(" & ")
              member match {
                case m : DFBits[_] => concat
                case m : DFUInt[_] => s"unsigned($concat)"
                case m : DFSInt[_] => s"signed($concat)"
                case m : DFBool => s"to_sl($concat)"
                case m : DFEnum[_] => s"${db.Package.declarations.enums(m.enum)}'VAL($concat)"
                case _ => throw new IllegalArgumentException(s"\nUnsupported type for VHDL compilation. The variable ${member.fullName} has type ${member.typeName}")
              }
          }
          override lazy val value : String = if (showValueInsteadOfName) aliasStr else ref(refPipe)
          if (!showValueInsteadOfName) architecture.statements.async_process.assignment(this, Value(aliasStr, Type(member)))
        }
        object alias {
          def apply(member : DFAny.Alias[_]) : alias = {
            if (!member.reference.isInstanceOf[DFAny.Alias.Reference.AsIs]) assert(member.aliasedVars.length == 1)
            val dst = new alias(member, Name(member.name))
            dst
          }
        }

        case class component(rtComponent: RTComponent) extends PortsHandler(rtComponent, 2) {
          rtComponent.ports.foreach(p => port(p))
          override def toString: String = s"\n${delim}component ${rtComponent.typeName.toLowerCase}${ports}\n${delim}end component;"

          components.list += this
        }
        object components {
          val list = ListBuffer.empty[component]
          override def toString: String = list.mkString("\n")
        }
        override def toString : String = s"$signals$components"
      }
      object statements {
        def func2(member : Func2Comp[_,_,_], leftReplace : Option[DFAny] = None) : Reference = {
//          println(s"$member ===> ${member.leftArg.asInstanceOf[DFAny].fullName} ${member.opString} ${member.rightArg.asInstanceOf[DFAny].fullName}")
          val leftStr = {
//            val left = Value(leftReplace.getOrElse(member.leftArg.asInstanceOf[DFAny]))
            val tag = member.leftBalancedSource.elements.head.tag.get
            if (tag.pipeStep > 0) References(tag.dfVal).ref(tag.pipeStep)
            else Value(tag.dfVal).value.applyBrackets()
          }
          val leftStrFixed = member.opString match {
            case "+" | "-" | "*" => s"resize($leftStr, ${member.width})"
            case _ => leftStr
          }
          val rightStr = {
            val tag = member.rightBalancedSource.elements.head.tag.get
            if (tag.pipeStep > 0) References(tag.dfVal).ref(tag.pipeStep)
            else Value(tag.dfVal).value.applyBrackets()
//            val right = Value(member.rightArg.asInstanceOf[DFAny])
//            val rightPipe : PipeValue = ??? //member.rightBalanceLB.get.elements.head + member.rightArg.asInstanceOf[DFAny].extraPipe
//            rightPipe match {
//              case PipeValue(w, Some(p)) if p > 0 && !member.rightArg.isInstanceOf[DFAny.Const[_]] =>
//                References(member.rightArg.asInstanceOf[DFAny]).ref(p)
//              case _ => Value(member.rightArg.asInstanceOf[DFAny]).value.applyBrackets()
//            }
          }
          val op = member.opString match {
            case "&" | "&&" => "and"
            case "|" | "||" => "or"
            case "^" => "xor"
            case "==" => "="
            case "!=" => "/="
            case "<<" => if (member.leftArg.isInstanceOf[DFSInt[_]]) "sla" else "sll"
            case ">>" => if (member.leftArg.isInstanceOf[DFSInt[_]]) "sra" else "srl"
            case others => others
          }
          val infixOpStr = op match {
            case "<" | ">" | "<=" | ">=" | "=" | "/=" => s"to_sl($leftStrFixed $op $rightStr)"
            case "sla" | "sll" | "sra" | "srl" => s"$leftStrFixed $op to_integer($rightStr)"
            case _ => s"$leftStrFixed $op $rightStr"
          }
          val result = architecture.declarations.signal(member)
          result.assign(Value(infixOpStr, Type(member)))
          result
        }

        case class component_instance(member : DFInterface) extends VHDL(member, self) {
          private def emitConnection(portName : String, signalName : String) : String =
            f"\n$delim$portName%-20s => $signalName"
          case class connection(port : DFAny.Port[_ <: DFAny,_ <: DFDir], signal : architecture.declarations.signal) {
            self.References.add(port, signal, forceUpdate = true)
            override def toString: String = emitConnection(port.name.toUpperCase, signal.name.toString) //TODO: use actual port name
          }
          object ports_map {
            lazy val list : List[connection] = member.ports.filterNot(p => p.isNotDiscovered).map(p => {
              connection(p, architecture.declarations.signal(p))
            })
            private val clkConns : List[String] = member match {
              case x : RTComponent => x.clockList.toList.map(c => emitConnection(c.name.toUpperCase, clkName.value))
              case _ => List(emitConnection(clkName.value, clkName.value))
            }
            private val rstConns : List[String] = member match {
              case x : RTComponent => x.resetList.toList.map(r => emitConnection(r.name.toUpperCase, rstName.value))
              case _ => List(emitConnection(rstName.value, rstName.value))
            }
            override def toString: String =
              if (hasSyncProcess) (clkConns ++ rstConns ++ list.map(e => e.toString)).mkString(",")
              else list.map(e => e.toString).mkString(",")
          }

          components.list += this
          ports_map.list
          override def toString: String =
            if (member.isInstanceOf[RTComponent])
              s"\n${member.name} : $entityName port map ($ports_map\n);"
            else
              s"\n${member.name} : entity work.$entityName($archName) port map ($ports_map\n);"
        }
        object components {
          val list : ListBuffer[component_instance] = ListBuffer.empty[component_instance]
          override def toString: String = list.mkString("","\n","\n")
        }

        class process(val indent : Int) {
          var condBlock : Int = 0
          def statementIndent : Int = condBlock + indent
          class variable(member : DFAny, name : Name, override val sigport : Reference) extends Reference(member, name) {
            variables.list += this
            override def assign(src : Value): Unit = {
              architecture.statements.async_process.assignment(this, src)
              assignedValue = src.value
            }
            override def declare : String = f"\n${delim}variable $name%-11s : $typeS;"

            override val refName: String = sigport.refName

            override val addRef: Unit = References.add(member, this, true)

            override lazy val value : String = name.value
          }
          object variable {
            def apply(member : DFAny, name : Name, sigport : Reference) : variable = new variable(member, name, sigport)
          }
          object variables {
            val list : ListBuffer[variable] = ListBuffer.empty[variable]
//            def fromSigPorts : String = list.map(v => assignment(v.sigport))
            def toSigPorts : Unit = list.foreach(v => if (!v.showValueInsteadOfName) assignment(v.sigport, v))
            override def toString: String = list.map(v => v.declare).mkString
          }
          class statement {
            val currentDelim : String = delim * statementIndent
            steadyStateStatements.list += this
          }
          class assignment (dst : Reference, src : Value) extends statement {
            final val op = dst match {
              case x : variable => ":="
              case _ => "<="
            }
            val dstStr : String = dst.name.value
            override def toString: String = f"\n${currentDelim + dstStr}%-22s $op $src;"
          }
          class assignment_partial (dst : Reference, src : Value, relWidth : Int, relBitLow : Int)
            extends assignment(dst, src) {
            override val dstStr: String =
              if (relWidth == 1) s"${dst.name}($relBitLow)"
              else s"${dst.name}(${relWidth+relBitLow-1} downto $relBitLow)"
          }
          object assignment {
            def apply(dst : Reference, src : Value) : assignment = new assignment(dst, src.to(dst.typeS))
            def apply(dst : Reference, src : Value, relWidth : Int, relBitLow : Int)
            : assignment_partial = new assignment_partial(dst, src, relWidth, relBitLow)
          }
          object steadyStateStatements {
            val list : ListBuffer[statement] = ListBuffer.empty[statement]
            override def toString: String = list.mkString
          }
          object ifStatement {
            case class ifBegin(condMember : DFAny) extends statement {
              override def toString: String = s"\n${currentDelim}if ${Value(condMember).value.applyBrackets()} = '1' then"
            }
            case class elseIfBegin(condMember : DFAny) extends statement {
              override def toString: String = s"\n${currentDelim}elsif ${Value(condMember).value.applyBrackets()} = '1' then"
            }
            case class elseBegin() extends statement {
              override def toString: String = s"\n${currentDelim}else"
            }
            case class ifEnd() extends statement {
              override def toString: String = s"\n${currentDelim}end if;"
            }
          }
          object caseStatement {
            case class caseBegin(expressionMember : DFAny) extends statement {
              val expression : String = {
                val ref = References(expressionMember)
                ref.typeS match {
                  case x : Type.std_logic_vector => s"$ref"
                  case x : Type.unsigned => s"to_integer($ref)"
                  case x : Type.signed => s"to_integer($ref)"
                  case x : Type.std_logic => s"$ref"
                  case x : Type.enumeration => s"$ref"
                }
              }
              override def toString: String = s"\n${currentDelim}case $expression is"
            }
            class when(pattern : DFAny.Pattern[_]) extends statement {
              override def toString: String = s"\n${currentDelim}when ${Pattern(pattern)} =>"
            }
            object when {
              def apply(pattern : DFAny.Pattern[_]) : when = new when(pattern)
            }
            case class whenOthers() extends statement {
              override def toString: String = s"\n${currentDelim}when others =>"
            }
            case class caseEnd() extends statement {
              override def toString: String = s"\n${currentDelim}end case;"
            }
          }
          case class assert(condMember : Option[DFAny], msg : Message, severity : Severity) extends statement {
            val severityStr : String = severity match {
              case Severity.Note => "note"
              case Severity.Warning => "warning"
              case Severity.Error => "error"
              case Severity.Failure => "failure"
            }
            val msgString : String = msg.value.collect {
              case x : SourceTag =>
                val convFuncStr : String = x.dfVal match {
                  case d : DFBits[_] if d.width % 8 == 0 => "to_hstring"
                  case d : DFUInt[_] if d.width % 8 == 0 => "to_hstring"
                  case _ => "to_string"
                }
                if (x.pipeStep > 0) s"$convFuncStr(${References(x.dfVal).ref(x.pipeStep)})"
                else s"$convFuncStr(${Value(x.dfVal)})"
              case x => s""""$x""""
            }.mkString(" & ")
            override def toString: String = condMember match {
              case Some(c) =>
                s"""
                   |${currentDelim}if rising_edge($clkName) then
                   |$delim${currentDelim}assert (${Value(c)} = '1') report $msgString severity $severityStr;
                   |${currentDelim}end if;""".stripMargin
              case None =>
                s"""
                   |${currentDelim}if rising_edge($clkName) then
                   |$delim${currentDelim}report $msgString severity $severityStr;
                   |${currentDelim}end if;""".stripMargin
            }
          }
          case class finish() extends statement {
            override def toString: String =
                s"""
                   |${currentDelim}if rising_edge($clkName) then
                   |$delim${currentDelim}finish(0);
                   |${currentDelim}end if;""".stripMargin
          }
        }
        object sync_process extends process(2) {
          case class resetStatement(dst : Reference, value : Value){
            resetStatements.list += this
            override def toString: String = f"\n${delim * indent + dst.name}%-22s <= $value;"
          }
          object resetStatements {
            val list : ListBuffer[resetStatement] = ListBuffer.empty[resetStatement]
            override def toString: String = list.mkString
          }
          lazy val exists : Boolean = steadyStateStatements.list.nonEmpty
          override def toString: String = if (!exists) "" else
            s"""
               |process ($clkName, $rstName)
               |begin
               |  if $rstName = '0' then$resetStatements
               |  elsif rising_edge($clkName) then$steadyStateStatements
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
        object clkgen {
          override def toString: String = simClkPeriodKHz match {
            case Some(p) => s"\n$clkName <= not $clkName after ${(1000000000L / 2 ) / p} ps;"
            case _ => ""
          }
        }
        object rstgen {
          override def toString: String = simClkPeriodKHz match {
            case Some(p) => s"\n$rstName <= '1' after 1 ps;"
            case _ => ""
          }
        }
        override def toString : String = s"$components$clkgen$rstgen$sync_process$async_process"
      }
      def body : String = {
        val statementsStr = statements.toString //Must load all statements first because they generate declarations
        val declarationsStr = declarations.toString
        s"$declarationsStr\nbegin\n$statementsStr"
      }
    }
    //////////////////////////////////////////////////////////////////////////////////

    protected def pass(dsn : DFInterface) : Unit = dsn.discoveredList.foreach {
      case x : DFAny.Port[_,_] =>
        val dstSig = entity.port(x)
        if (x.isAssigned) {
          val dstVar = architecture.statements.async_process.variable(x, Name(s"v_${dstSig.name}"), dstSig)
          if (x.maxPrevUse > 0) {
            val dstSigP1 = new architecture.declarations.signal(x, Name(s"${dstSig.name}_prev1"))
            dstSig.maxPrevUse = 1
            architecture.statements.async_process.assignment(dstVar, dstSigP1)
            if (x.initLB.get.nonEmpty)
              architecture.statements.sync_process.resetStatement(dstSigP1, Value(x, x.initLB.get.head))
            architecture.statements.sync_process.assignment(dstSigP1, dstSig)
          }
        }
      case x : DFAny.NewVar[_] =>
//        if (x.assigned) {
//        println(s"${x.fullName}, ${x.maxPrevUse}")
          val dstSig = architecture.declarations.signal(x)
          val dstVar = architecture.statements.async_process.variable(x, Name(s"v_${dstSig.name}"), dstSig)
          if (x.maxPrevUse > 0) {
            val dstSigP1 = new architecture.declarations.signal(x, Name(s"${dstSig.name}_prev1"))
            dstSig.maxPrevUse = 1
            architecture.statements.async_process.assignment(dstVar, dstSigP1)
            if (x.initLB.get.nonEmpty)
              architecture.statements.sync_process.resetStatement(dstSigP1, Value(x, x.initLB.get.head))
            //          else throw new IllegalArgumentException(s"\nUninitialized state variable ${x.fullName} may lead to deadlocks")
            architecture.statements.sync_process.assignment(dstSigP1, dstSig)
          }
//        }
//        else architecture.declarations.signal(x)
      case x : DFAny.Const[_] => //Do nothing
      case x : CompAlias =>
//        if (x.bypassAlias)
//          References.add(x, architecture.statements.func2(x.comp, Some(x.unextendedLeft)), forceUpdate = true)
//        else
          architecture.declarations.alias(x.alias)
      case x : DFAny.Alias[_] => architecture.declarations.alias(x)
      case x : RTComponent =>
        architecture.declarations.component(x)
        architecture.statements.component_instance(x)
      case x : Func2Comp[_,_,_] => architecture.statements.func2(x)
      case x : Assert => architecture.statements.async_process.assert(x.cond, x.msg, x.severity)
      case x : Finish => architecture.statements.async_process.finish()

      case x : ConditionalBlock.IfNoRetVal#DFIfBlock =>
        x match {
          case ifBlock : ConditionalBlock.IfNoRetVal#DFElseIfBlock =>
            architecture.statements.async_process.ifStatement.elseIfBegin(x.cond)
          case ifBlock : ConditionalBlock.IfNoRetVal#DFElseBlock =>
            architecture.statements.async_process.ifStatement.elseBegin()
          case ifBlock =>
            architecture.statements.async_process.ifStatement.ifBegin(x.cond)
        }
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
        if (x.isFinalBlock) architecture.statements.async_process.ifStatement.ifEnd()

      case x : ConditionalBlock.IfWithRetVal[_,_,_]#DFIfBlock =>
        x match {
          case ifBlock : ConditionalBlock.IfWithRetVal[_,_,_]#DFElseIfBlock =>
            architecture.statements.async_process.ifStatement.elseIfBegin(x.cond)
          case ifBlock : ConditionalBlock.IfWithRetVal[_,_,_]#DFElseBlock =>
            architecture.statements.async_process.ifStatement.elseBegin()
          case ifBlock =>
            architecture.statements.async_process.ifStatement.ifBegin(x.cond)
        }
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
        x match {
          case ifBlock : ConditionalBlock.IfWithRetVal[_,_,_]#DFElseBlock =>
            architecture.statements.async_process.ifStatement.ifEnd()
          case _ =>
        }
      case x : ConditionalBlock.MatchNoRetVal#DFMatchHeader[_] =>
        architecture.statements.async_process.caseStatement.caseBegin(x.matchVal)
        architecture.statements.async_process.condBlock += 1
      case x : ConditionalBlock.MatchNoRetVal#DFCase_Block[_] =>
        architecture.statements.async_process.caseStatement.whenOthers()
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
        architecture.statements.async_process.condBlock -= 1
        architecture.statements.async_process.caseStatement.caseEnd()
      case x : ConditionalBlock.MatchNoRetVal#DFCasePatternBlock[_] =>
        architecture.statements.async_process.caseStatement.when(x.pattern)
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
        if (x.isLastCase) {
          architecture.statements.async_process.caseStatement.whenOthers() //TODO consider removing default others
          architecture.statements.async_process.condBlock -= 1
          architecture.statements.async_process.caseStatement.caseEnd()
        }
      case x : ConditionalBlock.MatchWithRetVal[_,_,_]#DFMatchHeader[_] =>
        architecture.statements.async_process.caseStatement.caseBegin(x.matchVal)
        architecture.statements.async_process.condBlock += 1
      case x : ConditionalBlock.MatchWithRetVal[_,_,_]#DFCase_Block[_] =>
        architecture.statements.async_process.caseStatement.whenOthers()
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
        architecture.statements.async_process.condBlock -= 1
        architecture.statements.async_process.caseStatement.caseEnd()
      case x : ConditionalBlock.MatchWithRetVal[_,_,_]#DFCasePatternBlock[_] =>
        architecture.statements.async_process.caseStatement.when(x.pattern)
        architecture.statements.async_process.condBlock += 1
        pass(x)
        architecture.statements.async_process.condBlock -= 1
      case x : DFDesign => architecture.statements.component_instance(x)
      case x : DFAny.Connector => if (!x.toPort.owner.isInstanceOf[Func2Comp[_,_,_]]) {
        val dstSig = References(x.toPort)
        val srcSig = Value(x.fromVal)
        dstSig.assign(srcSig)
      }
      case x : DFAny.Assignment =>
        References(x.toVar).assign(Value(x.fromVal))
      case x =>
        throw new IllegalArgumentException(s"\nunsupported construct: $x")
    }

    private def body : Tuple2[String, String] = (entity.body, architecture.body)
    override def toString: String = db.toString
    final def print() : this.type = {println(toString); this}
    final def toFile(fileName : String) : this.type = {
      import java.io._
      val pw = new FileWriter(new File(fileName))
      pw.write(toString)
      pw.close()
      this
    }

    protected final lazy val hasSyncProcess : Boolean = design match {
      case x : RTComponent if x.resetList.nonEmpty || x.clockList.nonEmpty => true
      case x : DFBlock if x.hasSimMembers => true
      case _ =>
        design.isTop ||
          architecture.statements.components.list.map(e => e.hasSyncProcess)
            .foldLeft(architecture.statements.sync_process.exists)((l, r) => l || r)
    }

    val entityName : Name = if (design.isInstanceOf[RTComponent]) Name(design.typeName.toLowerCase) else {
      pass(design)
      architecture.statements.async_process.variables.toSigPorts
      val topOrElseName = if (design.isTop) design.name else design.typeName
      Name(db.addOwnerBody(topOrElseName.toLowerCase, body, this))
    }
    val archName : Name = Name(s"${entityName}_arch")
  }

  object VHDL {
    private case class DB(topName : String, inSimulation : Boolean)(implicit nameDB : NameDB) extends DSLOwnerConstruct.DB[VHDL, Tuple2[String, String]] {
      //////////////////////////////////////////////////////////////////////////////////
      // Library
      //////////////////////////////////////////////////////////////////////////////////
      private object Library {
        override def toString : String =
          s"""
             |library ieee;
             |use ieee.std_logic_1164.all;
             |use ieee.numeric_std.all;
             |use work.${topName}_pkg.all;
             |""".stripMargin
      }
      private object SimLibrary {
        override def toString: String = if (inSimulation)
          s"""
             |library std;
             |use std.env.all;
             |""".stripMargin
        else ""
      }
      //////////////////////////////////////////////////////////////////////////////////

      object HelperFunctions{
        val bitReverseFunc : String =
          s"""
             |--Taken from http://www.vlsiip.com/intel/vhdlf.html
             |function bit_reverse(s1 : std_logic_vector) return std_logic_vector;
         """.stripMargin
        val to_slFunc1 : String =
          s"""
             |function to_sl(b : boolean) return std_logic;
         """.stripMargin
        val to_slFunc2 : String =
          s"""
             |function to_sl(arg : std_logic_vector) return std_logic;
         """.stripMargin
        val to_slvFunc1 : String =
          s"""
             |function to_slv(arg : std_logic) return std_logic_vector;
         """.stripMargin
        val to_slvFunc2 : String =
          s"""
             |function to_slv(arg : unsigned) return std_logic_vector;
         """.stripMargin
        val to_slvFunc3 : String =
          s"""
             |function to_slv(arg : signed) return std_logic_vector;
         """.stripMargin

        override def toString: String = bitReverseFunc + to_slFunc1 + to_slFunc2 + to_slvFunc1 + to_slvFunc2 + to_slvFunc3
      }
      object HelperFunctionsBody{
        val bitReverseFunc : String =
          s"""
             |--Taken from http://www.vlsiip.com/intel/vhdlf.html
             |function bit_reverse(s1 : std_logic_vector) return std_logic_vector is
             |   variable rr : std_logic_vector(s1'high downto s1'low);
             |begin
             |  for ii in s1'high downto s1'low loop
             |    rr(ii) := s1(s1'high-ii);
             |  end loop;
             |  return rr;
             |end bit_reverse;
         """.stripMargin
        val to_slFunc1 : String =
          s"""
             |function to_sl(b : boolean) return std_logic is
             |begin
             |  if (b) then
             |    return '1';
             |  else
             |    return '0';
             |  end if;
             |end to_sl;
         """.stripMargin
        val to_slFunc2 : String =
          s"""
             |function to_sl(arg : std_logic_vector) return std_logic is
             |begin
             |  return arg(arg'LOW);
             |end to_sl;
         """.stripMargin
        val to_slvFunc1 : String =
          s"""
             |function to_slv(arg : std_logic) return std_logic_vector is
             |begin
             |  if (arg = '1') then
             |    return "1";
             |  else
             |    return "0";
             |  end if;
             |end to_slv;
         """.stripMargin
        val to_slvFunc2 : String =
          s"""
             |function to_slv(arg : unsigned) return std_logic_vector is
             |  variable slv : std_logic_vector(arg'length-1 downto 0);
             |begin
             |  slv := std_logic_vector(arg);
             |  return slv;
             |end to_slv;
         """.stripMargin
        val to_slvFunc3 : String =
          s"""
             |function to_slv(arg : signed) return std_logic_vector is
             |  variable slv : std_logic_vector(arg'length-1 downto 0);
             |begin
             |  slv := std_logic_vector(arg);
             |  return slv;
             |end to_slv;
         """.stripMargin

        override def toString: String = bitReverseFunc + to_slFunc1 + to_slFunc2 + to_slvFunc1 + to_slvFunc2 + to_slvFunc3
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
              def apply(entry : Enum.Entry) : enum_entry = hashMap.getOrElse(entry, {enums(entry.enumOwner); hashMap(entry)})
            }
            override def toString: String = hashMap.values.mkString("\n")
          }
          override def toString: String = s"$enums"
        }
        override def toString: String =
          s"""
             |library ieee;
             |use ieee.std_logic_1164.all;
             |use ieee.numeric_std.all;
             |
             |package $name is
             |$HelperFunctions
             |$declarations
             |end package $name;
             |
             |package body $name is
             |$HelperFunctionsBody
             |end package body $name;
           """.stripMargin
      }
      //////////////////////////////////////////////////////////////////////////////////

      def ownerToString(ownerTypeName: String, ownerBody: (String, String)): String = {
        def entity : String = s"\nentity $ownerTypeName is${ownerBody._1}\nend $ownerTypeName;"
        def architecture : String = s"\narchitecture ${ownerTypeName}_arch of $ownerTypeName is${ownerBody._2}\nend ${ownerTypeName}_arch;"
        s"$Library$SimLibrary$entity\n$architecture"
      }

      override def toString : String = s"$Package\n${super.toString}"
    }
  }
}