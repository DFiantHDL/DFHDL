package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*
import DFVal.*
import DFiant.compiler.ir.AlwaysBlock.Sensitivity
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern

protected trait RTOwnerPrinter extends AbstractOwnerPrinter:
  type TPrinter <: RTPrinter
  val useStdSimLibrary: Boolean = true
  def fileSuffix = "vhdl"
  def packageName: String =
    s"${getSet.designDB.top.dclName}_pkg"
  def csLibrary(inSimulation: Boolean): String =
    val default =
      s"""library ieee;
         |use ieee.std_logic_1164.all;
         |use ieee.numeric_std.all;
         |use work.$packageName.all;""".stripMargin
    if (useStdSimLibrary && inSimulation)
      s"""$default
         |
         |library std;
         |use std.env.all;""".stripMargin
    else default
  def entityName(design: DFDesignBlock): String = design.dclName
  def csEntityDcl(design: DFDesignBlock): String =
    val ports = design
      .members(MemberView.Folded)
      .view
      .collect {
        case p: DFVal.Dcl if p.isPort => printer.csDFValNamed(p)
      }
      .mkString(";\n")
    val portBlock = ports.emptyOr(v => s"""
         |port (
         |${ports.indent}
         |);""".stripMargin)
    s"""entity ${entityName(design)} is$portBlock
       |end ${entityName(design)};""".stripMargin
  def archName(design: DFDesignBlock): String = s"${design.dclName}_arch"
  def csArchitectureDcl(design: DFDesignBlock): String =
    val localTypeDcls = printer.csLocalTypeDcls(design)
    val designMembers = design.members(MemberView.Folded)
    val dfValDcls =
      designMembers.view
        .collect {
          case p: DFVal.Dcl if p.isVar          => p
          case c: DFVal.Const if !c.isAnonymous => c
        }
        .map(printer.csDFValNamed)
        .emptyOr(_.mkString("", ";\n", ";"))
    val declarations = s"$localTypeDcls$dfValDcls".emptyOr(v => s"\n${v.indent}")
    val statements = csDFMembers(designMembers.filter {
      case _: DFVal.Dcl   => false
      case _: DFVal.Const => false
      case _              => true
    })
    s"""architecture ${archName(design)} of ${design.dclName} is$declarations
       |begin
       |${statements.indent}
       |end ${archName(design)};""".stripMargin
  end csArchitectureDcl
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csEntityDcl(design)}
       |
       |${csArchitectureDcl(design)}""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerLateBody(design)
    val inst = s"${design.name} : entity work.${entityName(design)}(${archName(design)})"
    if (body.isEmpty) s"$inst" else s"$inst port map (\n${body.indent}\n)"
  def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => s"if ${ifBlock.guardRef.refCodeString} then"
      case _ =>
        ifBlock.guardRef.get match
          case DFMember.Empty => s"else"
          case _              => s"elsif ${ifBlock.guardRef.refCodeString} then"
  def csDFIfEnd: String = "end if"
  def csDFCasePattern(pattern: Pattern): String = pattern match
    case Pattern.CatchAll                => "others"
    case Pattern.Singleton(token)        => printer.csDFToken(token)
    case Pattern.Alternative(list)       => list.map(csDFCasePattern).mkString(" | ")
    case Pattern.Struct(name, list)      => printer.unsupported
    case Pattern.Bind(ref, pattern)      => printer.unsupported
    case Pattern.BindSI(op, parts, refs) => printer.unsupported

  def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String =
    caseBlock.guardRef.get match
      case DFMember.Empty => // ok
      case _              => printer.unsupported
    s"when ${csDFCasePattern(caseBlock.pattern)} =>"
  def csDFMatchEnd: String = "end case"
  def csDFConditional(ch: DFConditional.Header): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString
        s"case ($csSelector)\n${csChains.indent}"
      case ih: DFConditional.DFIfHeader => csChains
  end csDFConditional
  def csAlwaysBlock(ab: AlwaysBlock): String =
    val (statements, dcls) = ab
      .members(MemberView.Folded)
      .partition {
        case dcl: DFVal.Dcl                           => false
        case const: DFVal.Const if !const.isAnonymous => false
        case _                                        => true
      }
    val body = csDFMembers(statements)
    val dcl =
      if (dcls.isEmpty) ""
      else s"\n${csDFMembers(dcls).indent}"
    val named = ab.meta.nameOpt.map(n => s"$n : ").getOrElse("")
    val senList = ab.sensitivity match
      case Sensitivity.All => " (all)"
      case Sensitivity.List(refs) =>
        if (refs.isEmpty) "" else s" ${refs.map(_.refCodeString).mkStringBrackets}"
    s"${named}process$senList$dcl\nbegin\n${body.indent}\nend process"
  end csAlwaysBlock
  def csDomainBlock(ab: DomainBlock): String = printer.unsupported
end RTOwnerPrinter
