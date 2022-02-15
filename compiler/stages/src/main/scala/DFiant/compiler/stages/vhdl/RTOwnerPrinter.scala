package DFiant.compiler.stages.vhdl
import DFiant.compiler.printing.*
import DFiant.compiler.ir.*
import DFiant.compiler.analysis.*
import DFiant.internals.*
import DFVal.*
import DFiant.compiler.ir.AlwaysBlock.Sensitivity
import DFiant.compiler.ir.DFConditional.DFCaseBlock.Pattern

protected trait RTOwnerPrinter extends AbstractOwnerPrinter:
  private def csDFOwnerBody(owner: DFOwner, lateConstruction: Boolean): String =
    csDFMembers(owner.members(MemberView.Folded), lateConstruction)
  def csDFMembers(
      members: List[DFMember],
      lateConstruction: Boolean
  ): String =
    members.view
      // only members that match the requested construction mode
      .filter {
        case n: DFNet => n.lateConstruction == lateConstruction
        case _        => !lateConstruction
      }
      // exclude bind members
      .filter {
        case Bind(_) => false
        case _       => true
      }
      // only members the following members are accepted:
      .collect {
        // an ident placeholder (can be anonymous)
        case m @ Ident(_) => m
        // named members
        case m: DFMember.Named if !m.isAnonymous => m
        // nets
        case net: DFNet => net
        // conditional headers
        case ch: DFConditional.Header if ch.dfType == NoType => ch
        // always block
        case ab: AlwaysBlock => ab
      }
      .map(_.codeString)
      .filter(_.nonEmpty)
      .mkString("\n")
  val useStdSimLibrary: Boolean
  val packageName: String
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
  def csEntityDcl(design: DFDesignBlock): String =
    ""
  def csArchitectureDcl(design: DFDesignBlock): String =
    val localDcls = printer.csLocalTypeDcls(design)
    ""
  def csDFDesignBlockDcl(design: DFDesignBlock): String =
    s"""${csLibrary(design.inSimulation)}
       |
       |${csEntityDcl(design)}
       |
       |${csArchitectureDcl(design)}""".stripMargin
  def csDFDesignBlockInst(design: DFDesignBlock): String =
    val body = csDFOwnerBody(design, true)
    val inst = s"val ${design.name} = new ${design.dclName}"
    if (body.isEmpty) inst else s"$inst:\n${body.indent(1)}"
  def csDFIfElseStatement(ifBlock: DFConditional.DFIfElseBlock): String =
    ifBlock.prevBlockOrHeaderRef.get match
      case _: DFConditional.Header => s"if (${ifBlock.guardRef.refCodeString})"
      case _ =>
        ifBlock.guardRef.get match
          case DFMember.Empty => s"else"
          case _              => s"else if (${ifBlock.guardRef.refCodeString})"
  def csDFCasePattern(pattern: DFConditional.DFCaseBlock.Pattern): String = pattern match
    case Pattern.CatchAll => "_"
    case Pattern.Singleton(token) =>
      val csToken = printer.csDFToken(token)
      token match
        case DFEnum.Token(dt, data) => s"$csToken()"
        case _                      => csToken
    case Pattern.Alternative(list) =>
      list.map(csDFCasePattern).mkString(" | ")
    case Pattern.Struct(name, list) =>
      name + list.map(csDFCasePattern).mkStringBrackets
    case Pattern.Bind(ref, pattern) =>
      val bindStr = pattern match
        case Pattern.CatchAll => ""
        case _                => s" @ ${csDFCasePattern(pattern)}"
      s"${ref.get.name}$bindStr"
    case Pattern.BindSI(op, parts, refs) =>
      val csBinds = refs.view
        .map { r => r.get }
        .map(bindVal => s"$${${bindVal.name}: B[${bindVal.dfType.width}]}")
      val fullTerm = parts.coalesce(csBinds).mkString
      s"""$op"$fullTerm""""

  def csDFCaseStatement(caseBlock: DFConditional.DFCaseBlock): String =
    val csGuard =
      caseBlock.guardRef.get match
        case DFMember.Empty => ""
        case _              => s"if ${caseBlock.guardRef.refCodeString} "
    s"case ${csDFCasePattern(caseBlock.pattern)} ${csGuard}=>"
  def csDFConditionalBlock(cb: DFConditional.Block): String =
    val body = csDFOwnerBody(cb, false)
    val statement = cb match
      case caseBlock: DFConditional.DFCaseBlock => csDFCaseStatement(caseBlock)
      case ifBlock: DFConditional.DFIfElseBlock => csDFIfElseStatement(ifBlock)
    val indentBody =
      if (body.contains("\n")) s"\n${body.indent()}" else s" $body"
    if (body.isEmpty) cb match
      case caseBlock: DFConditional.DFCaseBlock => statement
      case ifBlock: DFConditional.DFIfElseBlock => s"$statement {}"
    else s"$statement$indentBody"
  def csDFConditional(ch: DFConditional.Header): String =
    val chain = getSet.designDB.conditionalChainTable(ch)
    val csChains = chain.map(ib => csDFConditionalBlock(ib)).mkString("\n")
    ch match
      case mh: DFConditional.DFMatchHeader =>
        val csSelector = mh.selectorRef.refCodeString.applyBrackets()
        s"$csSelector match\n${csChains.indent()}"
      case ih: DFConditional.DFIfHeader => csChains
  end csDFConditional
  def csAlwaysBlock(ab: AlwaysBlock): String =
    val body = csDFOwnerBody(ab, false)
    val named = ab.meta.nameOpt.map(n => s"val $n = ").getOrElse("")
    val senList = ab.sensitivity match
      case Sensitivity.All        => ".all"
      case Sensitivity.List(refs) => refs.map(_.refCodeString).mkStringBrackets
    s"${named}always${senList} {\n${body.indent()}\n}"
end RTOwnerPrinter
