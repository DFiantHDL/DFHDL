package dfhdl.core
import dfhdl.compiler.ir
import ir.TextOut.Severity
import scala.quoted.*
object TextOut:
  def apply(
      op: ir.TextOut.Op,
      msgParts: List[String],
      msgArgs: List[DFValAny]
  )(using dfc: DFC): Unit =
    ir.TextOut(
      op = op,
      msgParts = msgParts,
      msgArgs = msgArgs.map(_.asIR.refTW[ir.TextOut]),
      ownerRef = dfc.owner.ref,
      meta = dfc.getMeta,
      tags = dfc.tags
    ).addMember

  object Op:
    object Assert:
      def apply(
          assertion: DFValOf[DFBoolOrBit],
          severity: Severity
      )(using DFC): ir.TextOut.Op =
        ir.TextOut.Op.Assert(assertion.asIR.refTW[ir.TextOut], severity)
    export ir.TextOut.Op.{Print, Println, Report, Debug, Finish}
  end Op

  object Ops:
    def debug(args: DFValAny*)(using DFC): Unit =
      TextOut(Op.Debug, Nil, args.toList)

    def finish()(using DFC): Unit =
      TextOut(Op.Finish, Nil, Nil)

    transparent inline def print(inline msg: Any): Unit =
      compiletime.summonFrom {
        case given DomainType =>
          textOut(Op.Print, Some(msg))
        case _ =>
          scala.Predef.print(msg)
      }

    transparent inline def println(inline msg: Any): Unit =
      compiletime.summonFrom {
        case given DomainType =>
          textOut(Op.Println, Some(msg))
        case _ =>
          scala.Predef.println(msg)
      }

    transparent inline def println(): Unit =
      compiletime.summonFrom {
        case given DomainType =>
          textOut(Op.Println, None)
        case _ =>
          scala.Predef.println()
      }

    inline def report(inline message: Any, severity: Severity = Severity.Info): Unit =
      textOut(Op.Report(severity), Some(message))

    inline def assert(
        inline assertion: Any,
        inline message: Any,
        severity: Severity
    ): Unit =
      assertDFHDL(assertion, Some(message), severity)(using compiletime.summonInline[DFC])

    transparent inline def assert(inline assertion: Any, inline message: => Any): Unit =
      compiletime.summonFrom {
        case given DomainType =>
          assertDFHDL(assertion, Some(message), Severity.Error)(using compiletime.summonInline[DFC])
        case _ =>
          inline assertion match
            case assertion: Boolean => scala.Predef.assert(assertion, message)
            case _                  => compiletime.error("assert: assertion must be a Boolean")
      }

    transparent inline def assert(inline assertion: Any): Unit =
      compiletime.summonFrom {
        case given DomainType =>
          assertDFHDL(assertion, None, Severity.Error)(using compiletime.summonInline[DFC])
        case _ =>
          inline assertion match
            case assertion: Boolean => scala.Predef.assert(assertion)
            case _                  => compiletime.error("assert: assertion must be a Boolean")
      }

    private inline def assertDFHDL(
        inline assertion: Any,
        inline msgOption: Option[Any],
        severity: Severity
    )(using DFC): Unit =
      val assertionDFVal = inline assertion match
        case assertion: DFValOf[DFBoolOrBit] =>
          assertion
        case assertion: Boolean =>
          DFVal.Const(DFBool, Some(assertion))
        case assertion: BitNum =>
          DFVal.Const(DFBit, Some(assertion > 0))
        case _ =>
          compiletime.error("assert: assertion must be a Boolean/BitNum or DFHDL Boolean/Bit")
      textOut(Op.Assert(assertionDFVal, severity), msgOption)
    end assertDFHDL

    private inline def textOut(
        op: ir.TextOut.Op,
        inline msgOption: Option[Any]
    ): Unit = ${ textOutMacro('op, 'msgOption) }
    private def textOutMacro(
        op: Expr[ir.TextOut.Op],
        msgOption: Expr[Option[Any]]
    )(using
        Quotes
    ): Expr[Unit] =
      import quotes.reflect.*
      import scala.quoted.FromExpr.StringFromExpr
      def recurse(t: Term): Term = t match
        case Typed(t, _)      => recurse(t)
        case Inlined(_, _, t) => recurse(t)
        case _                => t
      var msgPartsExpr: Expr[List[String]] = '{ List.empty[String] }
      var msgArgsExpr: Expr[List[DFValAny]] = '{ List.empty[DFValAny] }
      val dfc = Expr.summon[DFC].get
      recurse(msgOption.asTerm).asExpr match
        case '{ None } =>
        case '{ Some($msg) } =>
          msg match
            case '{ StringContext(${ Varargs(partsExprs) }*).s(${ Varargs(argsExprs) }*) } =>
              // applying the standard string interpolation escape rules
              val msgPartsUpdated = partsExprs.map { p =>
                Expr(
                  p.value.get
                    .replaceAll("\\\\n", "\n")
                    .replaceAll("\\\\t", "\t")
                    .replaceAll("\\\\r", "\r")
                    .replaceAll("\\\\\"", "\"")
                    .replaceAll("\\\\\\\\", "\\\\")
                )
              }
              msgPartsExpr = '{ List(${ Varargs(msgPartsUpdated) }*) }
              val argsExprsUpdated = argsExprs.map {
                case value if value.isExprOf[DFValAny] => value.asExprOf[DFValAny]
                case value => '{ DFVal.Const(DFString, Some(${ value }.toString))(using $dfc) }
              }
              msgArgsExpr = '{ List(${ Varargs(argsExprsUpdated) }*) }
            case value if value.isExprOf[DFValAny] =>
              msgArgsExpr = '{ List(${ value.asExprOf[DFValAny] }) }
              msgPartsExpr = '{ List("", "") }
            case _ =>
              msgPartsExpr = '{ List(${ msg }.toString) }
      end match
      '{ TextOut($op, $msgPartsExpr, $msgArgsExpr)(using $dfc) }
    end textOutMacro
  end Ops
end TextOut
