package dfhdl.core
import dfhdl.compiler.ir
import ir.TextOut.Severity
import scala.quoted.*
import dfhdl.internals.*

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

  trait ScalaPrintsFlag
  trait ScalaAssertsFlag

  object Ops:
    // ~~~ the two questions, kept separate on purpose ~~~
    //
    // 1. "Is this DFHDL code, or plain Scala?" The `summonFrom` dispatch asks this via
    //    `case given DFC`, and it decides which `println` the USER gets: the DFHDL text-out or
    //    `scala.Predef.println`. `DFC` is the right differentiator precisely because it is NOT
    //    ambient: a design body has one (`HasDFC` gives it), plain Scala does not. The global
    //    fallback `DFCG` does not leak into this, since its givens live in `object DFCG`, the
    //    companion of the opaque SUBTYPE, which is not in `DFC`'s implicit scope.
    //
    //    A scope is the wrong question to ask here. `Scope.Function`'s given is AMBIENT, so any
    //    summon of one of its supertypes succeeds even in plain Scala code.
    //
    // 2. "Does this DFHDL scope actually grant text output?" That is this assertion, and it is
    //    NARROW: it summons `HasTextOut`, which `Function` does not have. It fires once the
    //    dispatch has already chosen DFHDL.
    //
    // `summonInline` is what defers the check to the inline expansion site (the user's call site).
    // Resolving it at the definition site would summon `Scope.Global` here in this very file.
    trait InTextOutScope
    given [S <: DFC.Scope](using
        sc: S
    )(using
        AssertGiven[
          S <:< DFC.Scope.HasTextOut,
          "Text output is not allowed here.\n`print`/`println`/`report`/`assert`/`debug`/`finish` are allowed inside a design, a domain, a process, an `initial` block, or a procedural (task) method body.\nThey are NOT allowed inside a function method body, which must remain pure."
        ]
    ): InTextOutScope with {}

    def debug(args: DFValAny*)(using DFC)(using InTextOutScope): Unit =
      TextOut(Op.Debug, Nil, args.toList)

    def finish()(using DFC)(using InTextOutScope): Unit =
      TextOut(Op.Finish, Nil, Nil)

    transparent inline def print(inline msg: Any): Unit =
      compiletime.summonFrom {
        case given ScalaPrintsFlag => scala.Predef.print(msg)
        case given DFC             =>
          compiletime.summonInline[InTextOutScope]
          textOut(Op.Print, Some(msg))(using compiletime.summonInline[DFC])
        case _ => scala.Predef.print(msg)
      }

    transparent inline def println(inline msg: Any): Unit =
      compiletime.summonFrom {
        case given ScalaPrintsFlag => scala.Predef.println(msg)
        case given DFC             =>
          compiletime.summonInline[InTextOutScope]
          textOut(Op.Println, Some(msg))(using compiletime.summonInline[DFC])
        case _ => scala.Predef.println(msg)
      }

    transparent inline def println(): Unit =
      compiletime.summonFrom {
        case given ScalaPrintsFlag => scala.Predef.println()
        case given DFC             =>
          compiletime.summonInline[InTextOutScope]
          textOut(Op.Println, None)(using compiletime.summonInline[DFC])
        case _ => scala.Predef.println()
      }

    inline def report(inline message: Any, severity: Severity = Severity.Info): Unit =
      compiletime.summonInline[InTextOutScope]
      textOut(Op.Report(severity), Some(message))(using compiletime.summonInline[DFC])

    inline def assert(
        inline assertion: Any,
        inline message: Any,
        severity: Severity
    )(using dfc: DFC): Unit =
      compiletime.summonInline[InTextOutScope]
      assertDFHDL(assertion, Some(message), severity)(using compiletime.summonInline[DFC])

    transparent inline def assert(inline assertion: Any, inline message: => Any): Unit =
      compiletime.summonFrom {
        case given ScalaAssertsFlag =>
          inline assertion match
            case assertion: Boolean => scala.Predef.assert(assertion, message)
            case _                  => compiletime.error("assert: assertion must be a Boolean")
        case given DFC =>
          compiletime.summonInline[InTextOutScope]
          assertDFHDL(assertion, Some(message), Severity.Error)(using compiletime.summonInline[DFC])
        case _ =>
          inline assertion match
            case assertion: Boolean => scala.Predef.assert(assertion, message)
            case _                  => compiletime.error("assert: assertion must be a Boolean")
      }

    transparent inline def assert(inline assertion: Any): Unit =
      compiletime.summonFrom {
        case given ScalaAssertsFlag =>
          inline assertion match
            case assertion: Boolean => scala.Predef.assert(assertion)
            case _                  => compiletime.error("assert: assertion must be a Boolean")
        case given DFC =>
          compiletime.summonInline[InTextOutScope]
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
    )(using dfc: DFC): Unit = ${ textOutMacro('op, 'msgOption)('dfc) }
    private def textOutMacro(
        op: Expr[ir.TextOut.Op],
        msgOption: Expr[Option[Any]]
    )(dfc: Expr[DFC])(using
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
      recurse(msgOption.asTerm).asExpr match
        case '{ None }       =>
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
      '{
        trydf { TextOut($op, $msgPartsExpr, $msgArgsExpr)(using $dfc) }(using
          $dfc,
          CTName(${ Expr(op.toString) })
        )
      }
    end textOutMacro
  end Ops
end TextOut
