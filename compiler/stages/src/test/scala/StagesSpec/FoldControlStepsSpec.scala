// package StagesSpec

// import dfhdl.*
// import dfhdl.compiler.stages.foldControlSteps
// // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

// // NOTE (Phase 2): inputs are written in the step-block syntax valid after `DropRTWaits` — explicit
// // `def S: Step`, relative gotos (`NextStep`/`ThisStep`), manual `waitCnt`/index `VAR.REG`s, and
// // explicit `def onEntry`/`onExit`/`fallThrough` blocks — never high-level `for`/`while`/`wait`
// // (matches `DropRTWaitsSpec`/`FlattenStepBlocksSpec`). The expected strings for the *folding* cases
// // are the hand-authored entry-folded contract; they fail against the Phase-1 no-op stub by design.
// // The no-op cases (no enclosing loop / fallThrough-blocked / already-folded / non-RT) already pass
// // against the stub, pinning the forms the stage must leave untouched.
// class FoldControlStepsSpec extends StageSpec():

//   // ── No-op: a multi-cycle wait with no enclosing control step has nothing to fold ──────────────
//   test("single wait without a loop is left unchanged") {
//     class Foo extends RTDesign:
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1: Step =
//           if (waitCnt != 49)
//             waitCnt.din := waitCnt + 1
//             ThisStep
//           else NextStep
//         end S_1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1: Step =
//          |      if (waitCnt != d"6'49")
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else NextStep
//          |    end S_1
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Fold: single loop — control `S_1` folds into wait `S_1_0`, guarded by `S_1_entered` ───────
//   test("single loop folds the control step into its wait") {
//     class Foo extends RTDesign:
//       val i       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1: Step =
//           if (i < 100)
//             def S_1_0: Step =
//               if (waitCnt != 49)
//                 waitCnt.din := waitCnt + 1
//                 ThisStep
//               else NextStep
//             end S_1_0
//             waitCnt.din := 0
//             i.din       := i + 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end S_1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val S_1_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1_0: Step =
//          |      if (waitCnt != d"6'49")
//          |        if (S_1_entered)
//          |          i.din := i + 1
//          |          S_1_entered.din := 0
//          |        end if
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else if (i < 100)
//          |        waitCnt.din := d"6'0"
//          |        S_1_entered.din := 1
//          |        ThisStep
//          |      else
//          |        finish()
//          |        NextStep
//          |      end if
//          |    end S_1_0
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Fold: nested loops — BOTH control steps fold into the innermost wait `S_1_0_0`. The whole
//   //    chain folds in one pass: inner index `j++` (guard `S_1_0_entered`) and outer index `i++`
//   //    (guard `S_1_entered`) at the guarded entry; the exit weaves inner-continue (`else if j<10`),
//   //    then the inner-exhausted path (`j := 0`) into the outer-continue/leave. ─────────────────────
//   test("nested loops fold both control steps into the innermost wait") {
//     class Foo extends RTDesign:
//       val i       = Int     <> VAR.REG init 0
//       val j       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1: Step =
//           if (i < 10)
//             def S_1_0: Step =
//               if (j < 10)
//                 def S_1_0_0: Step =
//                   if (waitCnt != 49)
//                     waitCnt.din := waitCnt + 1
//                     ThisStep
//                   else NextStep
//                 end S_1_0_0
//                 waitCnt.din := 0
//                 j.din       := j + 1
//                 ThisStep
//               else NextStep
//             end S_1_0
//             j.din := 0
//             i.din := i + 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end S_1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG init 0
//          |  val j = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val S_1_entered = Bit <> VAR.REG init 1
//          |  val S_1_0_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1_0_0: Step =
//          |      if (waitCnt != d"6'49")
//          |        if (S_1_0_entered)
//          |          j.din := j + 1
//          |          S_1_0_entered.din := 0
//          |        end if
//          |        if (S_1_entered)
//          |          i.din := i + 1
//          |          S_1_entered.din := 0
//          |        end if
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else if (j < 10)
//          |        waitCnt.din := d"6'0"
//          |        S_1_0_entered.din := 1
//          |        ThisStep
//          |      else
//          |        j.din := 0
//          |        if (i < 10)
//          |          waitCnt.din := d"6'0"
//          |          S_1_0_entered.din := 1
//          |          S_1_entered.din := 1
//          |          ThisStep
//          |        else
//          |          finish()
//          |          NextStep
//          |        end if
//          |      end if
//          |    end S_1_0_0
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Precondition: a `fallThrough` block on the wait blocks the fold (pair left unchanged) ─────
//   test("a fallThrough block blocks the fold") {
//     class Foo extends RTDesign:
//       val i       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1: Step =
//           if (i < 100)
//             def S_1_0: Step =
//               def fallThrough = waitCnt == 0
//               if (waitCnt != 49)
//                 waitCnt.din := waitCnt + 1
//                 ThisStep
//               else NextStep
//             end S_1_0
//             waitCnt.din := 0
//             i.din       := i + 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end S_1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1: Step =
//          |      if (i < 100)
//          |        def S_1_0: Step =
//          |          def fallThrough: Boolean <> VAL =
//          |            waitCnt == d"6'0"
//          |          end fallThrough
//          |          if (waitCnt != d"6'49")
//          |            waitCnt.din := waitCnt + d"6'1"
//          |            ThisStep
//          |          else NextStep
//          |        end S_1_0
//          |        waitCnt.din := d"6'0"
//          |        i.din := i + 1
//          |        ThisStep
//          |      else
//          |        finish()
//          |        NextStep
//          |      end if
//          |    end S_1
//          |end Foo""".stripMargin
//     )
//   }

//   // ── No-op: idempotency — re-running on an already-folded process changes nothing ──────────────
//   test("idempotency: an already-folded process is left unchanged") {
//     class Foo extends RTDesign:
//       val i           = Int     <> VAR.REG init 0
//       val waitCnt     = UInt(6) <> VAR.REG init 0
//       val S_1_entered = Bit     <> VAR.REG init 1
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1_0: Step =
//           if (waitCnt != 49)
//             if (S_1_entered)
//               i.din           := i + 1
//               S_1_entered.din := 0
//             waitCnt.din := waitCnt + 1
//             ThisStep
//           else if (i < 100)
//             waitCnt.din     := 0
//             S_1_entered.din := 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end S_1_0
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val S_1_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1_0: Step =
//          |      if (waitCnt != d"6'49")
//          |        if (S_1_entered)
//          |          i.din := i + 1
//          |          S_1_entered.din := 0
//          |        end if
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else if (i < 100)
//          |        waitCnt.din := d"6'0"
//          |        S_1_entered.din := 1
//          |        ThisStep
//          |      else
//          |        finish()
//          |        NextStep
//          |      end if
//          |    end S_1_0
//          |end Foo""".stripMargin
//     )
//   }

//   // ── No-op: non-RT (plain DF) design is untouched ─────────────────────────────────────────────
//   test("non-RT design is left unchanged") {
//     class Foo extends DFDesign:
//       val x = Bit <> OUT
//       val y = Bit <> IN
//       x := y
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends DFDesign:
//          |  val x = Bit <> OUT
//          |  val y = Bit <> IN
//          |  x := y
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Fold: explicit step names — control `Count` folds into wait `Count_0`, guard `Count_entered`,
//   //    with sequencing steps `Start`/`Done` around it (explicit, not relative) ────────────────────
//   test("explicit-named loop folds with the guard named after the control step") {
//     class Foo extends RTDesign:
//       val i       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def Start: Step =
//           NextStep
//         end Start
//         def Count: Step =
//           if (i < 100)
//             def Count_0: Step =
//               if (waitCnt != 49)
//                 waitCnt.din := waitCnt + 1
//                 ThisStep
//               else NextStep
//             end Count_0
//             waitCnt.din := 0
//             i.din       := i + 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end Count
//         def Done: Step =
//           NextStep
//         end Done
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val Count_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def Start: Step =
//          |      NextStep
//          |    end Start
//          |    def Count_0: Step =
//          |      if (waitCnt != d"6'49")
//          |        if (Count_entered)
//          |          i.din := i + 1
//          |          Count_entered.din := 0
//          |        end if
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else if (i < 100)
//          |        waitCnt.din := d"6'0"
//          |        Count_entered.din := 1
//          |        ThisStep
//          |      else
//          |        finish()
//          |        NextStep
//          |      end if
//          |    end Count_0
//          |    def Done: Step =
//          |      NextStep
//          |    end Done
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Fold: a nested foldable loop inside an enclosing step that carries `onEntry`/`onExit`, in a
//   //    multi-step chain (`Start` → `Count` → `Done`). The fold rewrites the inner loop (`Count_0`
//   //    folds into wait `Count_0_0`, guard `Count_0_entered`); `Count`'s `onEntry`/`onExit` — which
//   //    fire on the real transitions into/out of `Count` — are preserved untouched. ───────────────
//   test("nested fold preserves an enclosing step's onEntry/onExit in a multi-step chain") {
//     class Foo extends RTDesign:
//       val x       = Bit     <> OUT.REG init 0
//       val i       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def Start: Step =
//           x.din := 0
//           Count
//         end Start
//         def Count: Step =
//           def onEntry =
//             x.din := 1
//           def onExit =
//             x.din := 0
//           while (i != 100)
//             while (waitCnt != 49)
//               waitCnt.din := waitCnt + 1
//             waitCnt.din   := 0
//             i.din         := i + 1
//           Done
//         end Count
//         def Done: Step =
//           x.din := !x
//           Start
//         end Done
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val x = Bit <> OUT.REG init 0
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val Count_0_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def Start: Step =
//          |      x.din := 0
//          |      Count
//          |    end Start
//          |    def Count: Step =
//          |      def onEntry: Unit =
//          |        x.din := 1
//          |      end onEntry
//          |      def onExit: Unit =
//          |        x.din := 0
//          |      end onExit
//          |      def Count_0_0: Step =
//          |        if (waitCnt != d"6'49")
//          |          if (Count_0_entered)
//          |            i.din := i + 1
//          |            Count_0_entered.din := 0
//          |          end if
//          |          waitCnt.din := waitCnt + d"6'1"
//          |          ThisStep
//          |        else if (i != 100)
//          |          waitCnt.din := d"6'0"
//          |          Count_0_entered.din := 1
//          |          ThisStep
//          |        else NextStep
//          |        end if
//          |      end Count_0_0
//          |      Done
//          |    end Count
//          |    def Done: Step =
//          |      x.din := !x
//          |      Start
//          |    end Done
//          |end Foo""".stripMargin
//     )
//   }

//   // ── No-op (harvested DropRTWaitsSpec form): a pure `while` loop is a *wait* step (self-loop with
//   //    no nested wait), not a control step — nothing to fold. ─────────────────────────────────────
//   test("harvested: a pure while loop (wait step) is left unchanged") {
//     class Foo extends RTDesign:
//       val x        = Bit     <> OUT.REG
//       val waitCnt1 = UInt(8) <> VAR.REG init 0
//       process:
//         while (waitCnt1 != 149)
//           waitCnt1.din := waitCnt1 + 1
//         waitCnt1.din   := 0
//         x.din          := !x
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val x = Bit <> OUT.REG
//          |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
//          |  process:
//          |    def S_0: Step =
//          |      if (waitCnt1 != d"8'149")
//          |        waitCnt1.din := waitCnt1 + d"8'1"
//          |        ThisStep
//          |      else NextStep
//          |    end S_0
//          |    waitCnt1.din := d"8'0"
//          |    x.din := !x
//          |end Foo""".stripMargin
//     )
//   }

//   // ── No-op (harvested): two sequential pure while loops — both wait steps, neither folds ────────
//   test("harvested: multiple sequential while loops are left unchanged") {
//     class Foo extends RTDesign:
//       val x        = Bit     <> OUT.REG
//       val waitCnt1 = UInt(8) <> VAR.REG init 0
//       val waitCnt2 = UInt(8) <> VAR.REG init 0
//       process:
//         while (waitCnt1 != 149)
//           waitCnt1.din := waitCnt1 + 1
//         waitCnt1.din   := 0
//         x.din          := !x
//         while (waitCnt2 != 149)
//           waitCnt2.din := waitCnt2 + 1
//         waitCnt2.din   := 0
//         x.din          := 1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val x = Bit <> OUT.REG
//          |  val waitCnt1 = UInt(8) <> VAR.REG init d"8'0"
//          |  val waitCnt2 = UInt(8) <> VAR.REG init d"8'0"
//          |  process:
//          |    def S_0: Step =
//          |      if (waitCnt1 != d"8'149")
//          |        waitCnt1.din := waitCnt1 + d"8'1"
//          |        ThisStep
//          |      else NextStep
//          |    end S_0
//          |    waitCnt1.din := d"8'0"
//          |    x.din := !x
//          |    def S_1: Step =
//          |      if (waitCnt2 != d"8'149")
//          |        waitCnt2.din := waitCnt2 + d"8'1"
//          |        ThisStep
//          |      else NextStep
//          |    end S_1
//          |    waitCnt2.din := d"8'0"
//          |    x.din := 1
//          |end Foo""".stripMargin
//     )
//   }

//   // ── No-op (safety): the inner loop body READS the outer index `i` (`println(i, j)`). Entry-folding
//   //    would move `i.din := i + 1` to the wait's first cycle, changing `i` mid-loop and corrupting
//   //    those reads — so the `i` loop is left unfolded. Crucially, every body statement (the `println`
//   //    and any nested combinational logic) is preserved verbatim; nothing is dropped. ──────────────
//   test("a loop whose body reads its index is left unfolded (statements preserved)") {
//     class Foo extends RTDesign:
//       val i = Int <> VAR.REG
//       val j = Int <> VAR.REG
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         i.din := 0
//         def S_1: Step =
//           if (i < 3)
//             j.din := 0
//             def S_1_0: Step =
//               if (j < 3)
//                 println(s"Hello i: ${i}, j: ${j}")
//                 j.din := j + 1
//                 ThisStep
//               else NextStep
//             end S_1_0
//             i.din := i + 1
//             ThisStep
//           else NextStep
//           end if
//         end S_1
//         finish()
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val i = Int <> VAR.REG
//          |  val j = Int <> VAR.REG
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    i.din := 0
//          |    def S_1: Step =
//          |      if (i < 3)
//          |        j.din := 0
//          |        def S_1_0: Step =
//          |          if (j < 3)
//          |            println(s"Hello i: ${i}, j: ${j}")
//          |            j.din := j + 1
//          |            ThisStep
//          |          else NextStep
//          |        end S_1_0
//          |        i.din := i + 1
//          |        ThisStep
//          |      else NextStep
//          |      end if
//          |    end S_1
//          |    finish()
//          |end Foo""".stripMargin
//     )
//   }

//   // ── Fold: a wait body with an extra statement that does NOT read the folded index — the loop
//   //    folds and the body statement is preserved in the count branch (no drop). ────────────────────
//   test("fold preserves a wait-body statement that does not read the index") {
//     class Foo extends RTDesign:
//       val o       = Bit     <> OUT.REG init 0
//       val i       = Int     <> VAR.REG init 0
//       val waitCnt = UInt(6) <> VAR.REG init 0
//       process:
//         def S_0: Step =
//           NextStep
//         end S_0
//         def S_1: Step =
//           if (i < 100)
//             def S_1_0: Step =
//               if (waitCnt != 49)
//                 o.din       := !o
//                 waitCnt.din := waitCnt + 1
//                 ThisStep
//               else NextStep
//             end S_1_0
//             waitCnt.din := 0
//             i.din       := i + 1
//             ThisStep
//           else
//             finish()
//             NextStep
//         end S_1
//     end Foo
//     val top = (new Foo).foldControlSteps
//     assertCodeString(
//       top,
//       """|class Foo extends RTDesign:
//          |  val o = Bit <> OUT.REG init 0
//          |  val i = Int <> VAR.REG init 0
//          |  val waitCnt = UInt(6) <> VAR.REG init d"6'0"
//          |  val S_1_entered = Bit <> VAR.REG init 1
//          |  process:
//          |    def S_0: Step =
//          |      NextStep
//          |    end S_0
//          |    def S_1_0: Step =
//          |      if (waitCnt != d"6'49")
//          |        if (S_1_entered)
//          |          i.din := i + 1
//          |          S_1_entered.din := 0
//          |        end if
//          |        o.din := !o
//          |        waitCnt.din := waitCnt + d"6'1"
//          |        ThisStep
//          |      else if (i < 100)
//          |        waitCnt.din := d"6'0"
//          |        S_1_entered.din := 1
//          |        ThisStep
//          |      else
//          |        finish()
//          |        NextStep
//          |      end if
//          |    end S_1_0
//          |end Foo""".stripMargin
//     )
//   }

// end FoldControlStepsSpec
