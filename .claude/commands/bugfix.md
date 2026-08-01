# DFHDL Bug Fixing Guide

> **For diagnosing and fixing a reported DFHDL bug**, especially one where the compiler produces
> wrong or illegal HDL rather than crashing.
> Invoke this before starting work on a bug report or a "generated code is broken" issue.
> For the mechanics of writing a stage, see [/new-stage](new-stage.md). For IR shapes, [/ir-reference](ir-reference.md).

---

## The Shape of a DFHDL Bug Fix

Most DFHDL bugs are not "a stage has a typo". They are **"the IR held a shape that should never
have existed, and everything downstream faithfully processed it"**. The stage that emits the
visibly-wrong output is usually innocent. So the work has a standard order:

1. **Reproduce** and get the illegal output in front of you.
2. **Localize** the stage that first produced the bad *shape*, not the one that printed it.
3. **Decide what the rule is**, and compile the shape in every scope to find the rule's true edges.
4. **Write the check first** (elaboration or sanity), and run the suite to measure the blast radius.
5. **Fix the stage(s)** that violate it.
6. **Test at the right level**, and mind the licensing of the reporter's code.

Skipping step 4 is the most common mistake: fixing the stage first hides which other stages, doc
examples, or checked-in designs were relying on the shape.

---

## 1. Reproduce

Put the design in `lib/src/test/scala/Playground.scala` and run the pipeline. Everything is
command-line driven, so no source edit is needed:

```bash
sbtn.bat 'lib/Test/runMain Foo --nocache --log trace compile --print-backend'
```

The command line is `[design-args] [app-options] <mode> [mode-options]`, a plain subcommand
structure, so placement around the mode is load-bearing:

| Piece | Side | Examples |
|---|---|---|
| Design args + app options | *before* the mode | `--width 12`, `--nocache`, `--log trace` |
| Mode | — | `compile`, `commit`, `simulate`, `lint` |
| Mode options | *after* the mode | `--print-backend`, `-b vhdl.v2008`, `-t verilator` |

`--backend`/`-b` is a **mode** option: it goes after `compile`, not before. Full reference:
`docs/user-guide/command-line/index.md`.

Always pass `--nocache`. Re-running an unchanged design short-circuits on the on-disk cache
(`Loading committed design from cache...`) and skips the stages and the trace entirely.

**Restore the Playground when you are done.** It is a working file the user may have their own
content in. Back it up first (`cp` to the scratchpad) and restore it after each probe.

---

## 2. Localize the stage that introduced the shape

### When something throws

The `SanityCheck` that throws fires *immediately after* the offending stage. Read the
`Running stage X....` sequence and take the first failure, not the first suspect.

### When nothing throws (the harder, more common case)

A run that succeeds end-to-end and emits HDL the tool rejects (`syntax error, unexpected
TOK_ASSIGN`) fires no check, because the DB is structurally fine. The shape is merely
unrepresentable in the target language. Use the `--log trace` code dumps:

- Read the dumps **forward** and find the first printout containing the offending construct.
  Attribute it to the stage that ran just before that dump.
- Then walk **backward** through each handoff asking: *is this IR shape legal, or merely tolerated
  by everything downstream?* The earliest stage that produced an illegal shape is the culprit, and
  it is usually several stages away from the symptom.

In issue #426 the backend faithfully printed `assign` inside an `always_comb`; the connection was
planted by `ExplicitNamedVars` several stages earlier, and the *named value* it wrapped was created
two stages before that by `NamedVerilogSelection`, which was the real culprit.

### Two habits that pay off

- **Check the other backend.** Re-run with `compile --backend vhdl.v2008` (or `verilog`). If both
  are wrong in *different* ways, you have two bugs and the shared IR shape is the root cause.
  VHDL tends to fail *silently* where Verilog fails loudly: the same #426 shape emitted a signal
  assignment inside a VHDL process whose next statement read the value a delta cycle too early.
- **Grep `lib/src/test/resources/ref/` for the construct.** If no reference output contains it,
  that code path is untested, which is why the bug survived. That also tells you the fix needs a
  new reference test, not just a patched stage.

### Fix the shared base, not the subclass you happened to find

If the culprit is one of several stages sharing an abstract base (the `NamedAliases` family, the
`ComposedDFTypeReplacement` family), check whether siblings reproduce it before fixing the one you
found. #426 surfaced via `NamedVerilogSelection`, but `NamedVHDLSelection` reproduced it and
`NamedAnonMultiref` is backend-independent, so the fix belonged in the base.

---

## 3. Establish the rule, and find its real edges

Before writing any check, state the invariant as a sentence, then **compile the shape in every
scope it can appear in**. The intuitive rule is usually broader than the real one.

For #426 the intuitive rule was "a conditional expression branch is not a scope, so it cannot hold
a named value". Compiling that shape in five scopes showed four of them lower correctly:

| Conditional expression sits in | Named value in a branch | Why |
|---|---|---|
| ED domain body (**concurrent**) | **illegal** | branch is not a block; the drive becomes a connection |
| ED `process` | legal | branch lowers to a procedural block |
| RT / DF domain | legal | same |
| conditional *statement* branch (`dfType == DFUnit`) | legal | branch is a scope in its own right |

A uniform rule would have forced pointless rewrites of two stage specs that were exercising
working behavior. **Let the mechanism set the boundary, not the intuition**: here the line is
exactly the predicate `ExplicitNamedVars` uses to choose a connection over an assignment
(`isInEDDomain && !isInProcess`), because a connection is the only drive with nowhere to live.

Watch for **exemptions inside the rule** too. A named conditional *header* is legal anywhere,
because `ExplicitNamedVars` drives it through `patchChains` (an assignment per branch, never a
connection). A check written without that exemption rejects working user code. Probe each
sub-shape of the rule separately, and when the check rejects something, verify it *actually*
miscompiled before accepting the rejection.

---

## 4. Write the check first

### Elaboration check vs `SanityCheck`

| | `DB.check` (elaboration) | `SanityCheck` (stage pipeline) |
|---|---|---|
| Runs | once, right after elaboration | after every non-`NoCheckStage`, debug mode |
| Answers | is the *user's design* well-formed? | did a *stage* corrupt the DB? |
| Wire it up in | `DB.subDBCheck` / `rootDBCheck` in `DB.scala` | `SanityCheck.transformSubDB` |
| Test it in | `lib/.../ElaborationChecksSpec.scala` | the failing `<Stage>Spec` |

**The deciding question is: can a user write this by hand?** If yes it is an elaboration check,
*even when the bug you are chasing reached you through a compiler stage*. A rule only a stage can
violate (ref-table integrity, ownership ordering, an HDL-method body restriction laundered through
a helper `def`) belongs in `SanityCheck` — see the comment on `SanityCheck.hdlMethodCheck` for why
it is deliberately not on the elaboration path.

**The two are not alternatives.** A rule that a user can violate *and* a stage can violate has to
be wired into **both**, and this is easy to get wrong because the paths are completely disjoint:

- `DB.check` is a `lazy val` invoked from exactly one place, `Design.onCreateEnd`. It runs once, on
  the elaborated user design, and never again.
- `SanityCheck` has its own structural checks and **never calls `DB.check`**.

So an elaboration-only check binds the user and *nothing else*: every stage in the pipeline may
produce the forbidden shape silently, forever. That is precisely how issue #426 survived. Add the
check to `SanityCheck.transformSubDB` as well; every `StageSpec` calls `sanityCheck` directly, so
all stage tests then enforce it regardless of log level (in a normal compile `SanityCheck` only
runs at `logLevel >= DEBUG`).

Expect that wiring to fail immediately, and treat that as the point: a stage whose *own output*
trips the check cannot be repaired by a later cleanup stage, because the DB between them is
invalid. The fix has to be inside the offending stage's own patch.

Keep the check and the stage predicate textually tied: put a comment on each pointing at the other
and saying they must agree. They encode the same fact and will drift otherwise.

### Then measure the blast radius

Run the full suite with the check in and **no stage fixes yet**. The failures are the deliverable
of this step: they tell you which stages violate the rule and whether any checked-in design or doc
example depended on the shape. Report them before fixing, because "this is bad code we should fix"
and "the rule is too strict" are the user's call, not yours.

### Position-sensitive elaboration tests

`ElaborationChecksSpec` expectations embed `<file>:<line>:<col>` of the offending expression.
scalafmt reflows the test design (a braces-on-one-line block becomes multi-line), which silently
shifts those positions. Write the design in the already-normalized indented form so reformatting
does not move it, and re-check the positions after running scalafmt.

---

## 5. Fix the stage

Load [/new-stage](new-stage.md) **before editing any stage**, including a one-line change. It
carries the invariants (determinism, idempotency, printability), the patch merge table, and the
structural rules that decide whether your fix is even the right shape.

Two structural rules that come up constantly in bug fixes:

- **A new phase inside an existing stage is almost always wrong.** Either the work is a
  self-sustained, idempotent, fix-point transformation, in which case it is its own **stage**, or
  it belongs in the **same patch** as the existing work.
- **But if the work exists to keep the stage's own output legal, "separate stage" is off the table
  too** — see the `SanityCheck` point above. It must be the same patch. When that looks
  unmergeable, check whether a different `Patch.Replace` config dissolves the collision before
  concluding it is impossible: for replace-and-relocate, `ChangeRefAndRemove` plus moving the
  *replacement* keeps the two patches on different keys, where `FullReplacement` would collide
  with the `Remove` that `Patch.Move` emits per moved member.
- **A fix that can decline is not a fix.** If your transform has a case it refuses to handle, that
  case still emits the illegal shape; you have narrowed the bug, not fixed it. Make the transform
  total, even when that means carrying more with it (a conditional expression relocates as
  `header :: blocks ::: contents`). Watch for two transform sites wanting to relocate the same
  sub-tree — gate each pass to the innermost and drive the stage `@tailrec`.

### Moving members: everything must still be defined above the anchor

When a fix relocates a member, check every ref of every moved member. A dependency that lives in
the region you are moving out of, but is not moving with them, is left behind as a forward
reference, and its drive stays put — inferring a latch in the generated HDL. Neither `SanityCheck`
nor the backend complains (`orderCheck()` is commented out in `SanityCheck`), so this is silent.
Abandon the whole move in that case rather than splitting it.

`collectRelMembers` will not warn you: it recurses only into **anonymous** values, so it stops at
any dependency a sibling patch in the same pass just named. A guard written against the cone
silently passes. Check the members' `getRefs` directly.

### When the culprit really is the printer

Not every wrong-HDL bug is a wrong IR shape. Sometimes the IR is right and the backend flattens a
distinction the target language makes. `DFNet.Op.Assignment` is one IR construct, but VHDL picks
the operator from the target's **object class** (`:=` for a variable, `<=` for a signal) where
DFHDL picks it from blocking-vs-non-blocking. `VHDLPrinter.csAssignment` hard-coded `:=`, so every
assignment to a signal was illegal VHDL. Two lessons generalize:

- **Hand the backend the IR member, not a pre-digested boolean.** `csAssignment` took
  `shared: Boolean` because Verilog wanted one bit for a lint pragma, which left VHDL no way to ask
  its own question. Passing the LHS declaration lets each backend derive what it needs, and the
  next distinction costs no signature change.
- **When two print sites decide the same fact, derive both from one predicate in `analysis`.** The
  declaration keyword (`signal` / `variable` / `shared variable`) and the assignment operator are
  the same question asked twice; kept apart they drift into declaring a `signal` you then write
  with `:=`. `DFVal.Dcl.isHDLVariable` answers it once, with a comment at each call site saying so.
  This is the printer analogue of the check/predicate pairing in §4.

When you rewrite an existing predicate into a shared one, expand both forms case by case and
confirm they agree on every branch, including the ones no test reaches (a `VAR.SHARED` inside an
HDL method). A "simplification" that quietly moves an edge case is a second bug riding along.

The blast-radius step still applies, and here it reads inverted: a fully green suite with no
reference output changed is not evidence the fix is inert, it confirms the whole branch was
untested. The `ref/` grep from §2 predicts this: only the shared-variable form of `:=` appeared
anywhere under `lib/src/test/resources/ref/`, which is exactly the one form that was already right.

---

## 6. Test at the right level

**Prefer stage specs.** A `<Stage>Spec` test pins the mechanism at the layer that owns it and runs
in milliseconds. Reach for an end-to-end test only for a property no single stage owns.

**When the input shape is unwritable by hand.** If the bug's IR shape is one elaboration now
rejects, a self-contained spec input is impossible by construction. Express the *pre-naming* form
and let the stage's declared `dependencies` build the shape it consumes — that is what dependencies
are for. Say so in a comment, since it deliberately departs from the self-contained-input rule.

**Code-string assertions beat lint.** `assertNoDiff(design.getCompiledCodeString, ...)` is
deterministic and needs no external tool. `.compile.lint` under `options.LinterOptions.WError` will
fail on warnings unrelated to your fix — an `abs`-style design trips `UNUSEDSIGNAL` on the high bit
of every intermediate that is only part-selected.

**Do not copy the reporter's code into the repo.** Issue reports usually carry no license. Write a
minimal design of your own that exercises the same path; if the shape is fully covered by stage
specs, no `issues/iNNN.scala` file is needed at all.

---

## Checklist

- [ ] Reproduced with `--nocache --log trace compile --print-backend`; Playground backed up and restored
- [ ] Located the stage that **introduced** the shape, not the one that printed it
- [ ] Checked the other backend (a silent VHDL failure often shadows a loud Verilog one)
- [ ] Checked whether sibling stages sharing a base reproduce it
- [ ] Stated the invariant, and compiled the shape in **every** scope to find its real edges
- [ ] Probed each sub-shape for exemptions; confirmed anything the check rejects really miscompiles
- [ ] Check written **first**, and wired into **both** `DB.check` and `SanityCheck` if a stage can violate it too
- [ ] Full suite run with the check in and no stage fixes yet, to measure blast radius
- [ ] Blast radius reported to the user before fixing stages
- [ ] [/new-stage](new-stage.md) invoked before touching any stage
- [ ] Fix is in the offending stage's own patch, never a new phase and never a later cleanup stage
- [ ] Fix is total: no case it declines to handle
- [ ] Regression tests at stage level; no unlicensed code copied in
- [ ] Both skills updated with anything general learned

---

## Keeping This Skill Up to Date

Add a lesson here when it is about **finding and shaping a fix** — diagnosis technique, where a
rule belongs, how to scope an invariant, how to test it. Lessons about **writing a stage** (patch
mechanics, MetaDesign, IR APIs) belong in [/new-stage](new-stage.md) instead. Keep the split clean
so neither file becomes the dumping ground.
