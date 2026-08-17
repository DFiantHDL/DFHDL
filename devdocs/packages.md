# Packages

How a declaration's Scala package becomes an HDL package: `veer_types::lsu_pkt_t` in SystemVerilog,
`work.veer_types.lsu_pkt_t` in VHDL, one file per package, instead of everything in a single global
defs file per top design.

The motivating case is translation review. `cav`'s `interface_precheck` string-compares port types
against a reference design, so a struct port emitted as `t_struct_lsu_pkt_t` never matched the gold
`veer_types::lsu_pkt_t`, and every struct-port module was blocked. Matching that form needs two
things: the declaring package must survive elaboration into the IR, and the emitted type name must be
exactly what the user wrote.

Related: [methods.md](methods.md) for the static functions and ED methods that are packaged
alongside types, and [elaboration-caching.md](elaboration-caching.md) for the `Meta` identity rules
this feature had to work within.

## 1. Terminology

| Term | Means |
|---|---|
| **namespace** | the Scala package path of a DECLARATION, carried in `ir.Meta.namespace` (`""` for the root package). Package level only: an enclosing `object` or `class` is scoping, not namespacing |
| **package** | the emitted HDL unit: an SV `package`, a VHDL `package`/`package body` pair, a `package <ns>:` section in DFHDL code |
| **placement** | the decision that maps a namespace to either the general global defs file or one dedicated package |
| **general defs file** | the pre-existing per-top globals file (`<Top>_defs.svh`, `<Top>_pkg.vhd`), which still holds everything not placed in a package |
| **packaged declaration** | one whose placement is a package: a named type, a global constant, or a global HDL method |

## 2. Where the namespace comes from

`Meta.namespace` is captured at declaration time by whichever mechanism already builds that
declaration's `Meta`. There is no annotation and nothing for a user to write.

| Declaration | Captured by |
|---|---|
| design class, design/method `def` | compiler plugin, `CommonPhase.mkNamespace` (via `genDclMeta`) → `enclosingPackageClass` |
| `case class ... extends Struct`, `enum ... extends Encoded` | derivation macro, [TypeMetaGen](../core/src/main/scala/dfhdl/core/TypeMetaGen.scala) |
| `case class ... extends Opaque(...)` | `ClassEv` (`dclNamespace`, captured in the same macro) |
| global value (`<> CONST` at global scope) | plugin `genMeta` → `DFC.namespace` |
| anything reaching neither | runtime `getClass.getPackageName` fallback in `DFStruct`/`DFEnum`/`DFOpaque` |

A name that starts with `<` (the compiler's empty-package marker) becomes `""`.

Two gates matter:

- **`DFC.getMeta` keeps the namespace only at global scope** (`ownerOption.isEmpty`). A
  design-scoped value's namespace is its design, not the Scala package its file happens to be in, so
  it must not turn into a package. `getDclMeta` is the *ungated* variant and is what `Design.Block`
  uses, because a design block's namespace is a genuine declaration property.
- **Namespaces are package-level.** `object Internal { case class Foo(...) }` in package `p` gives
  `Foo` the namespace `p`, not `p.Internal`. This was implemented both ways and deliberately settled
  on packages only: object nesting is a Scala scoping device with no HDL counterpart.

`NamedDFType` (`DFStruct`, `DFEnum`, `DFOpaque`, `DFView`) carries a full `meta: Meta` rather than a
bare name, which is what makes a type's namespace, position and doc comment available to the
printers (the doc comment is why a named type's declaration can carry its ScalaDoc).

### `Meta` identity

Adding a field to `Meta` forced its equality to be stated explicitly. `Meta` has **no `CanEqual`**;
a call site names the comparison it means:

- `sameIdentityAs`: name + namespace + annotations. Excludes `position` and `docOpt`, which are
  invisible to the code digest and so can drift while an elaboration-cache entry stays valid. This
  is what `equals`/`hashCode` implement, so member and type equality compose it implicitly.
- `sameDclAs`: all fields. "Same declaration", anchored on position: `DesignLoadKey`'s intra-run
  gate and `UniqueDesigns`' grouping need same-named designs from *different* declarations to stay
  apart.

The namespace participates in identity in both (a package clause is in the typed tree, hence in the
digest), which is what keeps `p1.Foo` and `p2.Foo` distinct types.

## 3. Placement

[Namespacing.scala](../compiler/ir/src/main/scala/dfhdl/compiler/printing/Namespacing.scala) is the
whole rule, shared by every printer and by the `DropPackages` stage:

```
isGlobalPlaced(ns, topNs) = ns.isEmpty || ns == topNs || topNs.startsWith(s"$ns.")
packageNameOf(ns, topNs)  = ns relative to topNs, remaining segments joined with `_`
```

A declaration whose namespace equals the top design's, or is an **ancestor** package of it, stays in
the general defs file. Anything else gets a package named by its namespace *relative* to the top's:

| top namespace | declaration namespace | placement |
|---|---|---|
| `veer` | `veer` | general defs file |
| `veer` | `` (root) | general defs file |
| `veer` | `veer.veer_types` | package `veer_types` |
| `mydesign` | `dfhdl.lib.crypto.aes` | package `dfhdl_lib_crypto_aes` |

The ancestor rule is what keeps a design that merely *lives* in a package from pushing its
neighbours and parents into separate files. Magnet-kind opaques (`Clk`, `Rst`) are excluded by
`Namespacing.typePlacementOf`: they are language-level and their declaring namespace is a
DFHDL-internal one.

Distinct namespaces map to distinct package names, with one residual clash: a top under `top` with
declarations in `top.x` and in a root-level `x` would produce two `x` packages. The emission detects
it and fails loudly rather than merging.

## 4. Emission

The shared machinery is in [Printer.scala](../compiler/ir/src/main/scala/dfhdl/compiler/printing/Printer.scala)
and [DFTypePrinter.scala](../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFTypePrinter.scala);
each backend supplies the syntax.

| Hook | Role |
|---|---|
| `supportPackages` | does this backend emit package files at all (false for verilog.v95/v2001) |
| `topNamespace` | `rootDB.top.dclMeta.namespace`, the reference point of every placement |
| `typePlacementOf` / `memberPlacementOf` | placement of a named type / of a global constant or method |
| `currentPackage` | the package being rendered; its own declarations and same-package references print unqualified |
| `packagedTypeEntries` | per-package type declarations, in cross-package dependency order |
| `packagedGlobalDeclEntries` | per-package constants and methods, in the same dependency order the general globals use |
| `packagedContents` | the two merged per package: what `printedDB` and `csDB` iterate |
| `csGlobalMemberQualifier` | the backend's spelling of a packaged reference's qualifier |

Three details are less obvious than they look:

- **Design-local types are packaged too.** Placement overrides design-locality: a type declared in a
  package but used by a single design is still emitted into that package, not into that design's
  declarative region. `packagedTypeEntries` collects the per-design local types for exactly this.
- **Hoisting.** A package file cannot reference a type declared inside a design, so a design-local,
  *general-placed* type that packaged content references (a global-placed struct used as a field of
  a packaged struct) is hoisted into the general defs file by `packagedHoistedTypes`, and excluded
  from the design's own local declarations.
- **Order.** Packages are emitted in topological order (a package precedes any package referencing
  it) and, in `printedDB`, ahead of the design files. Both VHDL analysis order and SV compilation
  order need this. A package name colliding with a design name is a hard error.

### Qualification, not imports

Every backend references a packaged declaration **through its package**. No backend emits a
blanket import, so two packages can hold the same simple name and no use site is ever ambiguous.

| Backend | Type / constant / call reference | Package unit |
|---|---|---|
| SystemVerilog | `pkg::Name` | `package pkg; ... endpackage` in `pkg.sv`, including the global defs header |
| VHDL | `work.pkg.Name` (types, enum literals, conversion functions, constants, method calls) | `package` + `package body` in `pkg.vhd`, `use`ing only ieee, `dfhdl_pkg` and the general package |
| DFHDL code | `<namespace>.Name` | `package <namespace>:` section |

VHDL selected names replaced an earlier `use work.<pkg>.all` design. The blanket use clause makes
two same-named packaged types an ambiguous homograph at every use site, whereas a selected name is
unambiguous by construction, and it is what allows package-scoped name uniqueness (§5). Verified to
analyze under `ghdl --std=93`, `ghdl --std=08` and `nvc`, `case` choices on selected-name enum
literals included.

One asymmetry to keep in mind when editing the VHDL printer: `pkgQualifier` prefixes a type's
**reference** forms. Anything that builds an *identifier* out of a type name (an array type
name `t_arrX1_Foo`, a conversion function name `to_Foo`) must use the simple `dfType.name` and place
the qualifier ahead of the identifier it forms (`work.pkg.to_Foo`), which is what `csConvFuncName`
is for.

## 5. Names

Two changes to naming came with the feature.

**The `t_struct_` / `t_enum_` / `t_opaque_` prefixes are gone**, in every backend. The emitted type
name is the declared name, which is the point (`veer_types::lsu_pkt_t`, not
`veer_types::t_struct_lsu_pkt_t`). Without a prefix, type and value identifiers share one HDL
namespace (and VHDL is case-insensitive), so
[UniqueNames](../compiler/stages/src/main/scala/dfhdl/compiler/stages/UniqueNames.scala) now
reserves the final global type names against every value renamer, and each design's local type names
against that design's values.

**Uniqueness is scoped per package.** A name only has to be unique within the package it is emitted
into, since every reference to it is qualified. The scopes are processed general-group-first, and
the general group's final names are then reserved for every package group: a package's content sits
*alongside* the general globals (an SV package includes the global defs header; a VHDL package uses
the general package), while two different packages never see each other unqualified. This applies to
named types, global constants, and design-local packaged types.

There is no backend flag for this. A backend without packages reaches `UniqueNames` with no packaged
declarations left to scope, because `DropPackages` ran first (§6), leaving the single general scope,
which is exactly the across-the-board uniqueness such a backend needs.

## 6. Backends without packages: `DropPackages`

verilog.v95 and v2001 have no packages, so
[DropPackages](../compiler/stages/src/main/scala/dfhdl/compiler/stages/DropPackages.scala) folds each
packaged declaration's package name into its own name and **clears its namespace**:

```
typespkg1.PkgEnum   ->  typespkg1_PkgEnum
typespkg1.pkgCalc   ->  typespkg1_pkgCalc
typespkg2.PkgWide   ->  typespkg2_PkgWide
```

Everything then lands in the single global defs header, under names that still say where they came
from and that cannot collide across packages. The mirror of `pkg::Name` is deliberate: the same
declaration stays recognizable across dialects.

Clearing the namespace is what makes the stage idempotent *and* printable: the flattened declaration
is now general-placed, so a re-run finds nothing to do, and re-elaborating the printout reconstructs
the same state. Renaming while keeping the namespace would flatten again on every pass.

It is the last entry in `BackendPrepStage`, so it sees only what survives to the emission (v95/v2001
drop structs and opaques on the way there) and its names reach `<backend>UniqueNames`, which runs
after that bundle.

The stage covers exactly what the packaged emission places, which for methods means "emitted in the
shared globals area". That decision used to live in `Printer`; it now lives in
[HDLMethodAnalysis](../compiler/ir/src/main/scala/dfhdl/compiler/analysis/HDLMethodAnalysis.scala) so
the stage and the printers cannot drift apart. A backend may only WIDEN it (VHDL globalizes a static
function read by a port declaration) by overriding `Printer.globalHDLMethods`.

Two implementation notes worth carrying: a design block cannot be renamed with a `Patch` (it is its
sub-DB's top, and its `ownerRef` is the hierarchy key), so the block is swapped in every sub-DB's
member list *and* in every refTable value resolving to it; and an anonymous global carries a
namespace even though it has no name to flatten, so it gets its namespace cleared too, or it keeps
declaring a package of its own.

## 7. Where it is pinned

| Test | Covers |
|---|---|
| `StagesSpec.MetaSpec` | namespace capture per declaration kind, the placement rules, the two `Meta` comparisons |
| `StagesSpec.PkgFixtures` | the shared fixture: general-scope globals, `typespkg1`/`typespkg2` (with a cross-package dependency), and `dualpkg1`/`dualpkg2` (same simple names in both) |
| "Namespace-derived type packages" | the same design pinned in `PrintCodeStringSpec`, `PrintVerilogCodeSpec` and `PrintVHDLCodeSpec` |
| "Same-named declarations across packages" | per-package uniqueness, in the same three specs |
| "Namespace-derived declarations flattened under verilog.v95" | `DropPackages` end-to-end, in `PrintVerilogCodeSpec` |
| `StagesSpec.DropPackagesSpec` | the stage alone, its idempotency, and its no-op for a backend with packages |
| `StagesSpec.UniqueNamesSpec` | package-scoped uniqueness of types and global constants |

## 8. Known gaps

- **A global HDL method's name is still globally unique.** Same-named design blocks are enumerated
  (`f_0`, `f_1`) by elaboration in `MutableDB.dclNameEnumeration`, which is not package-aware, and
  `UniqueDesigns.scopedDclNameKey` scopes a method by its owning design rather than by its package.
  So two packages may each declare a type `Foo`, but not each a function `calc`. Correct output,
  just not package-named.
- **A global-scope `def` called both globally and from within a design mints two IR blocks.** The
  printed declaration is deduped (`globalDeclsDeduped`, by `sameDclAs`), so it is invisible in the
  output; the real fix is improvement #11 in [elaboration-caching.md](elaboration-caching.md).
- **A global vector type whose cell type is packaged** would be declared in the general package,
  which has no visibility of that package. Pre-existing and untested; it needs the vector type
  declaration to follow its cell type's placement.
- **`DFView` / interfaces** are a work in progress and were left out of scope, though `DFView` is a
  `NamedDFType` and so already carries a namespace.
