package app
import dfhdl.*

// Fixture used by `DesignArgsCLISpec` to cover a `@top` design nested inside an
// object. A nested companion cannot serve as a runnable entry point, so the
// DFHDL plugin generates a top-level object at the package level named by the
// nesting path (here `app.nestedcli_NestedCLIFoo`). The spec invokes that
// generated object's `main` reflectively.
object nestedcli:
  @top class NestedCLIFoo(val a: Int <> CONST = 7) extends EDDesign
