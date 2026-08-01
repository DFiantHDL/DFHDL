package app
import dfhdl.*

// Fixture used by `DesignArgsCLISpec` to observe where the command line's elaboration options
// land.
//
// They have to reach the design's OWN elaboration context, because that is what reads them back:
// the design load gate consults `cacheEnable` before using the sub-design cache, and the
// top-level warning check consults `Werror`. Neither can see the app's copy, since a design's
// context is built from the elaboration options given where the design is declared. So the
// design reports what it actually saw.
//
// `tag` exists to defeat the app's own elaborate-step cache: it is part of that step's key, so
// a test that passes a fresh value forces the body (and this recording) to run.
object ElabFlagsProbe:
  var lastCacheEnable: Option[Boolean] = None
  var lastWerror: Option[Boolean] = None

class TestCLIElabFlags(val tag: String <> CONST = "") extends EDDesign:
  val o = Bit <> OUT
  o <> 1
  ElabFlagsProbe.lastCacheEnable = Some(dfc.elaborationOptions.cacheEnable)
  ElabFlagsProbe.lastWerror = Some(dfc.elaborationOptions.Werror.toBoolean)
