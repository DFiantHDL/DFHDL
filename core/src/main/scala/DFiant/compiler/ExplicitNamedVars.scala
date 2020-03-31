package DFiant
package compiler

import DFDesign.DB.Patch

final class ExplicitNamedVarsOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset

  def explicitNamedVars = {
    val patchList = designDB.members.flatMap {
      case _ : DFAny.Dcl => None
      case named : DFAny if !named.isAnonymous =>
        val anon = named.anonymize
        val dsn = new MetaDesign() {
          final val newVar = DFAny.NewVar(named.dfType) setName(named.name)
          newVar.assign(addMember(anon))
        }
        Some(named -> Patch.Add(dsn, Patch.Add.Config.ReplaceWithFirst()))
      case _ => None
    }
    c.newStage[ExplicitNamedVars](designDB.patch(patchList), Seq())
  }
}

trait ExplicitNamedVars extends Compilable.Stage