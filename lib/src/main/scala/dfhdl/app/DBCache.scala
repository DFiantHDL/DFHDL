package dfhdl.app

import dfhdl.compiler.ir.DB
import dfhdl.internals.DiskCache

class DBCache(cachePath: String) extends DiskCache(cachePath):
  def getOrElsePutDB(step: String, key: Any)(designDB: => DB): DB =
    DB.fromJsonString(getOrElsePut(step, key) {
      val result = designDB
      result.toJsonString
    })
  end getOrElsePutDB
end DBCache
