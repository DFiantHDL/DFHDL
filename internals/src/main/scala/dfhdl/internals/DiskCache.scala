package dfhdl.internals

import java.nio.file.{Paths, Files}
import java.io.File.separatorChar
class DiskCache(val cacheFolder: String):
  protected def cacheFilePath(step: String, key: Any): String =
    s"$cacheFolder${separatorChar}${step}_${key.hashString}"
  def put(step: String, key: Any, value: String): Unit =
    val file = Paths.get(cacheFilePath(step, key))
    if (!Files.exists(file)) Files.createDirectories(file)
    val writer = Files.newBufferedWriter(file)
    writer.write(value)
    writer.close()
  def get(step: String, key: Any): Option[String] =
    val file = Paths.get(cacheFilePath(step, key))
    if (Files.exists(file)) Some(Files.readString(file))
    else None
  def getOrElsePut(step: String, key: Any, value: => String): String =
    get(step, key).getOrElse {
      val v = value
      put(step, key, v)
      v
    }
end DiskCache
