package dfhdl.internals

import java.nio.file.{Paths, Files}
import java.io.File.separatorChar
import java.io.FileWriter
class DiskCache(val cacheFolder: String):
  protected def cacheFilePath(step: String, key: Any): String =
    val folderPath = Paths.get(cacheFolder)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    val filePathStr = s"$cacheFolder${separatorChar}${step}_${key.hashString}.cache"
    if (Paths.get(filePathStr).isAbsolute) filePathStr
    else Paths.get(filePathStr).toAbsolutePath.normalize().toString
  end cacheFilePath
  def put(step: String, key: Any, value: String): Unit =
    val writer = new FileWriter(cacheFilePath(step, key))
    writer.write(value)
    writer.close()
  def get(step: String, key: Any): Option[String] =
    val file = Paths.get(cacheFilePath(step, key))
    if (Files.exists(file)) Some(Files.readString(file))
    else None
  def getOrElsePut(step: String, key: Any)(value: => String): String =
    get(step, key).getOrElse {
      val v = value
      put(step, key, v)
      v
    }
end DiskCache
