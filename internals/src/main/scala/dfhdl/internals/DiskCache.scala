package dfhdl.internals

import java.nio.file.{Paths, Files, Path, StandardCopyOption}
import java.io.File.separatorChar
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.util.hashing.MurmurHash3

class DiskCache(val cacheFolderStr: String):
  val cacheFolderPath =
    val folderPath = Paths.get(cacheFolderStr)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    if (folderPath.isAbsolute) folderPath
    else folderPath.toAbsolutePath.normalize()
  protected def cacheFilePath(step: String, suffix: String, key: String): String =
    cacheFolderPath.resolve(s"${step}_${key}.${suffix}").toString
  def isValid(step: String, suffix: String, key: String): Boolean =
    Files.exists(Paths.get(cacheFilePath(step, suffix, key)))
  def put(step: String, suffix: String, key: String, value: String): Unit =
    val writer = new FileWriter(cacheFilePath(step, suffix, key))
    writer.write(value)
    writer.close()
  def get(step: String, suffix: String, key: String): Option[String] =
    val file = Paths.get(cacheFilePath(step, suffix, key))
    if (Files.exists(file)) Some(Files.readString(file))
    else None
  def getOrElsePut(step: String, suffix: String, key: String)(value: => String): String =
    get(step, suffix, key).getOrElse {
      val v = value
      put(step, suffix, key, v)
      v
    }
  private val base64Encoder = java.util.Base64.getEncoder

  /** A Step represents a cacheable computation step in a processing pipeline.
    *
    * Each Step takes an input of type F, produces an output of type R, and caches the result based
    * on a provided key. Steps can be chained together to form a pipeline where each step's output
    * becomes the input to the next step.
    *
    * @tparam F
    *   The input type for this step
    * @tparam R
    *   The output type for this step
    *
    * @param prevStepOrValue
    *   Either a previous Step that produces this step's input, or a function that provides the
    *   input
    * @param otherDeps
    *   Additional dependencies for this step. These are used to determine if the cached result is
    *   still valid.
    *
    * To implement a Step, you must define:
    *   - `run`: The actual computation that transforms F to R
    *   - `valueToCacheStr`: How to serialize the result to a string for caching
    *   - `cacheStrToValue`: How to deserialize the cached string back to the result type
    *
    * Optionally, you can override:
    *   - `logCachedRun`: Custom logging when a cached result is used
    *   - `name`: The name used for cache files (defaults to the class name)
    */
  abstract class Step[F, R](
      prevStepOrValue: Step[?, F] | (() => F),
      val hasGenFiles: Boolean = false
  )(otherDeps: => Any*) extends HasTypeName:
    protected def run(from: F): R
    extension (key: String | Product | IterableOnce[?])
      protected def defaultHash: String =
        val hashInt = key match
          case s: String          => MurmurHash3.stringHash(s)
          case p: Product         => MurmurHash3.productHash(p)
          case i: IterableOnce[?] => MurmurHash3.orderedHash(i)
        hashInt.toHexString
    protected def valueToCacheStr(value: R): String
    protected def cacheStrToValue(str: String): R
    protected def logCachedRun(): Unit = {}
    protected def runAfterValue(value: R): Unit = {}
    protected def cleanUpBeforeFileRestore(value: R): Unit = {}
    protected def genFiles(value: R): List[String] = Nil
    protected val name: String = typeName
    private lazy val keyHash: String =
      (prevStepOrValue: @unchecked) match
        case prevStep: Step[?, F] =>
          (otherDeps.defaultHash, prevStep.getDataHash).defaultHash
        case prevValue: (() => F) => otherDeps.defaultHash
    private lazy val calcDataValue: R = run((prevStepOrValue: @unchecked) match
      case prevStep: Step[?, F] => prevStep()
      case prevValue: (() => F) => prevValue())
    private lazy val calcDataStr: String = valueToCacheStr(calcDataValue)
    private[Step] lazy val getDataHash: String =
      get(name, "hash", keyHash) match
        case Some(dataHash) => dataHash
        case None           =>
          val dataHash = MurmurHash3.stringHash(calcDataStr).toHexString
          put(name, "hash", keyHash, dataHash)
          dataHash
    private lazy val getCachedOrCalcDataValue: R =
      get(name, "data", getDataHash) match
        case Some(dataStr) =>
          logCachedRun()
          val value = cacheStrToValue(dataStr)
          if (hasGenFiles)
            cleanUpBeforeFileRestore(value)
            restoreFiles(value)
          value
        case None =>
          put(name, "data", getDataHash, calcDataStr)
          val value = calcDataValue
          if (hasGenFiles) cacheFiles(value)
          value
    private def cacheGenFilePath(filePath: String): Path =
      val filePathBase64 = base64Encoder.encodeToString(filePath.getBytes).replace('=', '_')
      val suffix = s"${filePathBase64}.file"
      cacheFolderPath.resolve(cacheFilePath(name, suffix, getDataHash))
    private def cacheFiles(value: R): Unit =
      genFiles(value).foreach { filePath =>
        val file = Paths.get(filePath)
        Files.copy(file, cacheGenFilePath(filePath), StandardCopyOption.REPLACE_EXISTING)
      }
    private def restoreFiles(value: R): Unit =
      prevStepOrValue match
        case prevStep: Step[?, ?] if prevStep.hasGenFiles =>
          prevStep.getCachedOrCalcDataValue // will force restoring previous files
        case _ =>
      val files = genFiles(value)
      files.forall { filePath =>
        val cachedFile = cacheGenFilePath(filePath)
        if (Files.exists(cachedFile))
          val file = Paths.get(filePath)
          Files.createDirectories(file.getParent)
          // Only copy if the file doesn't exist or has a different modification time
          if (
            !Files.exists(file) ||
            !(Files.getLastModifiedTime(file) equals Files.getLastModifiedTime(cachedFile))
          )
            Files.copy(cachedFile, file, StandardCopyOption.REPLACE_EXISTING)
          true
        // at least one file cache is missing, so we need to run the value calculation and cache the files
        else
          calcDataValue
          cacheFiles(value)
          false // aborting the restoration, since now new files are cached and ready to use
        end if
      }
    end restoreFiles

    protected def cacheEnable: Boolean = true

    // cached run, unless uncached is true and then only this step is run without caching
    final def apply(uncached: Boolean = !cacheEnable): R =
      val value =
        if (uncached) calcDataValue
        else getCachedOrCalcDataValue
      runAfterValue(value)
      value
  end Step
end DiskCache
