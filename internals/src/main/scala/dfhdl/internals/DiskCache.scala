package dfhdl.internals

import java.nio.file.{Paths, Files}
import java.io.File.separatorChar
import java.io.FileWriter
import scala.collection.mutable.ListBuffer
import scala.util.hashing.MurmurHash3

class DiskCache(val cacheFolderStr: String):
  type CacheKey = Product | String
  val cacheFolderPath =
    val folderPath = Paths.get(cacheFolderStr)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    if (folderPath.isAbsolute) folderPath
    else folderPath.toAbsolutePath.normalize()
  extension (key: CacheKey)
    def hashString: String =
      key match
        case s: String  => s
        case p: Product => MurmurHash3.productHash(p).toHexString
      end match
  protected def cacheFilePath(step: String, suffix: String, key: CacheKey): String =
    cacheFolderPath.resolve(s"${step}_${key.hashString}.${suffix}").toString
  def isValid(step: String, suffix: String, key: CacheKey): Boolean =
    Files.exists(Paths.get(cacheFilePath(step, suffix, key)))
  def put(step: String, suffix: String, key: CacheKey, value: String): Unit =
    val writer = new FileWriter(cacheFilePath(step, suffix, key))
    writer.write(value)
    writer.close()
  def get(step: String, suffix: String, key: CacheKey): Option[String] =
    val file = Paths.get(cacheFilePath(step, suffix, key))
    if (Files.exists(file)) Some(Files.readString(file))
    else None
  def getOrElsePut(step: String, suffix: String, key: CacheKey)(value: => String): String =
    get(step, suffix, key).getOrElse {
      val v = value
      put(step, suffix, key, v)
      v
    }

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
    * @param key
    *   A value used to determine if the cached result is still valid
    * @param prevStepOrValue
    *   Either a previous Step that produces this step's input, or a function that provides the
    *   input
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
      key: => CacheKey,
      prevStepOrValue: Step[?, F] | (() => F)
  ) extends HasTypeName:
    protected def run(from: F): R
    protected def valueToCacheStr(value: R): String
    protected def cacheStrToValue(str: String): R
    protected def logCachedRun(): Unit = {}
    protected val name: String = typeName
    final protected lazy val keyHash: String =
      (prevStepOrValue: @unchecked) match
        case prevStep: Step[?, F] => (key, prevStep.getDataHash).hashString
        case prevValue: (() => F) => key.hashString
    private lazy val calcDataValue: R = run((prevStepOrValue: @unchecked) match
      case prevStep: Step[?, F] => prevStep()
      case prevValue: (() => F) => prevValue())
    private lazy val calcDataStr: String = valueToCacheStr(calcDataValue)
    private[Step] lazy val getDataHash: String =
      get(name, "hash", keyHash) match
        case Some(dataHash) => dataHash
        case None =>
          val dataHash = MurmurHash3.stringHash(calcDataStr).toHexString
          put(name, "hash", keyHash, dataHash)
          dataHash
    private lazy val getDataValue: R =
      get(name, "data", getDataHash) match
        case Some(dataStr) =>
          logCachedRun()
          cacheStrToValue(dataStr)
        case None =>
          put(name, "data", getDataHash, calcDataStr)
          calcDataValue
    // cached run
    def apply(): R = getDataValue
  end Step
end DiskCache
