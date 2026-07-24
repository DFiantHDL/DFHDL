package dfhdl.internals

import factum.{Codec, Evaluator, Output, Task, TaskListener}
import factum.store.DiskStore

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import scala.util.hashing.MurmurHash3

/** Disk cache for the DFHDL app pipeline steps.
  *
  * This is a thin compatibility shim over the Factum library
  * (https://github.com/DFiantWorks/factum): a typed, persistent, content-addressed task-graph
  * cache. The `Step` surface is kept source-compatible with the original hand-rolled
  * implementation; the engine underneath (keying, content-addressed storage, artifact
  * caching/restore, atomic writes) is Factum's.
  *
  * Kept from the original semantics:
  *   - `otherDeps` keying is bit-compatible (MurmurHash3 over the deps sequence), because some deps
  *     (e.g. `DesignArgs` values, tool objects) are not yet stable under Factum's strict `KeyHash`.
  *     Migrating steps to typed, checked keys is a follow-up.
  *   - On a cache hit the order of events is: `logCachedRun()`, then for steps with generated
  *     files: value decode, `cleanUpBeforeFileRestore(value)`, and file restore. Steps without
  *     generated files decode lazily: an upstream hit whose value is never demanded downstream is
  *     not deserialized at all.
  *   - `apply(uncached = true)` runs only this step uncached (its dependencies still use their own
  *     caching defaults) and does not write to the cache.
  */
class DiskCache(val cacheFolderStr: String):
  val cacheFolderPath =
    val folderPath = Paths.get(cacheFolderStr)
    if (!Files.exists(folderPath))
      Files.createDirectories(folderPath)
    if (folderPath.isAbsolute) folderPath
    else folderPath.toAbsolutePath.normalize()

  // steps register themselves by cache name so evaluator hooks dispatch back to them
  private val steps = ConcurrentHashMap[String, Step[?, ?]]()
  private object stepListener extends TaskListener:
    override def onCacheHit(name: String): Unit =
      steps.get(name) match
        case null => ()
        case step => step.onCacheHitHook()
    override def onBeforeFilesRestore(name: String, value: () => Any): Unit =
      steps.get(name) match
        case null => ()
        case step => step.onBeforeRestoreHook(value())

  private lazy val evaluator = Evaluator(DiskStore(cacheFolderPath), listener = stepListener)

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
    *   - `name`: The name used for cache keys (defaults to the class name)
    */
  abstract class Step[F, R](
      prevStepOrValue: Step[?, F] | (() => F),
      val hasGenFiles: Boolean = false
  )(otherDeps: => Any*) extends HasTypeName:
    protected def run(from: F): R
    protected def valueToCacheStr(value: R): String
    protected def cacheStrToValue(str: String): R
    protected def logCachedRun(): Unit = {}
    protected def runAfterValue(value: R): Unit = {}
    protected def cleanUpBeforeFileRestore(value: R): Unit = {}
    protected def genFiles(value: R): List[String] = Nil
    protected val name: String = typeName
    protected def cacheEnable: Boolean = true

    // Bit-compatible with the pre-Factum implementation: the otherDeps sequence is
    // folded with MurmurHash3 and enters the Factum action key as a plain string.
    private def otherDepsKey: String = MurmurHash3.orderedHash(otherDeps).toHexString

    // Cache-hit hooks are dispatched through the evaluator's TaskListener (see
    // stepListener above), keeping the codec pure so Factum can decode values
    // lazily: steps without generated files only deserialize when demanded.
    private[DiskCache] def onCacheHitHook(): Unit = logCachedRun()
    private[DiskCache] def onBeforeRestoreHook(value: Any): Unit =
      cleanUpBeforeFileRestore(value.asInstanceOf[R])

    private object stepCodec extends Codec[R]:
      def encode(value: R): Array[Byte] =
        valueToCacheStr(value).getBytes(StandardCharsets.UTF_8)
      def decode(bytes: Array[Byte]): R =
        cacheStrToValue(String(bytes, StandardCharsets.UTF_8))

    // register after `name` is initialized above
    DiskCache.this.steps.put(name, this)

    private def runWithFiles(from: F): (R, Vector[Output]) =
      val value = run(from)
      (value, genFiles(value).map(p => Output.File(Paths.get(p))).toVector)

    private[DiskCache] lazy val task: Task[R] =
      given Codec[R] = stepCodec
      (prevStepOrValue: @unchecked) match
        case prevStep: Step[?, F] =>
          if (hasGenFiles)
            prevStep.task.cachedWithFiles(name, extraKey = otherDepsKey)(runWithFiles)
          else prevStep.task.cached(name, extraKey = otherDepsKey)(run)
        case prevValue: (() => F) =>
          if (hasGenFiles)
            Task.pure(()).cachedWithFiles(name, extraKey = otherDepsKey)(_ =>
              runWithFiles(prevValue())
            )
          else Task.pure(()).cached(name, extraKey = otherDepsKey)(_ => run(prevValue()))
    end task

    // cached run, unless uncached is true and then only this step is run without caching
    final def apply(uncached: Boolean = !cacheEnable): R =
      val value =
        if (uncached)
          val from = (prevStepOrValue: @unchecked) match
            case prevStep: Step[?, F] => prevStep()
            case prevValue: (() => F) => prevValue()
          run(from)
        else evaluator.eval(task)
      runAfterValue(value)
      value
  end Step
end DiskCache
