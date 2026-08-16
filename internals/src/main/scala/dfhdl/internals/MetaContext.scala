package dfhdl.internals

import scala.annotation.Annotation

final case class Position(
    file: String,
    lineStart: Int,
    columnStart: Int,
    lineEnd: Int,
    columnEnd: Int
) derives CanEqual:
  def shortFilePath: String = file.split("\\\\").last
  override def toString: String = s"$file:$lineStart:$columnStart - $lineEnd:$columnEnd"
  def fileUnixPath: String = file.replaceAll("\\\\", "/")
  def isUnknown: Boolean = this == Position.unknown

object Position:
  val unknown = Position("", 0, 0, 0, 0)
  def fromAbsPath(
      fileAbsPath: String,
      lineStart: Int,
      columnStart: Int,
      lineEnd: Int,
      columnEnd: Int
  ): Position = Position(getRelativePath(fileAbsPath), lineStart, columnStart, lineEnd, columnEnd)

/** Normalizes a RAW scaladoc comment (as `Symbol.docstring` returns it in macros, markers included)
  * to the cooked body the compiler plugin's `docString` yields: the `/**`/`*/` markers removed, and
  * each continuation line's leading whitespace-and-`*` margin stripped. Keeps the first line's
  * spacing verbatim (a single-line `/** My in */` cooks to " My in ").
  */
def sanitizedDocstring(raw: String): String =
  val body = raw.stripPrefix("/**").stripSuffix("*/")
  val lines = body.split("\n", -1)
  (lines.head +: lines.tail.map(_.replaceFirst("^\\s*(\\*|$)", ""))).mkString("\n")

trait MetaContext:
  def setMeta(
      nameOpt: Option[String],
      position: Position,
      doc: Option[String],
      annotations: List[Annotation],
      namespace: String
  ): this.type

  def setMetaAnon(
      position: Position
  ): this.type = setMeta(None, position, None, Nil, "")

  def setName(name: String): this.type

  def anonymize: this.type

  val nameOpt: Option[String]
  val position: Position
  final val isAnonymous: Boolean = nameOpt.isEmpty
  final val name: String =
    nameOpt.getOrElse(s"anon${this.hashString}")
end MetaContext

//annotating a function with `metaContextForward` causes the plugin to try
//and apply a named ownership to its argument as defined by the
//index  `argIdx` (considering flattened arguments blocks)
class metaContextForward(argIdx: Int) extends scala.annotation.StaticAnnotation
//annotating a function or a class with `metaContextIgnore` causes the
//plugin to ignore and not apply meta information on the given context
class metaContextIgnore extends scala.annotation.StaticAnnotation
class metaContextDelegate extends scala.annotation.StaticAnnotation
