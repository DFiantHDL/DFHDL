package dfhdl.app
import org.rogach.scallop.*

trait SingleValueConverter[T] extends ValueConverter[T]:
  def parse(arg: String): Either[String, Option[T]]
  final def parse(
      args: List[(String, List[String])]
  ): Either[String, Option[T]] =
    args match
      case (_, arg :: Nil) :: Nil => parse(arg)
      case _                      => Right(None)
  end parse

  final val argType = org.rogach.scallop.ArgType.SINGLE
end SingleValueConverter
