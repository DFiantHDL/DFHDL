package DFiant.internals

trait HasWidth {
  type Width
}
trait WidthTag[W] extends HasWidth {
  type Width = W
}
