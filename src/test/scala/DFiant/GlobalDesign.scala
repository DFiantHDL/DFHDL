package DFiant

import DFiant.basiclib._
import DFiant.internals._

object DFGlobalLib extends DFBasicLib {
  import DFComponent.Implementation
  implicit def `evU+U`(implicit blk : DFBlock) : Implementation[`U+U`] = ifc => {}
  implicit def `evU-U`(implicit blk : DFBlock) : Implementation[`U-U`] = ifc => {}
  implicit def `evU*U`(implicit blk : DFBlock) : Implementation[`U*U`] = ifc => {}

  implicit def `evU==U`[LW, RW](implicit blk : DFBlock) : Implementation[`U==U`[LW, RW]] = ifc => {}
  implicit def `evU!=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U!=U`[LW, RW]] = ifc => {}
  implicit def `evU<U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<U`[LW, RW]] = ifc => {}
  implicit def `evU>U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>U`[LW, RW]] = ifc => {}
  implicit def `evU<=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U<=U`[LW, RW]] = ifc => {}
  implicit def `evU>=U`[LW, RW](implicit blk : DFBlock) : Implementation[`U>=U`[LW, RW]] = ifc => {}

  implicit def `evE==E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E==E`[E]] = ifc => {}
  implicit def `evE!=E`[E <: Enum](implicit blk : DFBlock) : Implementation[`E!=E`[E]] = ifc => {}
}

object GlobalDesignName extends NameIt {
  override val value: String = "???"
}

object GlobalDesign extends DFDesign()(None, DFGlobalLib, GlobalDesignName) {
  override implicit val blk = this
}
