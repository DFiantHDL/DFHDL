package dfhdl.internals

trait StableEnum extends scala.reflect.Enum:
  override def hashCode: Int =
    if (this.getClass.isAnonymousClass())
      // uses both the enum class name and
      // the case ordinal to get a unique and stable hash
      (this.getClass.getName, this.ordinal).hashCode()
    else
      // uses both the enum class name and
      // the product iterator to get a unique and stable hash
      (this.getClass.getName, this.productIterator.toList).hashCode()
