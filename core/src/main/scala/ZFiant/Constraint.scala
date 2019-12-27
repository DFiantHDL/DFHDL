package ZFiant

trait Constraint extends Product with Serializable {

}

sealed trait MaxelerConstraint extends Constraint
object MaxelerConstraint {
  final case object ScalarIO extends MaxelerConstraint
}
