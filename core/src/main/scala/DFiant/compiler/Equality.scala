package DFiant.compiler

given canEqualNothingL: CanEqual[Nothing, Any] = CanEqual.derived
given canEqualNothingR: CanEqual[Any, Nothing] = CanEqual.derived