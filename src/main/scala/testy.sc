object Async {
  abstract class AsyncReceiver[T](initReady : Boolean = false) {
    final var ready : () => Boolean = () => initReady
    def valid() : Boolean
    def data() : Either[T, String]
    object sampled {
      var valid : Boolean = false
      var data : Either[T, String] = Right("Uninitialized")
    }
    def update() : Unit = {
      if (ready() && valid() && !sampled.valid) sampled.data = data()
      sampled.valid = valid()
    }
    final def <> (sender : AsyncSender[T]) : Unit = {
      this.ready = sender.ready
      sender.data = this.data
      sender.valid = this.valid
    }
  }

  abstract class AsyncSender[T](initData : Option[T] = None) {
    def ready() : Boolean
    var valid : () => Boolean = () => initData.isDefined
    var data : () => Either[T, String] = () => initData match {
      case Some(t) => Left(t)
      case None => Right("Uninitialized")
    }
    object sampled {
      var ready : Boolean = false
    }
    final def <> (receiver: AsyncReceiver[T]) : Unit = receiver <> this
  }

}
