package DFiant.simulator

import DFiant._
import internals.LazyBox

import scala.collection.mutable

case class DFSimulator(design : DFDesign) {
  private implicit val sim : DFSimulator = this
  private val db : mutable.HashMap[DFAny, TokenStream] = mutable.HashMap.empty[DFAny, TokenStream]
  private[simulator] def addTokenStream(dfVal : DFAny, tokenStream: TokenStream) : Unit =
    db.update(dfVal, tokenStream)
  private[simulator] def getTokenStream(dfVal : DFAny) : TokenStream = db(dfVal)
  def pass(dsn : DFDesign) : Unit = {
    dsn.discoveredList.foreach {
      case x : DFAny.Const[_] => TokenStream.Const(x)
      case x : DFAny.Port[_,_] =>
      case _ =>
    }
  }
  pass(design)
}
