package ZFiant
package compiler

import DFDesign.DB.Patch

final class ExplicitConversionsOps[D <: DFDesign, S <: shapeless.HList](c : Compilable[D, S]) {
  private val designDB = c.db
  import designDB.__getset
  def explicitResizes = {
    val patchList = designDB.members.flatMap {
      case net : DFNet =>
        val toVal = net.toRef.get
        val fromVal = net.fromRef.get
        (toVal, fromVal) match {
          case (DFUInt(toWidth), DFUInt(fromWidth)) if toWidth > fromWidth => Nil
          case (DFSInt(toWidth), DFSInt(fromWidth)) if toWidth > fromWidth => Nil
          case (DFBool(toLogical), DFBool(fromLogical)) =>
            //TODO: conversion between a logical boolean and a single bit
            Nil
        }
      case func : DFAny.Func2[_,_,_,_] =>
        val leftArg = func.leftArgRef.get
        val rightArg = func.rightArgRef.get
        (leftArg, rightArg) match {
          case (DFBool(toLogical), DFBool(fromLogical)) => Nil
          case _ => Nil
        }
      case _ => Nil
    }
    ??? //c.newStage[ExplicitConversions](designDB.patch(patchList), Seq())
  }
}

trait ExplicitConversions extends Compilable.Stage