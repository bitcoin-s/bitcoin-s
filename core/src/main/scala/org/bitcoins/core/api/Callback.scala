package org.bitcoins.core.api

import org.bitcoins.core.util.SeqWrapper

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** A function to be called in response to an event */
trait Callback[T] {
  def apply(param: T): Future[Unit]
}

/** A function with two parameters to be called in response to an event */
trait Callback2[T1, T2] extends Callback[(T1, T2)] {
  def apply(param1: T1, param2: T2): Future[Unit]

  override def apply(param: (T1, T2)): Future[Unit] = apply(param._1, param._2)
}

/** A function with three parameters to be called in response to an event */
trait Callback3[T1, T2, T3] extends Callback[(T1, T2, T3)] {
  def apply(param1: T1, param2: T2, param3: T3): Future[Unit]

  override def apply(param: (T1, T2, T3)): Future[Unit] =
    apply(param._1, param._2, param._3)
}

object Callback {

  /** Does nothing */
  def noop[T]: T => Future[Unit] = _ => Future.unit
}

/** Manages a set of callbacks, should be used to manage execution and logging if needed */
case class CallbackHandler[C, T <: Callback[C]](
    name: String,
    override val wrapped: IndexedSeq[T])
    extends SeqWrapper[T] {

  def ++(other: CallbackHandler[C, T]): CallbackHandler[C, T] = {
    require(name == other.name,
            "Cannot combine callback handlers with different names")
    CallbackHandler(name, wrapped ++ other.wrapped)
  }

  /** Executes the callbacks synchronously, if any fail, they are recovered by recoverFunc */
  def execute(param: C, recoverFunc: Throwable => Unit = _ => ())(implicit
      ec: ExecutionContext): Future[Unit] = {
    val executeFs = wrapped.map { callback =>
      // Need to wrap in another future so they are all started at once
      // and do not block each other
      Future {
        callback(param).recover { case NonFatal(err) =>
          recoverFunc(err)
        }
      }.flatten
    }

    Future.sequence(executeFs).map(_ => ())
  }
}
