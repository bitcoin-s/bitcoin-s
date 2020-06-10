package org.bitcoins.core.api

import org.bitcoins.core.util.{FutureUtil, SeqWrapper}
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}

trait Callback[T] {
  def apply(param: T): Future[Unit]
}

trait CallbackBinary[T1, T2] extends Callback[(T1, T2)] {
  def apply(param1: T1, param2: T2): Future[Unit]

  override def apply(param: (T1, T2)): Future[Unit] = apply(param._1, param._2)
}

trait CallbackTernary[T1, T2, T3] extends Callback[(T1, T2, T3)] {
  def apply(param1: T1, param2: T2, param3: T3): Future[Unit]

  override def apply(param: (T1, T2, T3)): Future[Unit] =
    apply(param._1, param._2, param._3)
}

case class CallbackHandler[C, T <: Callback[C]](
    name: String,
    override val wrapped: IndexedSeq[T])
    extends SeqWrapper[T] {

  def ++(other: CallbackHandler[C, T]): CallbackHandler[C, T] = {
    require(name == other.name,
            "Cannot combine callback handlers with different names")
    CallbackHandler(name, wrapped ++ other.wrapped)
  }

  def execute(param: C, recoverFunc: Throwable => Unit = _ => ())(
      implicit ec: ExecutionContext): Future[Unit] = {
    wrapped
      .foldLeft(FutureUtil.unit)((acc, callback) =>
        acc.flatMap(_ =>
          callback(param).recover {
            case err: Throwable =>
              recoverFunc(err)
          }))
  }

  def execute(logger: Logger, param: C)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val recoverFunc = (err: Throwable) =>
      logger.error(s"$name Callback failed with error: ", err)
    execute(param, recoverFunc)
  }
}
