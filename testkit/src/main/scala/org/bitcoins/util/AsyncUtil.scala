package org.bitcoins.util
import akka.actor.ActorSystem
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

abstract class AsyncUtil extends org.bitcoins.rpc.util.AsyncUtil {
  override protected def retryUntilSatisfiedWithCounter(
      conditionF: () => Future[Boolean],
      duration: FiniteDuration,
      counter: Int,
      maxTries: Int,
      stackTrace: Array[StackTraceElement])(
      implicit system: ActorSystem): Future[Unit] = {
    val retryF = super
      .retryUntilSatisfiedWithCounter(conditionF,
                                      duration,
                                      counter,
                                      maxTries,
                                      stackTrace)

    AsyncUtil.transformRetryToTestFailure(retryF)(system.dispatcher)
  }
}

object AsyncUtil extends AsyncUtil {
  /**
    * As opposed to the AsyncUtil in the rpc project, in the testkit, we can assume that
    * AsyncUtil methods are being called from tests and as such, we want to trim the stack
    * trace to exclude stack elements that occur before the beginning of a test.
    * Additionally, we want to transform RpcRetryExceptions to TestFailedExceptions which
    * conveniently mention the line that called the AsyncUtil method.
    */
  def transformRetryToTestFailure[T](fut: Future[T])(implicit ec: ExecutionContext): Future[T] = {
    fut.transform {
      case Failure(RpcRetryException(message, caller)) =>
        val relevantStackTrace = caller.tail
          .dropWhile(_.getFileName == "AsyncUtil.scala")
          .takeWhile(!_.getFileName.contains("TestSuite"))
        val stackElement = relevantStackTrace.head
        val file = stackElement.getFileName
        val path = stackElement.getClassName
        val line = stackElement.getLineNumber
        val pos = org.scalactic.source.Position(file, path, line)
        val err = new TestFailedException({ _: StackDepthException =>
          Some(message)
        }, None, pos)
        err.setStackTrace(relevantStackTrace)
        Failure(err)
      case Failure(err) => Failure(err)
      case Success(elem)  => Success(elem)
    }
  }
}
