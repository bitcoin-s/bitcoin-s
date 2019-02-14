package org.bitcoins.util
import akka.actor.ActorSystem
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

trait AsyncUtil extends org.bitcoins.rpc.util.AsyncUtil {
  override protected def retryUntilSatisfiedWithCounter(
      conditionF: () => Future[Boolean],
      duration: FiniteDuration,
      counter: Int,
      maxTries: Int,
      stackTrace: Array[StackTraceElement])(
      implicit system: ActorSystem): Future[Unit] = {
    import system.dispatcher

    super
      .retryUntilSatisfiedWithCounter(conditionF,
                                      duration,
                                      counter,
                                      maxTries,
                                      stackTrace)
      .transform {
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
        case Success(())  => Success(())
      }
  }
}

object AsyncUtil extends AsyncUtil
