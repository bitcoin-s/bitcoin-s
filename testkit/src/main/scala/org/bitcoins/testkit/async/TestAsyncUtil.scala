package org.bitcoins.testkit.async

import org.bitcoins.asyncutil.AsyncUtil
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

abstract class TestAsyncUtil extends AsyncUtil with Serializable {

  override protected def retryUntilSatisfiedWithCounter(
      conditionF: () => Future[Boolean],
      duration: FiniteDuration,
      counter: Int,
      maxTries: Int,
      stackTrace: Array[StackTraceElement],
      mode: RetryMode)(implicit ec: ExecutionContext): Future[Unit] = {
    val retryF = super
      .retryUntilSatisfiedWithCounter(conditionF,
                                      duration,
                                      counter,
                                      maxTries,
                                      stackTrace,
                                      mode)

    TestAsyncUtil.transformRetryToTestFailure(retryF)
  }
}

object TestAsyncUtil extends TestAsyncUtil {

  /** As opposed to the AsyncUtil in the rpc project, in the testkit, we can assume that
    * TestAsyncUtil methods are being called from tests and as such, we want to trim the stack
    * trace to exclude stack elements that occur before the beginning of a test.
    * Additionally, we want to transform RpcRetryExceptions to TestFailedExceptions which
    * conveniently mention the line that called the TestAsyncUtil method.
    */
  def transformRetryToTestFailure[T](fut: Future[T])(implicit
      ec: ExecutionContext): Future[T] = {
    def transformRetry(err: Throwable): Throwable = {
      if (err.isInstanceOf[AsyncUtil.RpcRetryException]) {
        val retryErr = err.asInstanceOf[AsyncUtil.RpcRetryException]
        val relevantStackTrace = retryErr.caller.tail
          .dropWhile(elem => retryErr.internalFiles.contains(elem.getFileName))
          .takeWhile(!_.getFileName.contains("TestSuite"))
        val stackElement = relevantStackTrace.head
        val file = stackElement.getFileName
        val path = stackElement.getClassName
        val line = stackElement.getLineNumber
        val pos = org.scalactic.source.Position(file, path, line)
        val newErr = new TestFailedException({ _: StackDepthException =>
                                               Some(retryErr.message)
                                             },
                                             None,
                                             pos)
        newErr.setStackTrace(relevantStackTrace)
        newErr
      } else {
        err
      }
    }

    fut.transform({ elem: T =>
                    elem
                  },
                  transformRetry)
  }
}
