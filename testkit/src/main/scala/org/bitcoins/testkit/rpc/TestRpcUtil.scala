package org.bitcoins.testkit.rpc

import akka.actor.ActorSystem
import org.bitcoins.testkit.async.TestAsyncUtil

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

abstract class TestRpcUtil extends org.bitcoins.rpc.util.RpcUtil {
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

    TestAsyncUtil.transformRetryToTestFailure(retryF)(system.dispatcher)
  }
}

object TestRpcUtil extends TestRpcUtil
