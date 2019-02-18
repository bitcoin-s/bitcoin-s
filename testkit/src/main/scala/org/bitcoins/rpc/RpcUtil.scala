package org.bitcoins.rpc

import akka.actor.ActorSystem
import org.bitcoins.util.AsyncUtil

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

abstract class RpcUtil extends org.bitcoins.rpc.util.RpcUtil {
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

object RpcUtil extends RpcUtil