package org.bitcoins.rpc.util

import akka.actor.ActorSystem
import org.bitcoins.rpc.client.BitcoindRpcClient

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait RpcUtil extends AsyncUtil {

  def awaitServer(
      server: BitcoindRpcClient,
      duration: FiniteDuration = 1.seconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    val f = () => server.isStarted
    awaitCondition(f, duration, maxTries)
  }

  def awaitServerShutdown(
      server: BitcoindRpcClient,
      duration: FiniteDuration = 300.milliseconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Unit = {
    val f = () => !server.isStarted
    awaitCondition(f, duration, maxTries)
  }
}

object RpcUtil extends RpcUtil
