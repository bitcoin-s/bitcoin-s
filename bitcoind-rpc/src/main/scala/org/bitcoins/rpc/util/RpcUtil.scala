package org.bitcoins.rpc.util

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.ZmqConfig

import java.net.InetSocketAddress
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

abstract class RpcUtil extends AsyncUtil {

  def awaitServerShutdown(
      server: BitcoindRpcClient,
      duration: FiniteDuration = 300.milliseconds,
      maxTries: Int = 50)(implicit ec: ExecutionContext): Future[Unit] = {
    retryUntilSatisfiedF(() => server.isStoppedF, duration, maxTries)
  }

  /** Generates a random port not in use
    */
  final def randomPort: Int = NetworkUtil.randomPort()

  /** Genreates a zmq config with unused ports */
  def zmqConfig: ZmqConfig = {
    ZmqConfig(
      hashBlock = Some(new InetSocketAddress(randomPort)),
      hashTx = Some(new InetSocketAddress(randomPort)),
      rawTx = Some(new InetSocketAddress(randomPort)),
      rawBlock = Some(new InetSocketAddress(randomPort))
    )
  }
}

object RpcUtil extends RpcUtil
