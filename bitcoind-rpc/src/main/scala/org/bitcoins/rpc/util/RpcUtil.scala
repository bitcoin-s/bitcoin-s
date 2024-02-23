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
      hashBlock =
        Some(InetSocketAddress.createUnresolved("127.0.0.1", randomPort)),
      hashTx =
        Some(InetSocketAddress.createUnresolved("127.0.0.1", randomPort)),
      rawTx = Some(InetSocketAddress.createUnresolved("127.0.0.1", randomPort)),
      rawBlock =
        Some(InetSocketAddress.createUnresolved("127.0.0.1", randomPort))
    )
  }
}

object RpcUtil extends RpcUtil
