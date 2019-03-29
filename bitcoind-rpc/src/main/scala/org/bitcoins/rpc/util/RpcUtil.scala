package org.bitcoins.rpc.util

import java.net.ServerSocket

import akka.actor.ActorSystem
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Random, Success, Try}

abstract class RpcUtil extends AsyncUtil {

  def awaitServerShutdown(
      server: BitcoindRpcClient,
      duration: FiniteDuration = 300.milliseconds,
      maxTries: Int = 50)(implicit system: ActorSystem): Future[Unit] = {
    retryUntilSatisfiedF(() => server.isStoppedF, duration, maxTries)
  }

  /**
    * Generates a random port not in use
    */
  @tailrec
  final def randomPort: Int = {
    val MAX = 65535 // max tcp port number
    val MIN = 1025 // lowest port not requiring sudo
    val port = Math.abs(Random.nextInt(MAX - MIN) + (MIN + 1))
    val attempt = Try {
      val socket = new ServerSocket(port)
      socket.close()
      socket.getLocalPort
    }

    attempt match {
      case Success(value) => value
      case Failure(_)     => randomPort
    }
  }
}

object RpcUtil extends RpcUtil
