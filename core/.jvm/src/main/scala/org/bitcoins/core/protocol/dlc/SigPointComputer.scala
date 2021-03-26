package org.bitcoins.core.protocol.dlc

import org.bitcoins.crypto.ECPublicKey

import scala.concurrent._
import scala.concurrent.duration.DurationInt

case class SigPointComputer(private val computeSigPoint: () => ECPublicKey) {
  private val sigPointF = Future(computeSigPoint())(ExecutionContext.global)

  lazy val compute: ECPublicKey = {
    Await.result(sigPointF, 20.seconds)
  }
}
