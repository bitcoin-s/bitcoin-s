package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.crypto.ECPublicKey

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

case class SigPointComputer(private val computeSigPoint: () => ECPublicKey) {
  private val sigPointF = Future(computeSigPoint())(ExecutionContext.global)

  lazy val compute: ECPublicKey = {
    Await.result(sigPointF, 20.seconds)
  }
}
