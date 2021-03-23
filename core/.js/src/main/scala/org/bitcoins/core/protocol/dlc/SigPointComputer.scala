package org.bitcoins.core.protocol.dlc

import org.bitcoins.crypto.ECPublicKey

case class SigPointComputer(private val computeSigPoint: () => ECPublicKey) {
  lazy val compute: ECPublicKey = computeSigPoint()
}
