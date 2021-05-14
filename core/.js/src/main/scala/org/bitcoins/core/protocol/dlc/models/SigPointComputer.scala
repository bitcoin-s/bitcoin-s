package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.crypto.ECPublicKey

case class SigPointComputer(private val computeSigPoint: () => ECPublicKey) {
  lazy val compute: ECPublicKey = computeSigPoint()
}
