package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{CryptoUtil, NetworkElement, Sha256Digest}
import scodec.bits.ByteVector

case class TapLeaf(leafVersion: Int, spk: ScriptPubKey) extends NetworkElement {

  override val bytes: ByteVector =
    ByteVector.fromInt(leafVersion, 1) ++ spk.bytes
  val sha256: Sha256Digest = CryptoUtil.tapLeafHash(bytes)
}
