package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{CryptoUtil, NetworkElement, Sha256Digest}
import scodec.bits.ByteVector

case class TapLeaf(leafVersion: Byte, spk: ScriptPubKey)
    extends NetworkElement {
  override val bytes: ByteVector = leafVersion +: spk.bytes
  val sha256: Sha256Digest = CryptoUtil.tapLeafHash(bytes)
}
