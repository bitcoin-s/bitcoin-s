package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, Factory}

sealed abstract class ChainCode extends NetworkElement
object ChainCode extends Factory[ChainCode] {
  private case class ChainCodeImpl(override val hex: String) extends ChainCode {
    require(bytes.size == 32, "ChainCode must be 32 bytes in size, got: " + bytes.size)
  }

  def fromBytes(bytes: Seq[Byte]): ChainCode = ChainCodeImpl(BitcoinSUtil.encodeHex(bytes))
}

