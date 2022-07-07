package org.bitcoins.core.protocol.blockchain

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.io.Source

class TaprootBlockTest extends BitcoinSUnitTest {

  behavior of "TaprootBlockTest"

  it must "0000000000000000000241a62189675ff8797a920890508d0a59ab9fe3ed8252" in {
    lazy val stream = Source
      .fromURL(getClass.getResource(
        "/0000000000000000000241a62189675ff8797a920890508d0a59ab9fe3ed8252.txt"))
      .mkString
    val _ = Block.fromHex(stream)
    succeed
  }
}
