package org.bitcoins.core.protocol

import java.time.{ZoneId, ZonedDateTime}

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.BlockStamp.{BlockHash, BlockHeight, BlockTime}
import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.util.Success

class BlockStampTest extends BitcoinSUnitTest {

  it must "format and parse its string representation" in {
    val blockHeight = BlockHeight(0xc0de)
    assert(blockHeight.mkString == "49374")
    assert(BlockStamp.fromString(blockHeight.mkString) == Success(blockHeight))

    val hex = "000102030405060708090a0b0c0d0e0f00112233445566778899aabbccddeeff"
    val blockHash = BlockHash(DoubleSha256DigestBE.fromHex(hex))
    assert(blockHash.mkString == hex)
    assert(BlockStamp.fromString(blockHash.mkString) == Success(blockHash))

    val hex1 = DoubleSha256DigestBE.empty.hex
    val blockHash1 = BlockHash(DoubleSha256DigestBE.fromHex(hex1))
    assert(blockHash1.mkString == hex1)
    assert(BlockStamp.fromString(blockHash1.mkString) == Success(blockHash1))

    val time =
      ZonedDateTime.of(2089, 4, 15, 16, 25, 37, 0, ZoneId.of("UTC"))
    val blockTime = BlockTime(time)
    assert(blockTime.mkString == "2089-04-15T16:25:37Z")
    assert(BlockStamp.fromString(blockTime.mkString) == Success(blockTime))
  }

}
