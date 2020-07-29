package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.testkit.core.gen.LnMessageGen
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class LnMessageTest extends BitcoinSUnitTest {

  "LnMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.lnMessage) { msg =>
      assert(LnMessage(msg.bytes) == msg)
    }
  }

  "UnknownMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.unknownMessage) { unknown =>
      assert(LnMessage(unknown.bytes) == unknown)
    }
  }

  "InitMessage" must "parse correctly as an unknown message" in {
    assert(
      LnMessage(
        "001000022200000302aaa2012006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f") == LnMessage(
        UnknownTLV(
          BigSizeUInt(16),
          ByteVector.fromValidHex(
            "00022200000302aaa2012006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f"))))
  }

  "ErrorMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.errorMessage) { error =>
      assert(LnMessage(error.bytes) == error)
    }
  }

  "PingMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.pingMessage) { ping =>
      assert(LnMessage(ping.bytes) == ping)
    }
  }

  "PongMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.pongMessage) { pong =>
      assert(LnMessage(pong.bytes) == pong)
    }
  }

  "PongMessage" must "parse correctly" in {
    assert(
      LnMessage("001300020000") == LnMessage(
        PongTLV.forIgnored(ByteVector.fromValidHex("0000"))))
  }

  "DLCOfferMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.dlcOfferMessage) { dlcOffer =>
      assert(LnMessage(dlcOffer.bytes) == dlcOffer)
    }
  }

  "DLCAcceptMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.dlcAcceptMessage) { dlcAccept =>
      assert(LnMessage(dlcAccept.bytes) == dlcAccept)
    }
  }

  "DLCSignMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.dlcSignMessage) { dlcSign =>
      assert(LnMessage(dlcSign.bytes) == dlcSign)
    }
  }
}
