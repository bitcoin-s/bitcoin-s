package org.bitcoins.core.protocol.tlv

import org.bitcoins.testkitcore.gen.LnMessageGen
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

import scala.util.Try

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

  "InitMessage" must "have serialization symmetry" in {
    forAll(LnMessageGen.initMessage) { initMessage =>
      assert(LnMessage(initMessage.bytes) == initMessage)
      assert(LnMessageFactory(InitTLV)(initMessage.bytes) == initMessage)
    }
  }

  "InitMessage" must "parse correctly" in {
    assert(Try(LnMessage(
      "001000022200000302aaa2012006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f")).isSuccess)
  }

  /** @see https://github.com/lightningnetwork/lightning-rfc/blob/master/01-messaging.md#appendix-c-message-extension */
  "InitMessage" must "pass static test vectors" in {
    assert(Try(LnMessageFactory(InitTLV)(hex"001000000000")).isSuccess)
    assert(
      Try(LnMessageFactory(InitTLV)(hex"00100000000001012a030104")).isSuccess)

    assert(Try(LnMessageFactory(InitTLV)(hex"00100000000001")).isFailure)
    assert(Try(LnMessageFactory(InitTLV)(hex"00100000000002012a")).isFailure)
    assert(
      Try(LnMessageFactory(InitTLV)(hex"001000000000010101010102")).isFailure)
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
