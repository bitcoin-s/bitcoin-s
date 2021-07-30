package org.bitcoins.dlc.node

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkitcore.gen.LnMessageGen

class DLCConnectionHandlerTest extends BitcoinSAsyncTest {

  behavior of "parseIndividualMessages"

  it must "DLC Accept message that is not aligned with a tcp frame" in {
    forAllAsync(LnMessageGen.dlcAcceptMessage) { accept =>
      //split the msg at a random index to simulate a tcp frame not being aligned
      val randomIndex = scala.util.Random.nextInt().abs % accept.bytes.size
      val (firstHalf, secondHalf) = accept.bytes.splitAt(randomIndex)
      val (firstHalfParseHeaders, remainingBytes) =
        DLCConnectionHandler.parseIndividualMessages(firstHalf)
      firstHalfParseHeaders must be(empty)

      val (secondHalfParsedHeaders, _) =
        DLCConnectionHandler.parseIndividualMessages(
          remainingBytes ++ secondHalf)
      val parsedLnMessage = secondHalfParsedHeaders.head
      val parsedLnAcceptMessage =
        parsedLnMessage.asInstanceOf[LnMessage[DLCAcceptTLV]]

      parsedLnAcceptMessage.bytes must be(accept.bytes)
    }
  }

  it must "return the entire byte array if a message is not aligned to a byte frame" in {
    forAllAsync(LnMessageGen.dlcAcceptMessage) { accept =>
      // remove last byte so the message is not aligned
      val bytes = accept.bytes.dropRight(1)
      val (parsedMessages, unAlignedBytes) =
        DLCConnectionHandler.parseIndividualMessages(bytes)

      assert(parsedMessages.isEmpty)
      assert(unAlignedBytes == bytes)
    }
  }

  // todo figure out how to properly handle unknown messages
  it must "parse an unknown message" ignore {
    forAllAsync(LnMessageGen.unknownMessage) { unknown =>
      val (messages, leftover) =
        DLCConnectionHandler.parseIndividualMessages(unknown.bytes)
      assert(messages == Vector(unknown))
      assert(leftover.isEmpty)
    }
  }
}
