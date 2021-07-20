package org.bitcoins.dlc.node

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.testkit.node._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.LnMessageGen

class P2PClientTest extends BitcoinSAsyncTest with CachedBitcoinSAppConfig {

  behavior of "parseIndividualMessages"

  it must "DLC Accept message that is not aligned with a tcp frame" in {
    val accept: LnMessage[DLCAcceptTLV] =
      LnMessageGen.dlcAcceptMessage.sampleSome

    //split the msg at a random index to simulate a tcp frame not being aligned
    val randomIndex = scala.util.Random.nextInt().abs % accept.bytes.size
    val (firstHalf, secondHalf) = accept.bytes.splitAt(randomIndex)
    val (firstHalfParseHeaders, remainingBytes) =
      P2PClient.parseIndividualMessages(firstHalf)
    firstHalfParseHeaders must be(empty)

    val (secondHalfParsedHeaders, _) =
      P2PClient.parseIndividualMessages(remainingBytes ++ secondHalf)
    val parsedLnMessage = secondHalfParsedHeaders.head
    val parsedLnAcceptMessage =
      parsedLnMessage.asInstanceOf[LnMessage[DLCAcceptTLV]]

    parsedLnAcceptMessage.bytes must be(accept.bytes)
  }

  it must "return the entire byte array if a message is not aligned to a byte frame" in {
    val message = LnMessageGen.knownLnMessage.sampleSome
    // remove last byte so the message is not aligned
    val bytes = message.bytes.dropRight(1)
    val (parsedMessages, unAlignedBytes) =
      P2PClient.parseIndividualMessages(bytes)

    assert(parsedMessages.isEmpty)
    assert(unAlignedBytes == bytes)
  }

  // todo figure out how to properly handle unknown messages
  it must "parse an unknown message" ignore {
    val unknown = LnMessageGen.unknownMessage.sampleSome

    val (messages, leftover) = P2PClient.parseIndividualMessages(unknown.bytes)
    assert(messages == Vector(unknown))
    assert(leftover.isEmpty)
  }
}
