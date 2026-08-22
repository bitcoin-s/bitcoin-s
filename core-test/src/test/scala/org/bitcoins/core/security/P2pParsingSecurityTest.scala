package org.bitcoins.core.security

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.{
  NetworkHeader,
  NetworkMessage,
  NetworkPayload,
  VerAckMessage,
  VersionMessage
}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/** Security reproduction tests for p2p message parsing. Each test asserts the
  * CORRECT/SAFE behavior, so every test here FAILS until the corresponding bug
  * is fixed. Do not change these assertions to match the buggy behavior.
  */
class P2pParsingSecurityTest extends BitcoinSUnitTest {

  "P2pParsing" must "handle a version message missing the optional relay byte gracefully" in {
    // Finding: a Version message without the optional `relay` byte throws
    // IndexOutOfBoundsException at
    // core/src/main/scala/org/bitcoins/core/serializers/p2p/messages/RawVersionMessageSerializer.scala:56
    // Correct behavior: accept the message with a default relay value, or fail
    // with a clean parse error -- never a raw index exception.
    // This is the version message from VersionMessageTest with the trailing
    // relay byte ("01") removed, as sent by pre-70001 peers.
    val payloadWithoutRelay = ByteVector.fromValidHex(
      "7f1101000d040000000000004ea1035d0000000000000000000000000000000000000000000000000000000000000d04000000000000000000000000000000000000000000000000fa562b93b3113e02122f5361746f7368693a302e31372e302e312f68000000")

    Try(VersionMessage.fromBytes(payloadWithoutRelay)) match {
      case Success(_) =>
        succeed // accepted with a default relay value
      case Failure(ex) =>
        // a clean parse failure is acceptable, a raw index exception is not
        assert(!ex.isInstanceOf[IndexOutOfBoundsException],
               s"Version message without relay byte must not throw " +
                 s"IndexOutOfBoundsException, got: $ex")
    }
  }

  it must "reject a network message with a corrupted checksum" in {
    // Finding: p2p message checksums are never verified, see
    // core/src/main/scala/org/bitcoins/core/p2p/NetworkPayload.scala:1674 and
    // core/src/main/scala/org/bitcoins/core/serializers/p2p/RawNetworkMessageSerializer.scala:11-20
    // Correct behavior: a message whose checksum does not match
    // doubleSHA256(payload) must be rejected.
    // This is the version message from the bitcoin wiki used in
    // NetworkMessageTest, with the last checksum nibble flipped (32 -> 33).
    val corruptedChecksumHex = {
      "f9beb4d976657273696f6e000000000065000000358d4933" +
        "62EA0000010000000000000011B2D05000000000010000000000000000000000000000000000FFFF000000000000010000000000000000000000000000000000FFFF0000000000003B2EB35D8CE617650F2F5361746F7368693A302E372E322FC03E0300" +
        "00"
    }.toLowerCase

    Try(NetworkMessage.fromHex(corruptedChecksumHex)) match {
      case Success(msg) =>
        fail(
          s"Network message with a corrupted checksum must be rejected, but parsed to: $msg")
      case Failure(_) =>
        succeed
    }
  }

  it must "produce a clean parse error for an unknown command name" in {
    // Finding: an unknown command string throws NoSuchElementException at
    // core/src/main/scala/org/bitcoins/core/p2p/NetworkPayload.scala:1674
    // Correct behavior: an unknown command must produce a clean, descriptive
    // parse error (or a handled unknown-payload type), not a raw
    // NoSuchElementException from a Map lookup.
    val header =
      NetworkHeader(RegTest,
                    "madeup",
                    UInt32.zero,
                    CryptoUtil.doubleSHA256(ByteVector.empty).bytes.take(4))

    Try(NetworkPayload(header, ByteVector.empty)) match {
      case Failure(_: NoSuchElementException) =>
        fail("Unknown command name must not throw a raw NoSuchElementException")
      case _ =>
        succeed // a descriptive failure or a handled unknown payload is fine
    }
  }

  it must "not interpret trailing bytes beyond payloadSize as part of the payload" in {
    // Finding: the payload is not sliced to the declared `payloadSize`, see
    // core/src/main/scala/org/bitcoins/core/serializers/p2p/RawNetworkMessageSerializer.scala:11-20
    // Correct behavior: only `payloadSize` bytes after the header may be
    // interpreted as the payload; trailing bytes belong to the next message
    // in the stream and must be left alone.
    // RejectMessage is used because its `extra` field consumes all remaining
    // bytes, so trailing bytes leak into the parsed payload.
    // payload: message="tx", code=0x10, reason="", no extra data
    val payloadBytes = ByteVector.fromValidHex("0274781000")
    val header =
      NetworkHeader(RegTest,
                    "reject",
                    UInt32(payloadBytes.size),
                    CryptoUtil.doubleSHA256(payloadBytes).bytes.take(4))
    // simulate framing where the next message's bytes follow immediately
    val trailingBytes = ByteVector.fromValidHex("f9beb4d976657261")
    val streamBytes = header.bytes ++ payloadBytes ++ trailingBytes

    val msg = NetworkMessage.fromBytes(streamBytes)
    msg.payload.bytes.size must be(header.payloadSize.toInt)
  }

  it must "not wedge forever on unparseable bytes at the start of the stream" in {
    // Finding: parseIndividualMessages never drops bytes that deterministically
    // fail to parse, so the same failing parse is retried on every subsequent
    // chunk and no later valid message is ever surfaced, see
    // core/src/main/scala/org/bitcoins/core/util/NetworkUtil.scala:202-207
    // Correct behavior: the parser must either skip the bad bytes or fail
    // cleanly -- it must not silently return empty forever while a valid
    // message sits later in the stream.
    // A "filterload" message whose declared payload (1 byte) is too short to
    // contain a bloom filter: the header parses fine, but the payload parse
    // fails deterministically (RawBloomFilterSerializer reads the flags byte
    // at index 9).
    val badPayload = ByteVector(0.toByte)
    val badHeader =
      NetworkHeader(RegTest,
                    "filterload",
                    UInt32(badPayload.size),
                    CryptoUtil.doubleSHA256(badPayload).bytes.take(4))
    val badBytes = badHeader.bytes ++ badPayload

    val validMessage = NetworkMessage(RegTest, VerAckMessage)

    // feed the stream chunk by chunk, as a TCP stream would arrive
    val (firstMessages, leftover) =
      NetworkUtil.parseIndividualMessages(badBytes)
    val secondAttempt =
      Try(NetworkUtil.parseIndividualMessages(leftover ++ validMessage.bytes))

    secondAttempt match {
      case Failure(_) =>
        succeed // failing cleanly on the garbage is acceptable
      case Success((secondMessages, _)) =>
        // the valid trailing message must eventually be surfaced
        (firstMessages ++ secondMessages).map(_.payload) must contain(
          VerAckMessage)
    }
  }
}
