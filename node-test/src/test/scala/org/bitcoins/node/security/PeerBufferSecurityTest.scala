package org.bitcoins.node.security

import org.apache.pekko.stream.scaladsl.{Sink, Source}
import org.apache.pekko.util.ByteString
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.NetworkHeader
import org.bitcoins.node.networking.peer.PeerConnection
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

import scala.concurrent.Future

class PeerBufferSecurityTest extends BitcoinSAsyncTest {

  behavior of "PeerConnection inbound message buffer"

  /** Bitcoin Core's MAX_SIZE: the largest payload the p2p protocol permits.
    * Headers that declare a larger payload must be rejected.
    */
  private val maxProtocolPayloadSize: Int = 0x02000000 // 32 MiB

  /** A declared payloadSize one byte above the protocol maximum. */
  private val oversizedPayloadSize: Int = maxProtocolPayloadSize + 1

  /** A command name that is not in NetworkPayload.commandNames, so the parser
    * treats the message as unknown and skips payload deserialization. The
    * attacker does not need a valid checksum for this attack.
    */
  private val unknownCommandName: String = "evilcmd"

  private val chunkSize: Int = 4096

  private def oversizedHeader(declaredPayloadSize: Int): ByteString = {
    val header = NetworkHeader(
      network = RegTest,
      commandName = unknownCommandName,
      payloadSize = UInt32(declaredPayloadSize),
      checksum = ByteVector.low(4)
    )
    ByteString.fromArray(header.bytes.toArray)
  }

  private def payloadChunk: ByteString =
    ByteString.fromArray(new Array[Byte](chunkSize))

  /** Drives the exact accumulation path used for inbound peer bytes:
    * PeerConnection.parseHelper folds every TCP frame into an unaligned buffer
    * and calls NetworkUtil.parseIndividualMessages on the whole buffer.
    */
  private def feedChunks(chunks: Vector[ByteString]): Future[Long] = {
    val start = System.nanoTime()
    Source(chunks)
      .via(PeerConnection.parseToNetworkMsgFlow)
      .runWith(Sink.ignore)
      .map(_ => System.nanoTime() - start)
  }

  // Finding 1 (High): unbounded inbound reassembly buffer, no maximum message
  // size. node/.../peer/PeerConnection.scala:400-426 and
  // core/.../util/NetworkUtil.scala:180-227.
  // Correct behavior: the parser rejects a header whose declared payloadSize
  // is above the protocol maximum (fails the stream), instead of buffering
  // attacker bytes without a limit.
  it must "fail the stream when a peer declares a payloadSize above the protocol maximum" in {
    // Send the header plus only a small amount of the declared payload.
    // The buffer holds all bytes we send and waits for the rest of the
    // 32 MiB + 1 payload that never comes.
    val numChunks = 100 // ~400 KiB of actual data
    val chunks =
      oversizedHeader(oversizedPayloadSize) +: Vector.fill(numChunks)(
        payloadChunk)

    val resultF = Source(chunks)
      .via(PeerConnection.parseToNetworkMsgFlow)
      .runWith(Sink.seq)

    // Safe behavior: the stream must fail because the declared payloadSize
    // exceeds the protocol maximum. Today the stream completes normally and
    // keeps the unbounded buffer, so this assertion fails.
    resultF.failed.map(_ => succeed)
  }

  // Finding 2 (Informational): O(n^2) copying in the accumulation path.
  // PeerConnection.parseHelper (PeerConnection.scala:400-410) copies the full
  // unaligned buffer into a new ByteVector for every chunk.
  // This test only measures and reports the scaling factor; it makes no
  // wall-clock assertion.
  it must "report the cost scaling of appending chunks to the unaligned buffer (informational)" in {
    // Declare a payload larger than the data sent so the buffer keeps
    // growing for the whole run. Total data sent stays at ~2 MiB.
    val declaredPayloadSize = 0x04000000 // 64 MiB, never reached
    val smallN = 256 // 1 MiB of payload chunks
    val largeN = smallN * 2 // 2 MiB of payload chunks

    def run(numChunks: Int): Future[Long] = {
      val chunks =
        oversizedHeader(declaredPayloadSize) +: Vector.fill(numChunks)(
          payloadChunk)
      feedChunks(chunks)
    }

    for {
      _ <- run(smallN) // warmup for JIT and stream setup
      smallNanos <- run(smallN)
      largeNanos <- run(largeN)
    } yield {
      val factor =
        if (smallNanos > 0) largeNanos.toDouble / smallNanos.toDouble else 0.0
      // Doubling the number of chunks must double the cost for a linear
      // append (factor ~2.0). With O(n^2) copying the factor approaches 4.0.
      println(
        s"PeerBufferSecurityTest scaling: chunks=$smallN took ${smallNanos}ns, " +
          s"chunks=$largeN took ${largeNanos}ns, factor=$factor " +
          s"(linear append ~= 2.0, quadratic copying ~= 4.0)")
      succeed
    }
  }
}
