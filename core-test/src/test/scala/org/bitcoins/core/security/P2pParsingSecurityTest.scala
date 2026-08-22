package org.bitcoins.core.security

import org.bitcoins.core.p2p.VersionMessage
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
}
