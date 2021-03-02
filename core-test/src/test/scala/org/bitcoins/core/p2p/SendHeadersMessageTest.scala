package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class SendHeadersMessageTest extends BitcoinSUnitTest {
  it should "have serialization symmetry" in {
    val message = SendHeadersMessage
    assert(message.bytes == ByteVector.empty)
    assert(message.commandName == NetworkPayload.sendHeadersCommandName)
  }
}
