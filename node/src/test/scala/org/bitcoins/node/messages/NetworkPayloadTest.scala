package org.bitcoins.node.messages

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.headers.NetworkHeader
import org.bitcoins.node.util.TestUtil
import org.bitcoins.node.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/10/16.
  */
class NetworkPayloadTest
    extends FlatSpec
    with MustMatchers
    with BitcoinSLogger {

  "NetworkMessage" must "create a payload object from it's network header and the payload bytes" in {
    val rawNetworkMessage = TestUtil.rawNetworkMessage
    val header = NetworkHeader(rawNetworkMessage.take(48))
    logger.debug("Header: " + header)
    val payloadHex = rawNetworkMessage.slice(48, rawNetworkMessage.length)
    val payload = NetworkPayload(header, payloadHex)
    payload.isInstanceOf[VersionMessage] must be(true)
    payload.commandName must be(NetworkPayload.versionCommandName)
  }
}
