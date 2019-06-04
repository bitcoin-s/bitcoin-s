package org.bitcoins.node.util

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.HeadersMessage
import org.bitcoins.node.messages.data.HeadersMessage
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.messages.HeadersMessage
import org.bitcoins.node.messages.control.VersionMessage
import org.bitcoins.node.messages.data.HeadersMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 9/6/16.
  */
class BitcoinSpvNodeUtilTest extends BitcoinSUnitTest {

  "BitcoinSpvNodeUtil" must "return the entire byte array if a message is not aligned to a byte frame" in {
    val versionMessage =
      VersionMessage(TestNet3.dnsSeeds(0), Constants.networkParameters)
    val networkMsg = NetworkMessage(Constants.networkParameters, versionMessage)
    //remove last byte so the message is not aligned
    val bytes = networkMsg.bytes.slice(0, networkMsg.bytes.size - 1)
    val (_, unAlignedBytes) = BitcoinSpvNodeUtil.parseIndividualMessages(bytes)

    unAlignedBytes must be(bytes)
  }

  it must "block header message that is not aligned with a tcp frame" in {

    val headersMsg = HeadersMessage(
      CompactSizeUInt(UInt64(2), 1),
      Vector(
        BlockHeader(
          Int32(315017594),
          DoubleSha256Digest(
            "177e777f078d2deeaa3ad4b82e78a00ad2f4738c5217f7a36d9cf3bd11e41817"),
          DoubleSha256Digest(
            "1dcaebebd620823bb344bd18a18276de508910d66b4e3cbb3426a14eced66224"),
          UInt32(2845833462L),
          UInt32(2626024374L),
          UInt32(2637850613L)
        ),
        BlockHeader(
          Int32(1694049746),
          DoubleSha256Digest(
            "07b6d61809476830bc7ef862a983a7222997df3f639e0d2aa5902a5a48018430"),
          DoubleSha256Digest(
            "68c65f803b70b72563e86ac3e8e20ad11fbfa2eac3f9fddf4bc624d03a14f084"),
          UInt32(202993555),
          UInt32(4046619225L),
          UInt32(1231236881)
        )
      )
    )
    val networkMsg = NetworkMessage(Constants.networkParameters, headersMsg)
    //split the network msg at a random index to simulate a tcp frame not being aligned
    val randomIndex = scala.util.Random.nextInt().abs % networkMsg.bytes.size
    val (firstHalf, secondHalf) = networkMsg.bytes.splitAt(randomIndex)
    val (firstHalfParseHeaders, remainingBytes) =
      BitcoinSpvNodeUtil.parseIndividualMessages(firstHalf)
    firstHalfParseHeaders.isEmpty must be(true)

    val (secondHalfParsedHeaders, _) =
      BitcoinSpvNodeUtil.parseIndividualMessages(remainingBytes ++ secondHalf)
    val parsedNetworkMsg = secondHalfParsedHeaders.head
    val parsedHeadersMsg = parsedNetworkMsg.payload.asInstanceOf[HeadersMessage]
    parsedNetworkMsg.header must be(networkMsg.header)
    parsedHeadersMsg.headers.head must be(headersMsg.headers.head)
    parsedHeadersMsg.headers(1) must be(parsedHeadersMsg.headers(1))

  }
}
