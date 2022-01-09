package org.bitcoins.core.util

import scodec.bits.ByteVector

class NetworkUtilTest extends BitcoinSUtilTest {
  "NetworkUtil" must "convert torV3 pubkey to correct .onion address and vice versa" in {
    val pubkey = ByteVector.fromValidHex(
      "98908ec971ad17171365865a249c420d04a66f103f973a62d8404ce2f4af1ee2")
    val address =
      "tcii5slrvulroe3fqzncjhccbuckm3yqh6ltuywyibgof5fpd3rpycyd.onion"
    val addressFromKey = NetworkUtil.parseUnresolvedInetSocketAddress(pubkey)
    val pubkeyFromAddress = ByteVector(NetworkUtil.torV3AddressToBytes(address))
    assert(address == addressFromKey)
    assert(pubkey == pubkeyFromAddress)
  }
}
