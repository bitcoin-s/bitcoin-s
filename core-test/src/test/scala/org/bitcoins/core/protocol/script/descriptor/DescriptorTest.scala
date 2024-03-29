package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DescriptorTest extends BitcoinSUnitTest {

  behavior of "OutputDescriptor"

  it must "parse a p2wpkh descriptor with a xpub embedded" ignore {
    val descriptorStr =
      "wpkh(xpub661MyMwAqRbcFvqQ14RrhJ5seDNrUeJGcaKYxmXwCfQKrCzUv8ScZDjaHoKvdjHuneaDQGGFbrozqw7JVoqHfBSs5i4igzf8zfUPxySeL6N/44/0/1/0/0)#2tapg52e"
    val descriptor = P2WPKHDescriptor.fromString(descriptorStr)
    val address = Bech32Address(descriptor.scriptPubKey, MainNet)
    val expectedSPK = P2WPKHWitnessSPKV0.fromAsmHex(
      "001406f3fcaef19f1d7824437b649d56fb306b1bbd9e")
    assert(descriptor.scriptPubKey == expectedSPK)
    assert(
      address == Bech32Address.fromString(
        "bc1qqmeleth3nuwhsfzr0djf64hmxp43h0v7ny5sam"))
    assert(descriptor.checksum.get == "2tapg52e")
  }
}
