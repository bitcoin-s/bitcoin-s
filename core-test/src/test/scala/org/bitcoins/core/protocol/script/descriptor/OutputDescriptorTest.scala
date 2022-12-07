package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.annotation.nowarn

class OutputDescriptorTest extends BitcoinSUnitTest {

  behavior of "OutputDescriptor"

  it must "parse a p2wpkh descriptor with a xpub embedded" in {
    @nowarn val descriptorStr =
      "wpkh(xpub661MyMwAqRbcFvqQ14RrhJ5seDNrUeJGcaKYxmXwCfQKrCzUv8ScZDjaHoKvdjHuneaDQGGFbrozqw7JVoqHfBSs5i4igzf8zfUPxySeL6N/44/0/1/0/0)#2tapg52e"
  }
}
