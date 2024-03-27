package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DescriptorChecksumTest extends BitcoinSUnitTest {

  behavior of "DescriptorChecksumTest"

  it must "calculate correct checksums from BIP380 examples" in {
    val str0 = "raw(deadbeef)#89f8spxm"
    val split0 = str0.split("#")
    val (payload, checksum) = (split0(0), split0(1))
    assert(OutputDescriptor.createChecksum(payload) == checksum)
  }
}
