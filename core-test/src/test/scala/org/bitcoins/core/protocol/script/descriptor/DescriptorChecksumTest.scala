package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.NonStandardScriptPubKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DescriptorChecksumTest extends BitcoinSUnitTest {

  behavior of "DescriptorChecksumTest"

  val expression =
    RawScriptExpression(NonStandardScriptPubKey.fromAsmHex("deadbeef"))
  it must "calculate correct checksums from BIP380 examples" in {
    val str0 = "raw(deadbeef)#89f8spxm"
    val split0 = str0.split("#")
    val (payload, checksum) = (split0(0), split0(1))
    assert(Descriptor.createChecksum(payload) == checksum)

    assert(Descriptor.isValidChecksum(expression, Some(checksum)))

    //expression with nochecksum should be valid
    assert(Descriptor.isValidChecksum(expression, None))
  }

  it must "fail when a bad checksum is given" in {
    //Missing checksum
    assert(!Descriptor.isValidChecksum(expression, Some("#")))
    //Too long checksum (9 chars)
    assert(!Descriptor.isValidChecksum(expression, Some("89f8spxmx")))
    //Too short checksum (7 chars)
    assert(!Descriptor.isValidChecksum(expression, Some("89f8spx")))
    //Error in payload
    val bad =
      RawScriptExpression(NonStandardScriptPubKey.fromAsmHex("deedbeef"))
    assert(!Descriptor.isValidChecksum(bad, Some("89f8spxm")))
    //Error in checksum
    assert(!Descriptor.isValidChecksum(expression, Some("#9f8spxm")))
  }
}
