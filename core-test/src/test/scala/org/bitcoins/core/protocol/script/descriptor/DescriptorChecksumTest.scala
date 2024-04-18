package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.NonStandardScriptPubKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DescriptorChecksumTest extends BitcoinSUnitTest {

  behavior of "DescriptorChecksumTest"

  val descriptor0 =
    RawDescriptor(
      RawScriptExpression(NonStandardScriptPubKey.fromAsmHex("deadbeef")),
      None)
  it must "calculate correct checksums from BIP380 examples" in {
    val str0 = "raw(deadbeef)#89f8spxm"
    val split0 = str0.split("#")
    val (payload, checksum) = (split0(0), split0(1))
    assert(Descriptor.createChecksum(payload) == checksum)

    assert(Descriptor.isValidChecksum(descriptor0, Some(checksum)))

    //expression with nochecksum should be valid
    assert(Descriptor.isValidChecksum(descriptor0, None))

    val descriptor1 =
      Descriptor.fromString(
        "wpkh([d34db33f/84h/0h/0h]xpub6DJ2dNUysrn5Vt36jH2KLBT2i1auw1tTSSomg8PhqNiUtx8QX2SvC9nrHu81fT41fvDUnhMjEzQgXnQjKEu3oaqMSzhSrHMxyyoEAmUHQbY/0/*)")
    val checksum1 = "cjjspncu"
    assert(Descriptor.isValidChecksum(descriptor1, Some(checksum1)))
    assert(Descriptor.createChecksum(descriptor1) == checksum1)
  }

  it must "fail when a bad checksum is given" in {
    //Missing checksum
    assert(!Descriptor.isValidChecksum(descriptor0, Some("#")))
    //Too long checksum (9 chars)
    assert(!Descriptor.isValidChecksum(descriptor0, Some("89f8spxmx")))
    //Too short checksum (7 chars)
    assert(!Descriptor.isValidChecksum(descriptor0, Some("89f8spx")))
    //Error in payload
    val bad =
      RawDescriptor(
        RawScriptExpression(NonStandardScriptPubKey.fromAsmHex("deedbeef")),
        None)
    assert(!Descriptor.isValidChecksum(bad, Some("89f8spxm")))
    //Error in checksum
    assert(!Descriptor.isValidChecksum(descriptor0, Some("#9f8spxm")))
  }
}
