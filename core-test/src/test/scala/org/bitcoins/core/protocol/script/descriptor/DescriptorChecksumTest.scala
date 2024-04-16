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

//descriptor.py
//INPUT_CHARSET = "0123456789()[],'/*abcdefgh@:$%{}IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~ijklmnopqrstuvwxyzABCDEFGH`#\"\\ "
//CHECKSUM_CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
//GENERATOR = [0xf5dee51989, 0xa9fdca3312, 0x1bab10e32d, 0x3706b1677a, 0x644d626ffd]
//
//def descsum_polymod(symbols):
//    """Internal function that computes the descriptor checksum."""
//    chk = 1
//    for value in symbols:
//        top = chk >> 35
//        chk = (chk & 0x7ffffffff) << 5 ^ value
//        for i in range(5):
//            chk ^= GENERATOR[i] if ((top >> i) & 1) else 0
//    return chk
//
//def descsum_expand(s):
//    """Internal function that does the character to symbol expansion"""
//    groups = []
//    symbols = []
//    for c in s:
//        if not c in INPUT_CHARSET:
//            return None
//        v = INPUT_CHARSET.find(c)
//        symbols.append(v & 31)
//        groups.append(v >> 5)
//        if len(groups) == 3:
//            symbols.append(groups[0] * 9 + groups[1] * 3 + groups[2])
//            groups = []
//    if len(groups) == 1:
//        symbols.append(groups[0])
//    elif len(groups) == 2:
//        symbols.append(groups[0] * 3 + groups[1])
//    return symbols
//
//def descsum_check(s):
//    """Verify that the checksum is correct in a descriptor"""
//    if s[-9] != '#':
//        return False
//    if not all(x in CHECKSUM_CHARSET for x in s[-8:]):
//        return False
//    symbols = descsum_expand(s[:-9]) + [CHECKSUM_CHARSET.find(x) for x in s[-8:]]
//    return descsum_polymod(symbols) == 1
//INPUT_CHARSET = "0123456789()[],'/*abcdefgh@:$%{}IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~ijklmnopqrstuvwxyzABCDEFGH`#\"\\ "
//CHECKSUM_CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
//GENERATOR = [0xf5dee51989, 0xa9fdca3312, 0x1bab10e32d, 0x3706b1677a, 0x644d626ffd]
//
//def descsum_polymod(symbols):
//    """Internal function that computes the descriptor checksum."""
//    chk = 1
//    for value in symbols:
//        top = chk >> 35
//        chk = (chk & 0x7ffffffff) << 5 ^ value
//        for i in range(5):
//            chk ^= GENERATOR[i] if ((top >> i) & 1) else 0
//    return chk
//
//def descsum_expand(s):
//    """Internal function that does the character to symbol expansion"""
//    groups = []
//    symbols = []
//    for c in s:
//        if not c in INPUT_CHARSET:
//            return None
//        v = INPUT_CHARSET.find(c)
//        symbols.append(v & 31)
//        groups.append(v >> 5)
//        if len(groups) == 3:
//            symbols.append(groups[0] * 9 + groups[1] * 3 + groups[2])
//            groups = []
//    if len(groups) == 1:
//        symbols.append(groups[0])
//    elif len(groups) == 2:
//        symbols.append(groups[0] * 3 + groups[1])
//    return symbols
//
//def descsum_check(s):
//    """Verify that the checksum is correct in a descriptor"""
//    if s[-9] != '#':
//        return False
//    if not all(x in CHECKSUM_CHARSET for x in s[-8:]):
//        return False
//    symbols = descsum_expand(s[:-9]) + [CHECKSUM_CHARSET.find(x) for x in s[-8:]]
//    return descsum_polymod(symbols) == 1
//
//result = descsum_check("wpkh([d34db33f/84h/0h/0h]xpub6DJ2dNUysrn5Vt36jH2KLBT2i1auw1tTSSomg8PhqNiUtx8QX2SvC9nrHu81fT41fvDUnhMjEzQgXnQjKEu3oaqMSzhSrHMxyyoEAmUHQbY/0/*)#cjjspncu")
//print(result)