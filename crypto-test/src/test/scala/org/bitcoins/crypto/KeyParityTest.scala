package org.bitcoins.crypto

import scodec.bits.ByteVector

class KeyParityTest extends BitcoinSCryptoTest {
  behavior of "KeyParity"

  it must "use 0x02 for even and 0x03 for odd" in {
    assert(EvenParity.bytes == ByteVector.fromByte(0x02))
    assert(OddParity.bytes == ByteVector.fromByte(0x03))
    assert(KeyParity.fromByte(0x02) == EvenParity)
    assert(KeyParity.fromByte(0x03) == OddParity)
    assert(KeyParity(ByteVector.fromByte(0x02)) == EvenParity)
    assert(KeyParity(ByteVector.fromByte(0x03)) == OddParity)
  }

  it must "fail to construct parity from bytes other than 0x02 or 0x03" in {
    forAll(NumberGenerator.byte.filterNot(b => b == 0x02 || b == 0x03)) {
      byte =>
        assertThrows[IllegalArgumentException](KeyParity.fromByte(byte))
    }

    forAll(NumberGenerator.bytevector.filter(_.length != 1)) { bytes =>
      assertThrows[IllegalArgumentException](KeyParity(bytes))
    }
  }
}
