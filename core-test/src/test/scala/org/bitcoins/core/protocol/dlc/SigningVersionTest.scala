package org.bitcoins.core.protocol.dlc

import org.bitcoins.testkit.util.BitcoinSUnitTest

class SigningVersionTest extends BitcoinSUnitTest {

  behavior of "SigningVersion"

  it must "read from string" in {
    SigningVersion.fromString("mock") must be(SigningVersion.Mock)
    SigningVersion.fromString("MoCk") must be(SigningVersion.Mock)
    SigningVersion.fromString("MOCK") must be(SigningVersion.Mock)
    SigningVersion.fromStringOpt("not a real signing version") must be(None)
  }
}
