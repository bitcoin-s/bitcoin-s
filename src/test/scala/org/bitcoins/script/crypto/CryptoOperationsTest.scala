package org.bitcoins.script.crypto

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/6/16.
 */
class CryptoOperationsTest extends FlatSpec with MustMatchers  {

  "CryptoOperations" must "define OP_RIPEMD160" in {
    OP_RIPEMD160.opCode must be (166)
  }

  it must "define OP_SHA1" in {
    OP_SHA1.opCode must be (167)
  }

  it must "define OP_SHA256" in {
    OP_SHA256.opCode must be (168)
  }

  it must "define OP_HASH160" in {
    OP_HASH160.opCode must be (169)
  }

  it must "define OP_HASH256" in {
    OP_HASH256.opCode must be (170)
  }

  it must "define OP_CODESEPARATOR" in {
    OP_CODESEPARATOR.opCode must be (171)
  }

  it must "define OP_CHECKSIG" in {
    OP_CHECKSIG.opCode must be (172)
  }

  it must "define OP_CHECKSIGVERIFY" in {
    OP_CHECKSIGVERIFY.opCode must be (173)
  }

  it must "define OP_CHECKMULTISIG" in {
    OP_CHECKMULTISIG.opCode must be (174)
  }

  it must "define OP_CHECKMULTISIGVERIFY" in {
    OP_CHECKMULTISIGVERIFY.opCode must be (175)
  }
}
