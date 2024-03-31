package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class KeyExpressionTest extends BitcoinSUnitTest {

  behavior of "KeyExpression"

  it must "serialize and deserialize key origin examples in BIP380" in {
    val str0 = "[deadbeef/0'/0'/0']"
    val str1 = "[deadbeef/0h/0h/0h]"
    val str2 = "[deadbeef/0'/0h/0']"
    val keyOrigin = KeyOriginExpression.fromString(str0)
    assert(str0 == keyOrigin.toString)
    assert(keyOrigin == KeyOriginExpression.fromString(str1))
    keyOrigin == KeyOriginExpression.fromString(str2)
  }

  it must "parse valid private key expressions from BIP380" in {
    val str0 = "5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss"
    val str1 = "L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1"

    assert(KeyExpression.fromString(str0).toString == str0)
    assert(KeyExpression.fromString(str1).toString == str1)
  }

  it must "parse valid public key expressions from BIP380" in {
    val str0 =
      "0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600"
    val str1 =
      "04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235"

    assert(KeyExpression.fromString(str0).toString == str0)
    assert(KeyExpression.fromString(str1).toString == str1)
  }
}
