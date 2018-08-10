package org.bitcoins.core.crypto

import org.scalatest.{ FlatSpec, MustMatchers }
import scodec.bits.ByteVector

/**
 * Created by chris on 3/22/16.
 */
class ECDigitalSignatureTest extends FlatSpec with MustMatchers {

  "ECDigitalSignature" must "say that empty signature is a valid DER encoded signature" in {
    val emptySiganture = ECDigitalSignature(ByteVector.empty)
    emptySiganture.isDEREncoded must be(true)

  }

  it must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature("304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80")
    signature.isDEREncoded must be(true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac")
    signature.isDEREncoded must be(true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature("304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b7420")
    signature.isDEREncoded must be(true)
  }

  it must "say that the empty digital signatures r,s values are both 0" in {
    EmptyDigitalSignature.r must be(0)
    EmptyDigitalSignature.s must be(0)
  }

  it must "create a digital signature from it's r,s components" in {
    //from the tx 44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff
    val rawDigitalSignature = "3044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26"
    val digitalSignature = ECDigitalSignature(rawDigitalSignature)
    val (r, s) = (digitalSignature.r, digitalSignature.s)
    val digitalSignatureFromRS = ECDigitalSignature(r, s)
    digitalSignatureFromRS must be(digitalSignature)
  }

  it must "create an empty digital signature when given 0 in hex or byte format" in {
    val hex = ECDigitalSignature("00")
    val byte = ECDigitalSignature(ByteVector.low(1))
    val emptySignature = ECDigitalSignature("")
    byte must be(emptySignature)
    hex must be(emptySignature)
  }
}
