package org.bitcoins.crypto

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/22/16.
 */
class ECDigitalSignatureTest extends FlatSpec with MustMatchers {


  "ECDigitalSignature" must "say that empty signature is a valid DER encoded signature" in {
    val emptySiganture = ECFactory.digitalSignature(Seq())
    emptySiganture.isDEREncoded must be (true)

  }

  it must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in  {
    val signature = ECFactory.digitalSignature("304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf8001")
    signature.isDEREncoded must be (true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in  {
    val signature = ECFactory.digitalSignature("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01")
    signature.isDEREncoded must be (true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {
    val signature = ECFactory.digitalSignature("304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001")
    signature.isDEREncoded must be (true)
  }

  it must "say that the empty digital signatures r,s values are both 0" in {
    EmptyDigitalSignature.r must be (0)
    EmptyDigitalSignature.s must be (0)
  }
}
