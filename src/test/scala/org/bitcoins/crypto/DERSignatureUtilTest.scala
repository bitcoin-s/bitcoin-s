package org.bitcoins.crypto

import org.bitcoins.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/23/16.
 */
class DERSignatureUtilTest extends FlatSpec with MustMatchers {

  val p2shSignature = ECFactory.digitalSignature("304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf8001")
  val p2pkhSignature = ECFactory.digitalSignature("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01")
  val p2pkSignature = ECFactory.digitalSignature("304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001")
  "DERSignatureUtil" must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in  {
    DERSignatureUtil.isDEREncoded(p2shSignature) must be (true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in  {
    DERSignatureUtil.isDEREncoded(p2pkhSignature) must be (true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {

    DERSignatureUtil.isDEREncoded(p2pkSignature) must be (true)
  }

  it must "retrieve the (r,s) values for a p2sh signature in bitcoin" in {
    val (r,s) = DERSignatureUtil.decodeSignature(p2shSignature)
    r must be (BitcoinSUtil.hexToBigInt("5b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb"))
    s must be (BitcoinSUtil.hexToBigInt("2e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80"))
  }

  it must "retrieve the (r,s) values for a p2pkh signature in bitcoin" in {
    val (r,s) = DERSignatureUtil.decodeSignature(p2pkhSignature)
    r must be (BitcoinSUtil.hexToBigInt("16ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca03"))
    s must be (BitcoinSUtil.hexToBigInt("119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac"))
  }

  it must "retrieve the (r,s) values from a p2pk signature in bitcoin" in {
    val (r,s) = DERSignatureUtil.decodeSignature(p2pkSignature)
    r must be (BitcoinSUtil.hexToBigInt("0a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa1"))
    s must be (BitcoinSUtil.hexToBigInt("1fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b7420"))
  }

  it must "say that a signature taken from a p2sh transaction is a valid stirctly DER encoded signature" in {
    DERSignatureUtil.isStrictDEREncoding(p2shSignature) must be (true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid strictly DER encoded signature" in  {
    DERSignatureUtil.isStrictDEREncoding(p2pkhSignature) must be (true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid strictly DER encoded signature" in {

    DERSignatureUtil.isStrictDEREncoding(p2pkSignature) must be (true)
  }

  it must "say that the empty signature is a valid strictly encoded DER signature" in {
    DERSignatureUtil.isStrictDEREncoding(ECFactory.digitalSignature("")) must be (true)
    DERSignatureUtil.isStrictDEREncoding(EmptyDigitalSignature) must be (true)
  }


  it must "say that an overly long signature is NOT strict der encoded" in {
    val sig = ECFactory.digitalSignature("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    DERSignatureUtil.isStrictDEREncoding(sig) must be (false)
  }
}

