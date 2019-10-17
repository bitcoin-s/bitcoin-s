package org.bitcoins.core.crypto

import org.bitcoins.core.util.NumberUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 3/23/16.
  */
class DERSignatureUtilTest extends BitcoinSUnitTest {

  val p2shSignature = ECDigitalSignature(
    "304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf8001")

  val p2pkhSignature = ECDigitalSignature(
    "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01")

  val p2pkSignature = ECDigitalSignature(
    "304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001")

  "DERSignatureUtil" must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in {
    DERSignatureUtil.isValidSignatureEncoding(p2shSignature) must be(true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in {
    //need to remove the hash type byte to check for der encoding
    val hashTypeByteRemoved =
      p2pkhSignature.bytes.slice(0, p2pkhSignature.bytes.size - 1)
    DERSignatureUtil.isDEREncoded(hashTypeByteRemoved) must be(true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {
    val hashTypeByteRemoved =
      p2pkSignature.bytes.slice(0, p2pkSignature.bytes.size - 1)
    DERSignatureUtil.isDEREncoded(hashTypeByteRemoved) must be(true)
  }

  it must "retrieve the (r,s) values for a p2sh signature in bitcoin" in {
    val (r, s) = DERSignatureUtil.decodeSignature(p2shSignature)
    r must be(
      NumberUtil.toBigInt(
        "5b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb"))
    s must be(
      NumberUtil.toBigInt(
        "2e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80"))
  }

  it must "retrieve the (r,s) values for a p2pkh signature in bitcoin" in {
    val (r, s) = DERSignatureUtil.decodeSignature(p2pkhSignature)
    r must be(
      NumberUtil.toBigInt(
        "16ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca03"))
    s must be(
      NumberUtil.toBigInt(
        "119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac"))
  }

  it must "retrieve the (r,s) values from a p2pk signature in bitcoin" in {
    val (r, s) = DERSignatureUtil.decodeSignature(p2pkSignature)
    r must be(
      NumberUtil.toBigInt(
        "0a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa1"))
    s must be(
      NumberUtil.toBigInt(
        "1fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b7420"))
  }

  it must "say that a signature taken from a p2sh transaction is a valid stirctly DER encoded signature" in {
    DERSignatureUtil.isValidSignatureEncoding(p2shSignature) must be(true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid strictly DER encoded signature" in {
    DERSignatureUtil.isValidSignatureEncoding(p2pkhSignature) must be(true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid strictly DER encoded signature" in {

    DERSignatureUtil.isValidSignatureEncoding(p2pkSignature) must be(true)
  }

  it must "say that the empty signature is a valid strictly encoded DER signature" in {
    DERSignatureUtil.isValidSignatureEncoding(ECDigitalSignature("")) must be(
      true)
    DERSignatureUtil.isValidSignatureEncoding(EmptyDigitalSignature) must be(
      true)
  }

  it must "say that an overly long signature is NOT strict der encoded" in {
    val sig = ECDigitalSignature(
      "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
    DERSignatureUtil.isValidSignatureEncoding(sig) must be(false)
  }

  it must "determine if a signature is encoded with a low s value" in {
    val highS = ECDigitalSignature(
      "304502203e4516da7253cf068effec6b95c41221c0cf3a8e6ccb8cbf1725b562e9afde2c022100ab1e3da73d67e32045a20e0b999e049978ea8d6ee5480d485fcf2ce0d03b2ef001".toLowerCase)
    DERSignatureUtil.isLowS(highS) must be(false)
  }
}
