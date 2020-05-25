package org.bitcoins.crypto

import org.bitcoins.core.util.NumberUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

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

  it must "parse lax DER signatures" in {
    // Copied from https://github.com/rust-bitcoin/rust-secp256k1/blob/a1842125a77cadaa8ac7d6ca794ddb1f4852c593/src/lib.rs#L940-L956 except for the first one
    // which is from script_tests.json and the second one which is from tx_valid.json
    val signatures = Vector(
      "304402200060558477337b9022e70534f1fea71a318caf836812465a2509931c5e7c4987022078ec32bd50ac9e03a349ba953dfd9fe1c8d2dd8bdb1d38ddca844d3d5c78c11801",
      "30440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee09001",
      "304402204c2dd8a9b6f8d425fcd8ee9a20ac73b619906a6367eac6cb93e70375225ec0160220356878eff111ff3663d7e6bf08947f94443845e0dcc54961664d922f7660b80c",
      "304402202ea9d51c7173b1d96d331bd41b3d1b4e78e66148e64ed5992abd6ca66290321c0220628c47517e049b3e41509e9d71e480a0cdc766f8cdec265ef0017711c1b5336f",
      "3045022100bf8e050c85ffa1c313108ad8c482c4849027937916374617af3f2e9a881861c9022023f65814222cab09d5ec41032ce9c72ca96a5676020736614de7b78a4e55325a",
      "3046022100839c1fbc5304de944f697c9f4b1d01d1faeba32d751c0f7acb21ac8a0f436a72022100e89bd46bb3a5a62adc679f659b7ce876d83ee297c7a5587b2011c4fcc72eab45",
      "3046022100eaa5f90483eb20224616775891397d47efa64c68b969db1dacb1c30acdfc50aa022100cf9903bbefb1c8000cf482b0aeeb5af19287af20bd794de11d82716f9bae3db1",
      "3045022047d512bc85842ac463ca3b669b62666ab8672ee60725b6c06759e476cebdc6c102210083805e93bd941770109bcc797784a71db9e48913f702c56e60b1c3e2ff379a60",
      "3044022023ee4e95151b2fbbb08a72f35babe02830d14d54bd7ed1320e4751751d1baa4802206235245254f58fd1be6ff19ca291817da76da65c2f6d81d654b5185dd86b8acf"
    ).map(ByteVector.fromValidHex(_))

    signatures.foreach { sig =>
      DERSignatureUtil.parseDERLax(sig) match {
        case None => fail(s"Failed to parse $sig")
        case Some((r, s)) =>
          if (r == BigInt(0) || s == BigInt(0)) {
            fail(s"Failed to parse $sig")
          } else {
            succeed
          }
      }
    }
  }

  it must "fail to parse garbage signatures" in {
    val signatures = Vector(
      ("4402204c2dd8a9b6f8d425fcd8ee9a20ac73b619906a6367eac6cb93e70375225ec0160220356878eff111ff3663d7e6bf08947f94443845e0dcc54961664d922f7660b80c",
       "no leading byte"),
      ("204402202ea9d51c7173b1d96d331bd41b3d1b4e78e66148e64ed5992abd6ca66290321c0220628c47517e049b3e41509e9d71e480a0cdc766f8cdec265ef0017711c1b5336f",
       "bad leading byte"),
      ("30477022100bf8e050c85ffa1c313108ad8c482c4849027937916374617af3f2e9a881861c9022023f65814222cab09d5ec41032ce9c72ca96a5676020736614de7b78a4e55325a",
       "long length byte"),
      ("30462100839c1fbc5304de944f697c9f4b1d01d1faeba32d751c0f7acb21ac8a0f436a72022100e89bd46bb3a5a62adc679f659b7ce876d83ee297c7a5587b2011c4fcc72eab45",
       "no r integer tag"),
      ("3046022100eaa5f90483eb20224616775891397d47efa64c68b969db1dacb1c30acdfc50aa2100cf9903bbefb1c8000cf482b0aeeb5af19287af20bd794de11d82716f9bae3db1",
       "no s integer tag"),
      ("3045022847d512bc85842ac463ca3b669b62666ab8672ee60725b6c06759e476cebdc6c102210083805e93bd941770109bcc797784a71db9e48913f702c56e60b1c3e2ff379a60",
       "long r length"),
      ("3044022023ee4e95151b2fbbb08a72f35babe02830d14d54bd7ed1320e4751751d1baa4802286235245254f58fd1be6ff19ca291817da76da65c2f6d81d654b5185dd86b8acf",
       "long s length")
    ).map { case (sig, msg) => (ByteVector.fromValidHex(sig), msg) }

    signatures.foreach {
      case (sig, msg) =>
        DERSignatureUtil.parseDERLax(sig) match {
          case None => succeed
          case Some((r, s)) =>
            if (r == BigInt(0) || s == BigInt(0)) {
              succeed
            } else {
              fail(s"Successfully parsed bad signature $sig : $msg")
            }
        }
    }
  }
}
