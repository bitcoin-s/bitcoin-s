package org.bitcoins.core.crypto

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

/** Tests from https://github.com/sipa/bips/blob/bip-taproot/bip-0340/test-vectors.csv */
class BIP340Test extends BitcoinSUnitTest {
  behavior of "Schnorr Signing"

  def testSign(
      index: Int,
      secKey: ECPrivateKey,
      auxRand: ByteVector,
      msg: ByteVector,
      expectedSig: SchnorrDigitalSignature): Assertion = {
    val sig = secKey.schnorrSign(msg, auxRand)
    assert(sig == expectedSig, s"Test $index failed signing")
  }

  def testVerify(
      index: Int,
      pubKey: ECPublicKey,
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      expectedResult: Boolean,
      comment: String): Assertion = {
    val result = pubKey.schnorrVerify(msg, sig)
    assert(result == expectedResult,
           s"Test $index failed verification: $comment")
  }

  def test(
      index: Int,
      secKeyOpt: Option[String],
      pubKey: String,
      auxRandOpt: Option[String],
      msg: String,
      sig: String,
      result: Boolean,
      comment: String): Assertion = {
    val pk = ECPublicKey(s"02$pubKey") // TODO: Introduce x-only pubkey type
    val msgBytes = ByteVector.fromHex(msg).get
    val schnorrSig = SchnorrDigitalSignature(sig)

    (secKeyOpt, auxRandOpt) match {
      case (Some(secKeyStr), Some(auxRandStr)) =>
        val secKey = ECPrivateKey(secKeyStr)
        assert(secKey.publicKey.bytes.tail == pk.bytes.tail)
        val auxRand = ByteVector.fromHex(auxRandStr).get
        testSign(index, secKey, auxRand, msgBytes, schnorrSig)
      case _ => ()
    }

    testVerify(index, pk, msgBytes, schnorrSig, result, comment)
  }

  it must "pass the BIP 340 test-vectors" in {
    test(
      index = 0,
      secKeyOpt = Some(
        "0000000000000000000000000000000000000000000000000000000000000003"),
      pubKey =
        "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9",
      auxRandOpt = Some(
        "0000000000000000000000000000000000000000000000000000000000000000"),
      msg = "0000000000000000000000000000000000000000000000000000000000000000",
      sig =
        "067E337AD551B2276EC705E43F0920926A9CE08AC68159F9D258C9BBA412781C9F059FCDF4824F13B3D7C1305316F956704BB3FEA2C26142E18ACD90A90C947E",
      result = true,
      comment = ""
    )

    test(
      index = 1,
      secKeyOpt = Some(
        "B7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF"),
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = Some(
        "0000000000000000000000000000000000000000000000000000000000000001"),
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "0E12B8C520948A776753A96F21ABD7FDC2D7D0C0DDC90851BE17B04E75EF86A47EF0DA46C4DC4D0D1BCB8668C2CE16C54C7C23A6716EDE303AF86774917CF928",
      result = true,
      comment = ""
    )

    test(
      index = 2,
      secKeyOpt = Some(
        "C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B14E5C9"),
      pubKey =
        "DD308AFEC5777E13121FA72B9CC1B7CC0139715309B086C960E18FD969774EB8",
      auxRandOpt = Some(
        "C87AA53824B4D7AE2EB035A2B5BBBCCC080E76CDC6D1692C4B0B62D798E6D906"),
      msg = "7E2D58D8B3BCDF1ABADEC7829054F90DDA9805AAB56C77333024B9D0A508B75C",
      sig =
        "FC012F9FB8FE00A358F51EF93DCE0DC0C895F6E9A87C6C4905BC820B0C3677616B8737D14E703AF8E16E22E5B8F26227D41E5128F82D86F747244CC289C74D1D",
      result = true,
      comment = ""
    )

    test(
      index = 3,
      secKeyOpt = Some(
        "0B432B2677937381AEF05BB02A66ECD012773062CF3FA2549E44F58ED2401710"),
      pubKey =
        "25D1DFF95105F5253C4022F628A996AD3A0D95FBF21D468A1B33F8C160D8F517",
      auxRandOpt = Some(
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"),
      msg = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      sig =
        "FC132D4E426DFF535AEC0FA7083AC5118BC1D5FFFD848ABD8290C23F271CA0DD11AEDCEA3F55DA9BD677FE29C9DDA0CF878BCE43FDE0E313D69D1AF7A5AE8369",
      result = true,
      comment = "test fails if msg is reduced modulo p or n"
    )

    test(
      index = 4,
      secKeyOpt = None,
      pubKey =
        "D69C3509BB99E412E68B0FE8544E72837DFA30746D8BE2AA65975F29D22DC7B9",
      auxRandOpt = None,
      msg = "4DF3C3F68FCC83B27E9D42C90431A72499F17875C81A599B566C9889B9696703",
      sig =
        "00000000000000000000003B78CE563F89A0ED9414F5AA28AD0D96D6795F9C630EC50E5363E227ACAC6F542CE1C0B186657E0E0D1A6FFE283A33438DE4738419",
      result = true,
      comment = ""
    )

    test(
      index = 5,
      secKeyOpt = None,
      pubKey =
        "EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "7036D6BFE1837AE919631039A2CF652A295DFAC9A8BBB0806014B2F48DD7C807941607B563ABBA414287F374A332BA3636DE009EE1EF551A17796B72B68B8A24",
      result = false,
      comment = "public key not on the curve"
    )

    test(
      index = 6,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F995A579DA959FA739FCE39E8BD16FECB5CDCF97060B2C73CDE60E87ABCA1AA5D9",
      result = false,
      comment = "has_square_y(R) is false"
    )

    test(
      index = 7,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "F8704654F4687B7365ED32E796DE92761390A3BCC495179BFE073817B7ED32824E76B987F7C1F9A751EF5C343F7645D3CFFC7D570B9A7192EBF1898E1344E3BF",
      result = false,
      comment = "negated message"
    )

    test(
      index = 8,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "7036D6BFE1837AE919631039A2CF652A295DFAC9A8BBB0806014B2F48DD7C8076BE9F84A9C5445BEBD780C8B5CCD45C883D0DC47CD594B21A858F31A19AAB71D",
      result = false,
      comment = "negated s value"
    )

    test(
      index = 9,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "00000000000000000000000000000000000000000000000000000000000000009915EE59F07F9DBBAEDC31BFCC9B34AD49DE669CD24773BCED77DDA36D073EC8",
      result = false,
      comment =
        "sG - eP is infinite. Test fails in single verification if has_square_y(inf) is defined as true and x(inf) as 0"
    )

    test(
      index = 10,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "0000000000000000000000000000000000000000000000000000000000000001C7EC918B2B9CF34071BB54BED7EB4BB6BAB148E9A7E36E6B228F95DFA08B43EC",
      result = false,
      comment =
        "sG - eP is infinite. Test fails in single verification if has_square_y(inf) is defined as true and x(inf) as 1"
    )

    test(
      index = 11,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "4A298DACAE57395A15D0795DDBFD1DCB564DA82B0F269BC70A74F8220429BA1D941607B563ABBA414287F374A332BA3636DE009EE1EF551A17796B72B68B8A24",
      result = false,
      comment = "sig[0:32] is not an X coordinate on the curve"
    )

    test(
      index = 12,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F941607B563ABBA414287F374A332BA3636DE009EE1EF551A17796B72B68B8A24",
      result = false,
      comment = "sig[0:32] is equal to field size"
    )

    test(
      index = 13,
      secKeyOpt = None,
      pubKey =
        "DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "7036D6BFE1837AE919631039A2CF652A295DFAC9A8BBB0806014B2F48DD7C807FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141",
      result = false,
      comment = "sig[32:64] is equal to curve order"
    )

    test(
      index = 14,
      secKeyOpt = None,
      pubKey =
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30",
      auxRandOpt = None,
      msg = "243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89",
      sig =
        "7036D6BFE1837AE919631039A2CF652A295DFAC9A8BBB0806014B2F48DD7C807941607B563ABBA414287F374A332BA3636DE009EE1EF551A17796B72B68B8A24",
      result = false,
      comment =
        "public key is not a valid X coordinate because it exceeds the field size"
    )
  }
}
