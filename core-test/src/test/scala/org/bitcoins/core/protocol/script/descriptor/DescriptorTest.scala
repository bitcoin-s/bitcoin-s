package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion

import scala.annotation.tailrec

class DescriptorTest extends BitcoinSUnitTest {

  behavior of "OutputDescriptor"

  it must "parse valid descriptors in BIP381" in {
    val str0 = "pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected0 =
      "2103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bdac"
    runTest(str0, expected0)

    val str1 =
      "pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected1 =
      "2103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bdac"
    runTest(str1, expected1)

    val str2 =
      "pkh([deadbeef/1/2'/3/4']L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected2 = "76a9149a1c78a507689f6f54b847ad1cef1e614ee23f1e88ac"
    runTest(str2, expected2)

    val str3 =
      "pkh([deadbeef/1/2'/3/4']03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected3 = "76a9149a1c78a507689f6f54b847ad1cef1e614ee23f1e88ac"
    runTest(str3, expected3)

    val str4 =
      "pk(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    val expected4 =
      "4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235ac"
    runTest(str4, expected4)

    val str5 =
      "pk(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    val expected5 =
      "4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235ac"
    runTest(str5, expected5)

    val str6 = "pkh(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    val expected6 = "76a914b5bd079c4d57cc7fc28ecf8213a6b791625b818388ac"
    runTest(str6, expected6)

    val str7 =
      "pkh(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    val expected7 = "76a914b5bd079c4d57cc7fc28ecf8213a6b791625b818388ac"
    runTest(str7, expected7)

    val str8 = "sh(pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected8 = "a9141857af51a5e516552b3086430fd8ce55f7c1a52487"
    runTest(str8, expected8)

    val str9 =
      "sh(pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected9 = "a9141857af51a5e516552b3086430fd8ce55f7c1a52487"
    runTest(str9, expected9)

    val str10 = "sh(pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected10 = "a9141a31ad23bf49c247dd531a623c2ef57da3c400c587"
    runTest(str10, expected10)

    val str11 =
      "sh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected11 = "a9141a31ad23bf49c247dd531a623c2ef57da3c400c587"
    runTest(str11, expected11)

    val str12 =
      "pkh(xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U/2147483647'/0)"
    val expected12 = "76a914ebdc90806a9c4356c1c88e42216611e1cb4c1c1788ac"
    runTest(str12, expected12)

    //invalid hardened derivation, needs to be removed from BIP381
    /*    val str13 =
      "pkh(xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB/2147483647'/0)"
    val expected13 = "76a914ebdc90806a9c4356c1c88e42216611e1cb4c1c1788ac"
    runTest(str13, expected13)*/

    val str13 =
      "pkh([bd16bee5/2147483647']xpub69H7F5dQzmVd3vPuLKtcXJziMEQByuDidnX3YdwgtNsecY5HRGtAAQC5mXTt4dsv9RzyjgDjAQs9VGVV6ydYCHnprc9vvaA5YtqWyL6hyds/0)"
    val expected13 = "76a914ebdc90806a9c4356c1c88e42216611e1cb4c1c1788ac"
    runTest(str13, expected13)

    val str14 =
      "pk(xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L/0)"
    val expected14 =
      "210379e45b3cf75f9c5f9befd8e9506fb962f6a9d185ac87001ec44a8d3df8d4a9e3ac"
    runTest(str14, expected14)

    val str15 =
      "pk(xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y/0)"
    val expected15 =
      "210379e45b3cf75f9c5f9befd8e9506fb962f6a9d185ac87001ec44a8d3df8d4a9e3ac"
    runTest(str15, expected15)
  }

  it must "fail to parse invalid test vectors from BIP381" in {
    val str0 =
      "pk(pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str0)
    val str1 =
      "pkh(pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str1)
    val str2 =
      "sh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    runFailTest(str2)
    val str3 =
      "sh(sh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    runFailTest(str3)
  }
  it must "parse valid descriptors in BIP382" in {
    val str0 = "wpkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected0 = "00149a1c78a507689f6f54b847ad1cef1e614ee23f1e"
    runTest(str0, expected0)

    val str1 =
      "wpkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected1 = "00149a1c78a507689f6f54b847ad1cef1e614ee23f1e"
    runTest(str1, expected1)

    val str2 =
      "wpkh([ffffffff/13']xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt/1/2/0)"
    val expected2 = "0014326b2249e3a25d5dc60935f044ee835d090ba859"
    runTest(str2, expected2)

    val str3 =
      "wpkh([ffffffff/13']xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH/1/2/*)"
    val expected3 = Vector("0014326b2249e3a25d5dc60935f044ee835d090ba859",
                           "0014af0bd98abc2f2cae66e36896a39ffe2d32984fb7",
                           "00141fa798efd1cbf95cebf912c031b8a4a6e9fb9f27")
    runDerivationTest(str3, expected3)

    val str4 =
      "sh(wpkh(xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
    val expected4 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87",
                           "a914bed59fc0024fae941d6e20a3b44a109ae740129287",
                           "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
    runDerivationTest(str4, expected4)

    val str5 =
      "sh(wpkh(xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
    val expected5 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87",
                           "a914bed59fc0024fae941d6e20a3b44a109ae740129287",
                           "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
    runDerivationTest(str5, expected5)

    val str6 = "wsh(pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected6 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str6, expected6)

    val str7 =
      "wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected7 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str7, expected7)

    val str8 = "wsh(pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected8 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str8, expected8)

    val str9 =
      "wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected9 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str9, expected9)

    val str10 = "wsh(pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected10 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str10, expected10)

    val str11 =
      "wsh(pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected11 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str11, expected11)

    val str12 =
      "sh(wsh(pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)))"
    val expected12 = "a914b61b92e2ca21bac1e72a3ab859a742982bea960a87"
    runTest(str12, expected12)

    val str13 =
      "sh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    val expected13 = "a914b61b92e2ca21bac1e72a3ab859a742982bea960a87"
    runTest(str13, expected13)

  }

  it must "fail to parse invalid test vectors from BIP382" in {
    val str0 = "wpkh(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    runFailTest(str0)
    val str1 = "sh(wpkh(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss))"
    runFailTest(str1)
    val str2 =
      "wpkh(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    runFailTest(str2)
    val str3 =
      "sh(wpkh(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235))"
    runFailTest(str3)
    val str4 = "wsh(pk(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss))"
    runFailTest(str4)
    val str5 =
      "wsh(pk(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235))"
    runFailTest(str5)
    val str6 =
      "wsh(wpkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str6)
    val str7 =
      "wsh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    runFailTest(str7)
    val str8 =
      "sh(wsh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))))"
    runFailTest(str8)
    val str9 =
      "wpkh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    runFailTest(str9)
    val str10 =
      "wsh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    runFailTest(str10)
  }

  it must "parse test vectors from BIP383" in {
    val str0 =
      "multi(1,L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1,5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    val expected0 =
      "512103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea23552ae"
    runTest(str0, expected0)
  }

  def runTest(descriptor: String, expectedSPK: String): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val expected = ScriptPubKey.fromAsmHex(expectedSPK)
    assert(desc.scriptPubKey == expected)
    assert(desc.toString == descriptor)
  }

  @tailrec
  private def parseExtKeyExpression(
      expression: ScriptExpression): ExtKeyExpression = {
    expression match {
      case x: KeyExpressionScriptExpression =>
        x.source.asInstanceOf[ExtKeyExpression]
      case x: NestedScriptExpression =>
        parseExtKeyExpression(x.source)
      case x: RawScriptExpression =>
        sys.error(
          s"RawScriptExpression cannot be used in runDerivationTest(), got=$x")
    }
  }

  def runDerivationTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val extKeyDesc = parseExtKeyExpression(desc.expression)
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      val expected = ScriptPubKey.fromAsmHex(s)
      val derivedKey = extKeyDesc match {
        case xprv: XprvKeyExpression => xprv.deriveChild(idx).publicKey
        case xpub: XpubKeyExpression => xpub.deriveChild(idx)
      }

      val p2wpkh = P2WPKHWitnessSPKV0(derivedKey)
      val spk = desc.expression.descriptorType match {
        case DescriptorType.WPKH => p2wpkh
        case DescriptorType.SH   => P2SHScriptPubKey(p2wpkh)
        case DescriptorType.WSH  => P2WSHWitnessSPKV0(p2wpkh)
        case x                   => sys.error(s"Not supported by BIP382, got=$x")
      }
      assert(spk == expected)
    }
    succeed
  }

  private def runFailTest(str: String): Assertion = {
    assertThrows[RuntimeException] {
      KeyExpression.fromString(str)
    }
  }
}
