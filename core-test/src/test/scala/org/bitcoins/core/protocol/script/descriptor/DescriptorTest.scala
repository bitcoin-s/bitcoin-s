package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptPubKey}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion

class DescriptorTest extends BitcoinSUnitTest {

  behavior of "OutputDescriptor"

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

//    val str3 =
//      "wpkh([ffffffff/13']xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH/1/2/*)"
//    val expected3 = Vector("0014326b2249e3a25d5dc60935f044ee835d090ba859",
//                           "0014af0bd98abc2f2cae66e36896a39ffe2d32984fb7",
//                           "00141fa798efd1cbf95cebf912c031b8a4a6e9fb9f27")
//    runDerivationTest(str3, expected3)

//    val str4 = "sh(wpkh(xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
//    val expected4 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87", "a914bed59fc0024fae941d6e20a3b44a109ae740129287", "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
//    runDerivationTest(str4,expected4)

//    val str5 = "sh(wpkh(xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8/10/20/30/40/*h))"
//    val expected5 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87",
//      "a914bed59fc0024fae941d6e20a3b44a109ae740129287",
//     "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
//    runDerivationTest(str5,expected5)

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

  def runTest(descriptor: String, expectedSPK: String): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val expected = ScriptPubKey.fromAsmHex(expectedSPK)
    assert(desc.scriptPubKey == expected)
    assert(desc.toString == descriptor)
  }

  def runDerivationTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val extKeyDesc = desc.expression.asInstanceOf[ExtKeyExpression]
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      val expected = ScriptPubKey.fromAsmHex(s)
      val derivedKey = extKeyDesc.extKey.deriveChildPubKey(idx).get.key
      assert(P2WPKHWitnessSPKV0(derivedKey) == expected)
    }
    succeed
  }
}
