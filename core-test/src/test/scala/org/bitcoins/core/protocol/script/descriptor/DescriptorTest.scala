package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script._
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
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

    val str1 =
      "multi(1,03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    val expected1 =
      "512103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea23552ae"
    runTest(str1, expected1)

    val str2 =
      "sortedmulti(1,04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235,03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected2 =
      "512103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea23552ae"
    runTest(str2, expected2)

    val str3 =
      "sh(multi(2,[00000000/111'/222]xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc,xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L/0))"
    val expected3 = "a91445a9a622a8b0a1269944be477640eedc447bbd8487"
    runTest(str3, expected3)

    val str4 =
      "sortedmulti(2,xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL/*,xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y/0/0/*)"
    val expected4 = Vector(
      "5221025d5fc65ebb8d44a5274b53bac21ff8307fec2334a32df05553459f8b1f7fe1b62102fbd47cc8034098f0e6a94c6aeee8528abf0a2153a5d8e46d325b7284c046784652ae",
      "52210264fd4d1f5dea8ded94c61e9641309349b62f27fbffe807291f664e286bfbe6472103f4ece6dfccfa37b211eb3d0af4d0c61dba9ef698622dc17eecdf764beeb005a652ae",
      "5221022ccabda84c30bad578b13c89eb3b9544ce149787e5b538175b1d1ba259cbb83321024d902e1a2fc7a8755ab5b694c575fce742c48d9ff192e63df5193e4c7afe1f9c52ae"
    )
    runMultisigDerivationTest(str4, expected4)

    val str5 =
      "wsh(multi(2,xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U/2147483647'/0,xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt/1/2/*,xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
    val expected5 = Vector(
      "0020b92623201f3bb7c3771d45b2ad1d0351ea8fbf8cfe0a0e570264e1075fa1948f",
      "002036a08bbe4923af41cf4316817c93b8d37e2f635dd25cfff06bd50df6ae7ea203",
      "0020a96e7ab4607ca6b261bfe3245ffda9c746b28d3f59e83d34820ec0e2b36c139c"
    )
    runMultisigDerivationTest(str5, expected5)

    val str6 =
      "sh(wsh(multi(16,03669b8afcec803a0d323e9a17f3ea8e68e8abe5a278020a929adbec52421adbd0,0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600,0362a74e399c39ed5593852a30147f2959b56bb827dfa3e60e464b02ccf87dc5e8,0261345b53de74a4d721ef877c255429961b7e43714171ac06168d7e08c542a8b8,02da72e8b46901a65d4374fe6315538d8f368557dda3a1dcf9ea903f3afe7314c8,0318c82dd0b53fd3a932d16e0ba9e278fcc937c582d5781be626ff16e201f72286,0297ccef1ef99f9d73dec9ad37476ddb232f1238aff877af19e72ba04493361009,02e502cfd5c3f972fe9a3e2a18827820638f96b6f347e54d63deb839011fd5765d,03e687710f0e3ebe81c1037074da939d409c0025f17eb86adb9427d28f0f7ae0e9,02c04d3a5274952acdbc76987f3184b346a483d43be40874624b29e3692c1df5af,02ed06e0f418b5b43a7ec01d1d7d27290fa15f75771cb69b642a51471c29c84acd,036d46073cbb9ffee90473f3da429abc8de7f8751199da44485682a989a4bebb24,02f5d1ff7c9029a80a4e36b9a5497027ef7f3e73384a4a94fbfe7c4e9164eec8bc,02e41deffd1b7cce11cde209a781adcffdabd1b91c0ba0375857a2bfd9302419f3,02d76625f7956a7fc505ab02556c23ee72d832f1bac391bcd2d3abce5710a13d06,0399eb0a5487515802dc14544cf10b3666623762fbed2ec38a3975716e2c29c232)))"
    val expected6 = "a9147fc63e13dc25e8a95a3cee3d9a714ac3afd96f1e87"
    runTest(str6, expected6)

    val str7 =
      "wsh(multi(20,KzoAz5CanayRKex3fSLQ2BwJpN7U52gZvxMyk78nDMHuqrUxuSJy,KwGNz6YCCQtYvFzMtrC6D3tKTKdBBboMrLTsjr2NYVBwapCkn7Mr,KxogYhiNfwxuswvXV66eFyKcCpm7dZ7TqHVqujHAVUjJxyivxQ9X,L2BUNduTSyZwZjwNHynQTF14mv2uz2NRq5n5sYWTb4FkkmqgEE9f,L1okJGHGn1kFjdXHKxXjwVVtmCMR2JA5QsbKCSpSb7ReQjezKeoD,KxDCNSST75HFPaW5QKpzHtAyaCQC7p9Vo3FYfi2u4dXD1vgMiboK,L5edQjFtnkcf5UWURn6UuuoFrabgDQUHdheKCziwN42aLwS3KizU,KzF8UWFcEC7BYTq8Go1xVimMkDmyNYVmXV5PV7RuDicvAocoPB8i,L3nHUboKG2w4VSJ5jYZ5CBM97oeK6YuKvfZxrefdShECcjEYKMWZ,KyjHo36dWkYhimKmVVmQTq3gERv3pnqA4xFCpvUgbGDJad7eS8WE,KwsfyHKRUTZPQtysN7M3tZ4GXTnuov5XRgjdF2XCG8faAPmFruRF,KzCUbGhN9LJhdeFfL9zQgTJMjqxdBKEekRGZX24hXdgCNCijkkap,KzgpMBwwsDLwkaC5UrmBgCYaBD2WgZ7PBoGYXR8KT7gCA9UTN5a3,KyBXTPy4T7YG4q9tcAM3LkvfRpD1ybHMvcJ2ehaWXaSqeGUxEdkP,KzJDe9iwJRPtKP2F2AoN6zBgzS7uiuAwhWCfGdNeYJ3PC1HNJ8M8,L1xbHrxynrqLKkoYc4qtoQPx6uy5qYXR5ZDYVYBSRmCV5piU3JG9,KzRedjSwMggebB3VufhbzpYJnvHfHe9kPJSjCU5QpJdAW3NSZxYS,Kyjtp5858xL7JfeV4PNRCKy2t6XvgqNNepArGY9F9F1SSPqNEMs3,L2D4RLHPiHBidkHS8ftx11jJk1hGFELvxh8LoxNQheaGT58dKenW,KyLPZdwY4td98bKkXqEXTEBX3vwEYTQo1yyLjX2jKXA63GBpmSjv))"
    val expected7 =
      "0020376bd8344b8b6ebe504ff85ef743eaa1aa9272178223bcb6887e9378efb341ac"
    runTest(str7, expected7)

    val str8 =
      "sh(wsh(multi(20,KzoAz5CanayRKex3fSLQ2BwJpN7U52gZvxMyk78nDMHuqrUxuSJy,KwGNz6YCCQtYvFzMtrC6D3tKTKdBBboMrLTsjr2NYVBwapCkn7Mr,KxogYhiNfwxuswvXV66eFyKcCpm7dZ7TqHVqujHAVUjJxyivxQ9X,L2BUNduTSyZwZjwNHynQTF14mv2uz2NRq5n5sYWTb4FkkmqgEE9f,L1okJGHGn1kFjdXHKxXjwVVtmCMR2JA5QsbKCSpSb7ReQjezKeoD,KxDCNSST75HFPaW5QKpzHtAyaCQC7p9Vo3FYfi2u4dXD1vgMiboK,L5edQjFtnkcf5UWURn6UuuoFrabgDQUHdheKCziwN42aLwS3KizU,KzF8UWFcEC7BYTq8Go1xVimMkDmyNYVmXV5PV7RuDicvAocoPB8i,L3nHUboKG2w4VSJ5jYZ5CBM97oeK6YuKvfZxrefdShECcjEYKMWZ,KyjHo36dWkYhimKmVVmQTq3gERv3pnqA4xFCpvUgbGDJad7eS8WE,KwsfyHKRUTZPQtysN7M3tZ4GXTnuov5XRgjdF2XCG8faAPmFruRF,KzCUbGhN9LJhdeFfL9zQgTJMjqxdBKEekRGZX24hXdgCNCijkkap,KzgpMBwwsDLwkaC5UrmBgCYaBD2WgZ7PBoGYXR8KT7gCA9UTN5a3,KyBXTPy4T7YG4q9tcAM3LkvfRpD1ybHMvcJ2ehaWXaSqeGUxEdkP,KzJDe9iwJRPtKP2F2AoN6zBgzS7uiuAwhWCfGdNeYJ3PC1HNJ8M8,L1xbHrxynrqLKkoYc4qtoQPx6uy5qYXR5ZDYVYBSRmCV5piU3JG9,KzRedjSwMggebB3VufhbzpYJnvHfHe9kPJSjCU5QpJdAW3NSZxYS,Kyjtp5858xL7JfeV4PNRCKy2t6XvgqNNepArGY9F9F1SSPqNEMs3,L2D4RLHPiHBidkHS8ftx11jJk1hGFELvxh8LoxNQheaGT58dKenW,KyLPZdwY4td98bKkXqEXTEBX3vwEYTQo1yyLjX2jKXA63GBpmSjv)))"
    val expected8 = "a914c2c9c510e9d7f92fd6131e94803a8d34a8ef675e87"
    runTest(str8, expected8)
  }

  it must "fail to parse invalid test vectors from BIP383" in {
    val str0 =
      "sh(multi(16,03669b8afcec803a0d323e9a17f3ea8e68e8abe5a278020a929adbec52421adbd0,0260b2003c386519fc9eadf2b5cf124dd8eea4c4e68d5e154050a9346ea98ce600,0362a74e399c39ed5593852a30147f2959b56bb827dfa3e60e464b02ccf87dc5e8,0261345b53de74a4d721ef877c255429961b7e43714171ac06168d7e08c542a8b8,02da72e8b46901a65d4374fe6315538d8f368557dda3a1dcf9ea903f3afe7314c8,0318c82dd0b53fd3a932d16e0ba9e278fcc937c582d5781be626ff16e201f72286,0297ccef1ef99f9d73dec9ad37476ddb232f1238aff877af19e72ba04493361009,02e502cfd5c3f972fe9a3e2a18827820638f96b6f347e54d63deb839011fd5765d,03e687710f0e3ebe81c1037074da939d409c0025f17eb86adb9427d28f0f7ae0e9,02c04d3a5274952acdbc76987f3184b346a483d43be40874624b29e3692c1df5af,02ed06e0f418b5b43a7ec01d1d7d27290fa15f75771cb69b642a51471c29c84acd,036d46073cbb9ffee90473f3da429abc8de7f8751199da44485682a989a4bebb24,02f5d1ff7c9029a80a4e36b9a5497027ef7f3e73384a4a94fbfe7c4e9164eec8bc,02e41deffd1b7cce11cde209a781adcffdabd1b91c0ba0375857a2bfd9302419f3,02d76625f7956a7fc505ab02556c23ee72d832f1bac391bcd2d3abce5710a13d06,0399eb0a5487515802dc14544cf10b3666623762fbed2ec38a3975716e2c29c232))"
    runFailTest(str0)

    val str1 =
      "multi(a,03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    runFailTest(str1)

    val str2 =
      "multi(0,03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    runFailTest(str2)

    val str3 =
      "multi(3,L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1,5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    runFailTest(str3)
  }

  it must "parse test vectors from BIP384" in {
    val str0 = "combo(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected0 = Vector(
      "2103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bdac",
      "76a9149a1c78a507689f6f54b847ad1cef1e614ee23f1e88ac",
      "00149a1c78a507689f6f54b847ad1cef1e614ee23f1e",
      "a91484ab21b1b2fd065d4504ff693d832434b6108d7b87"
    )
    runComboTest(str0, expected0)

    val str1 =
      "combo(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    val expected1 = Vector(
      "4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235ac",
      "76a914b5bd079c4d57cc7fc28ecf8213a6b791625b818388ac"
    )
    runComboTest(str1, expected1)

    val str2 =
      "combo([01234567]xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL)"
    val expected2 = Vector(
      "2102d2b36900396c9282fa14628566582f206a5dd0bcc8d5e892611806cafb0301f0ac",
      "76a91431a507b815593dfc51ffc7245ae7e5aee304246e88ac",
      "001431a507b815593dfc51ffc7245ae7e5aee304246e",
      "a9142aafb926eb247cb18240a7f4c07983ad1f37922687"
    )
    runComboTest(str2, expected2)

    val str3 =
      "combo(xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334/*)"
    val expected3 = Vector(
      Vector(
        "2102df12b7035bdac8e3bab862a3a83d06ea6b17b6753d52edecba9be46f5d09e076ac",
        "76a914f90e3178ca25f2c808dc76624032d352fdbdfaf288ac",
        "0014f90e3178ca25f2c808dc76624032d352fdbdfaf2",
        "a91408f3ea8c68d4a7585bf9e8bda226723f70e445f087"
      ),
      Vector(
        "21032869a233c9adff9a994e4966e5b821fd5bac066da6c3112488dc52383b4a98ecac",
        "76a914a8409d1b6dfb1ed2a3e8aa5e0ef2ff26b15b75b788ac",
        "0014a8409d1b6dfb1ed2a3e8aa5e0ef2ff26b15b75b7",
        "a91473e39884cb71ae4e5ac9739e9225026c99763e6687"
      )
    )

    runComboDerivationTest(str3, expected3)
  }

  it must "fail to parse invalid test vectors from BIP384" in {
    val str0 =
      "sh(combo(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str0)
    val str1 =
      "wsh(combo(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str1)
    val str2 =
      "combo(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str2)
  }

  it must "parse test vectors from BIP385" in {
    val str0 = "raw(deadbeef)"
    val expected0 = "deadbeef"
    runTest(str0, expected0)

    val str1 =
      "raw(512103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea23552ae)"
    val expected1 =
      "512103a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd4104a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea23552ae"
    runTest(str1, expected1)

    val str2 = "raw(a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87)"
    val expected2 = "a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87"
    runTest(str2, expected2)
  }

  it must "fail to parse invalid test vectors from BIP385" in {
    val str0 = "raw(asdf)"
    runFailTest(str0)
    val str1 = "addr(asdf)"
    runFailTest(str1)
    val str2 = "sh(raw(deadbeef))"
    runFailTest(str2)
    val str3 = "wsh(raw(deadbeef))"
    runFailTest(str3)
    val str4 = "sh(addr(3PUNyaW7M55oKWJ3kDukwk9bsKvryra15j))"
    runFailTest(str4)
    val str5 = "wsh(addr(3PUNyaW7M55oKWJ3kDukwk9bsKvryra15j))"
    runFailTest(str5)
  }

  it must "parse test vectors from BIP386" in {
    val str0 =
      "tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected0 =
      "512077aab6e066f8a7419c5ab714c12c67d25007ed55a43cadcacb4d7a970a093f11"
    runTest(str0, expected0)

    val str1 = "tr(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected1 =
      "512077aab6e066f8a7419c5ab714c12c67d25007ed55a43cadcacb4d7a970a093f11"
    runTest(str1, expected1)

    val str2 =
      "tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,pk(669b8afcec803a0d323e9a17f3ea8e68e8abe5a278020a929adbec52421adbd0))"
    val expected2 =
      "512017cf18db381d836d8923b1bdb246cfcd818da1a9f0e6e7907f187f0b2f937754"
    runTest(str2, expected2)

    val str3 =
      "tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd,{pk(xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334/0),{{pk(xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL),pk(df12b7035bdac8e3bab862a3a83d06ea6b17b6753d52edecba9be46f5d09e076)},pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)}})"
    val expected3 =
      "512071fff39599a7b78bc02623cbe814efebf1a404f5d8ad34ea80f213bd8943f574"
    runTest(str3, expected3)

    val str4 =
      "tr(xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/0/*,pk(xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc/1/*))"
    val expectedSPKs4 = Vector(
      "512078bc707124daa551b65af74de2ec128b7525e10f374dc67b64e00ce0ab8b3e12",
      "512001f0a02a17808c20134b78faab80ef93ffba82261ccef0a2314f5d62b6438f11",
      "512021024954fcec88237a9386fce80ef2ced5f1e91b422b26c59ccfc174c8d1ad25"
    )
    runDerivationTest(str4, expectedSPKs4)
  }

  it must "fail to parse invalid test vectors from BIP386" in {
    val str0 = "tr(5kyzdueo39z3fprtux2qbbwgnnp5ztd7yyr2sc1j299sbcnwjss)"
    runFailTest(str0)

    val str1 =
      "tr(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    runFailTest(str1)

    val str2 =
      "wsh(tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str2)

    val str3 =
      "sh(tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str3)

    val str4 =
      "tr(a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd, pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    runFailTest(str4)
  }

  def runTest(descriptor: String, expectedSPK: String): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    assert(desc.toString == descriptor)
    assert(desc.scriptPubKey.asmHex == expectedSPK)
  }

  @tailrec
  private def parseExtKeyExpression(
      expression: ScriptExpression): ExtECPublicKeyExpression = {
    expression match {
      case x: KeyExpressionScriptExpression[_] =>
        x.source match {
          case ecPublic: ExtECPublicKeyExpression => ecPublic
          case xonly: ExtXOnlyPublicKeyExpression =>
            xonly.ecPublicKeyExpression
          case invalid @ (_: RawPrivateECPublicKeyExpression |
              _: RawPublicECPublicKeyExpression |
              _: RawPrivateXOnlyPublicKeyExpression |
              _: RawPrivateXOnlyPublicKeyExpression |
              _: InternalPublicKeyExpression | _: MultisigKeyExpression) =>
            sys.error(s"Cannot have single key in parseExtKey, got=$invalid")
        }
      case x: NestedScriptExpression =>
        parseExtKeyExpression(x.source)
      case x: RawScriptExpression =>
        sys.error(
          s"RawScriptExpression cannot be used in runDerivationTest(), got=$x")
      case x: ScriptPathTreeExpression =>
        x.source.leafs.head.source
          .asInstanceOf[P2PKScriptExpression[_]]
          .source
          .asInstanceOf[ExtXOnlyPublicKeyExpression]
          .ecPublicKeyExpression
    }
  }

  def runDerivationTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    assert(desc.toString == descriptor)
    val extKeyDesc = parseExtKeyExpression(desc.expression)
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      val expected = ScriptPubKey.fromAsmHex(s)
      val derivedKey = extKeyDesc match {
        case xprv: XprvECPublicKeyExpression => xprv.deriveChild(idx).publicKey
        case xpub: XpubECPublicKeyExpression => xpub.deriveChild(idx)
      }

      val p2wpkh = P2WPKHWitnessSPKV0(derivedKey)
      val spk = desc.expression.descriptorType match {
        case DescriptorType.WPKH => p2wpkh
        case DescriptorType.SH   => P2SHScriptPubKey(p2wpkh)
        case DescriptorType.TR =>
          val scriptPathTreeExpr =
            desc.expression.asInstanceOf[ScriptPathTreeExpression]
          val internalExtKey = parseExtKeyExpression(scriptPathTreeExpr.keyPath)
          val internal = internalExtKey.deriveChild(idx) match {
            case priv: ECPrivateKey => priv.toXOnly
            case pub: ECPublicKey   => pub.toXOnly
          }
          val tree = TapLeaf(0xc0, P2PKScriptPubKey(derivedKey.toXOnly))
          val (_, spk) =
            TaprootScriptPubKey.fromInternalKeyTapscriptTree(internal, tree)
          spk
        case x => sys.error(s"Not supported by BIP382, got=$x")
      }
      assert(spk == expected)

    }
    succeed
  }

  private def runMultisigDerivationTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc: ScriptDescriptor = ScriptDescriptor.fromString(descriptor)
    val expression: MultisigScriptExpression = desc.expression match {
      case m: MultisigScriptExpression => m
      case nested: NestedScriptExpression =>
        nested.source.asInstanceOf[MultisigScriptExpression]
      case x => sys.error(s"Invalid expression=$x")
    }
    val spk = expression.scriptPubKey
    val extKeyExprs: Vector[ExtECPublicKeyExpression] =
      expression.source.keyExpressions.collect {
        case extKey: ExtECPublicKeyExpression => extKey
      }
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      val derivedKeys = extKeyExprs.map {
        case xprv: XprvECPublicKeyExpression =>
          if (xprv.childrenHardenedOpt.isDefined)
            xprv.deriveChild(idx).publicKey
          else xprv.key.publicKeyBytes.toPublicKey
        case xpub: XpubECPublicKeyExpression =>
          if (xpub.childrenHardenedOpt.isDefined) xpub.deriveChild(idx)
          else xpub.key.toPublicKey
      }
      val sortedKeys =
        if (expression.isSorted) derivedKeys.sortBy(_.hex) else derivedKeys
      val multisig =
        MultiSignatureScriptPubKey(spk.requiredSigs, sortedKeys)
      val (actual, expected) = {
        desc.expression.descriptorType match {
          case DescriptorType.Multi | DescriptorType.SortedMulti =>
            (multisig, MultiSignatureScriptPubKey.fromAsmHex(s))
          case DescriptorType.SH =>
            (P2SHScriptPubKey(multisig), P2SHScriptPubKey.fromAsmHex(s))
          case DescriptorType.WSH =>
            (P2WSHWitnessSPKV0(multisig), P2WSHWitnessSPKV0.fromAsmHex(s))
          case x =>
            sys.error(
              s"Invalid descriptor type=$x for runMultisigDerivationTest()")
        }

      }

      assert(actual == expected)
      assert(desc.toString == descriptor)
    }
    succeed
  }

  private def runFailTest(str: String): Assertion = {
    assertThrows[RuntimeException] {
      ScriptDescriptor.fromString(str)
    }
  }

  private def runComboTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc = ComboDescriptor.fromString(descriptor)
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      if (idx == 0) {
        val expected = P2PKScriptPubKey.fromAsmHex(s)
        assert(desc.p2pk == expected)
      } else if (idx == 1) {
        val expected = P2PKHScriptPubKey.fromAsmHex(s)
        assert(desc.p2pkh == expected)
      } else if (idx == 2) {
        val expected = P2WPKHWitnessSPKV0.fromAsmHex(s)
        assert(desc.isInstanceOf[ComboDescriptorCompressed])
        val compressed = desc.asInstanceOf[ComboDescriptorCompressed]
        assert(compressed.p2wpkh == expected)
      } else if (idx == 3) {
        val expected = P2SHScriptPubKey.fromAsmHex(s)
        assert(desc.isInstanceOf[ComboDescriptorCompressed])
        val compressed = desc.asInstanceOf[ComboDescriptorCompressed]
        assert(compressed.p2shp2wpkh == expected)
      } else {
        fail(s"Unknown index for desc=$desc")
      }
    }
    succeed
  }

  private def runComboDerivationTest(
      descriptor: String,
      expectedSPKsNested: Vector[Vector[String]]): Assertion = {
    val desc = ComboDescriptor.fromString(descriptor)
    expectedSPKsNested.zipWithIndex.foreach { case (expectedSPKs, idx) =>
      val extKey = desc.expression.source.asInstanceOf[ExtECPublicKeyExpression]
      val pubKey = extKey.deriveChild(idx) match {
        case priv: ECPrivateKey => priv.publicKey
        case pub: ECPublicKey   => pub
      }
      expectedSPKs.zipWithIndex.foreach { case (s, nestedIdx) =>
        if (nestedIdx == 0) {
          val expected = P2PKScriptPubKey.fromAsmHex(s)
          assert(P2PKScriptPubKey(pubKey) == expected)
        } else if (nestedIdx == 1) {
          val expected = P2PKHScriptPubKey.fromAsmHex(s)
          assert(P2PKHScriptPubKey(pubKey) == expected)
        } else if (nestedIdx == 2) {
          val expected = P2WPKHWitnessSPKV0.fromAsmHex(s)
          assert(desc.isInstanceOf[ComboDescriptorCompressed])
          assert(P2WPKHWitnessSPKV0(pubKey) == expected)
        } else if (nestedIdx == 3) {
          val expected = P2SHScriptPubKey.fromAsmHex(s)
          assert(desc.isInstanceOf[ComboDescriptorCompressed])
          assert(P2SHScriptPubKey(P2WPKHWitnessSPKV0(pubKey)) == expected)
        } else {
          fail(s"Unknown index for desc=$desc")
        }
      }
    }
    succeed
  }
}
