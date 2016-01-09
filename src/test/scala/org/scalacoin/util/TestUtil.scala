package org.scalacoin.util

import org.scalacoin.protocol.{AssetAddress, BitcoinAddress}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant.{OP_0, ScriptConstantImpl}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP

/**
 * Created by chris on 12/2/15.
 */
object TestUtil {

  val testBitcoinAddress = BitcoinAddress("n3p1ct69ao3qxWvEvzLhLtWG2zJGTjN3EV")
  val testP2SHAddress = BitcoinAddress("2MzYbQdkSVp5wVyMRp6A5PHPuQNHpiaTbCj")
  val bitcoinAddress = BitcoinAddress("1C4kYhyLftmkn48YarSoLupxHfYFo8kp64")
  val multiSigAddress = BitcoinAddress("342ftSRCvFHfCeFFBuz4xwbeqnDw6BGUey")
  val assetAddress = AssetAddress("akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")

  val p2pkhInputScriptNotParsedAsm =
    "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01" +
      " 02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"
  val p2pkhInputScriptAsm = List(
    ScriptConstantImpl("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01"),
    ScriptConstantImpl("02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"))
  val p2pkhOutputScriptNotParsedAsm = "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
  val p2pkhOutputScriptAsm = List(OP_DUP,OP_HASH160,ScriptConstantImpl("e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b"),OP_EQUALVERIFY, OP_CHECKSIG)


  val p2shInputScriptNotParsedAsm =
    """0 3045022100884c8a4776f4aa2a70f25f6bc0071929ade0ff4987844347e051e018c267aae402201fcec5dd052e7b01198bb57e1b58696c38ccd9d0b408c55047cac89b47287b4101 3045022100b064d492712a080b726ecf37de2957b783fa411edae33bd13005e62d6a45d02302201b82b632df54cf1204758c2b5a3599f1f9a80da3d508951695bfcf8d2c2cce0f01 522102632178d046673c9729d828cfee388e121f497707f810c131e0d3fc0fe0bd66d62103a0951ec7d3a9da9de171617026442fcd30f34d66100fab539853b43f508787d452ae"""

  val p2shInputScriptAsm = List(
    OP_0,
    ScriptConstantImpl("3045022100884c8a4776f4aa2a70f25f6bc0071929ade0ff4987844347e051e018c267aae402201fcec5dd052e7b01198bb57e1b58696c38ccd9d0b408c55047cac89b47287b4101"),
    ScriptConstantImpl("3045022100b064d492712a080b726ecf37de2957b783fa411edae33bd13005e62d6a45d02302201b82b632df54cf1204758c2b5a3599f1f9a80da3d508951695bfcf8d2c2cce0f01"),
    ScriptConstantImpl("522102632178d046673c9729d828cfee388e121f497707f810c131e0d3fc0fe0bd66d62103a0951ec7d3a9da9de171617026442fcd30f34d66100fab539853b43f508787d452ae")
  )
  val p2shOutputScriptNotParsedAsm = "OP_HASH160 8ce5408cfeaddb7ccb2545ded41ef47810945484 OP_EQUAL"
  val p2shOutputScriptAsm = List(OP_HASH160, ScriptConstantImpl("8ce5408cfeaddb7ccb2545ded41ef47810945484"), OP_EQUAL)

}
