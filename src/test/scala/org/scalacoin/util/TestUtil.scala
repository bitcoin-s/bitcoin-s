package org.scalacoin.util

import org.scalacoin.protocol.{AssetAddress, BitcoinAddress}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant.{ScriptToken, OP_0, ScriptConstantImpl}
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

  val p2pkhInputScript = "473044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac012102af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"
  val p2pkhInputScriptNotParsedAsm =
    "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01" +
      " 02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"
  val p2pkhInputScriptAsm : List[ScriptToken] = List(
    ScriptConstantImpl("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01"),
    ScriptConstantImpl("02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"))

  val p2pkhOutputScript = "76a914e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b88ac"
  val p2pkhOutputScriptNotParsedAsm = "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
  val p2pkhOutputScriptAsm = List(OP_DUP,OP_HASH160,ScriptConstantImpl("e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b"),OP_EQUALVERIFY, OP_CHECKSIG)


  //tx id for p2sh inputs/outputs cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val p2shInputScriptNotParsedAsm =
    "0 304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001 512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae"

  val p2shInputScript = "0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae"
  val p2shInputScriptAsm = List(
    OP_0,
    ScriptConstantImpl("304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001"),
    ScriptConstantImpl("512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae")
  )

  val p2shOutputScript = "a914eda8ae08b5c9f973f49543e90a7c292367b3337c87"
  val p2shOutputScriptNotParsedAsm = "OP_HASH160 eda8ae08b5c9f973f49543e90a7c292367b3337c OP_EQUAL"
  val p2shOutputScriptAsm = List(OP_HASH160, ScriptConstantImpl("eda8ae08b5c9f973f49543e90a7c292367b3337c"), OP_EQUAL)

}
