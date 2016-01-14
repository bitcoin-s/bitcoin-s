package org.scalacoin.util

import org.scalacoin.marshallers.script.RawScriptPubKeyParser
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

  //txid on testnet 44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff
  val rawTransaction = "01000000" +
    "02df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd040011b0000006a473044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26012102c114f376c98d12a0540c3a81ab99bb1c5234245c05e8239d09f48229f9ebf011ffffffff" +
    "df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd04001340000006b483045022100cf317c320d078c5b884c44e7488825dab5bcdf3f88c66314ac925770cd8773a7022033fde60d33cc2842ea73fce5d9cf4f8da6fadf414a75b7085efdcd300407f438012102605c23537b27b80157c770cd23e066cd11db3800d3066a38b9b592fc08ae9c70ffffffff" +
    "02c02b00000000000017a914b0b06365c482eb4eabe6e0630029fb8328ea098487e81c0000000000001976a914938da2b50fd6d8acdfa20e30df0e7d8092f0bc7588ac00000000"


  //scriptPubKey taken from https://bitcoin.org/en/developer-reference#raw-transaction-format
  val rawScriptPubKey = "1976a914cbc20a7664f2f69e5355aa427045bc15e7c6c77288ac"
  val scriptPubKey = RawScriptPubKeyParser.read(rawScriptPubKey)
}
