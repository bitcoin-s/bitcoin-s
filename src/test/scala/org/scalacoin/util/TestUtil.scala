package org.scalacoin.util

import org.scalacoin.protocol.{AssetAddress, BitcoinAddress}
import org.scalacoin.script.ConstantImpl
import org.scalacoin.script.bitwise.OP_EQUALVERIFY
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

  val p2pkhInputScriptNotParsed =
    "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01" +
      " 02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"
  val p2pkhInputScript = List("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01",
    "02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652")
  val p2pkhOutputScriptNotParsed = "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"
  val p2pkhOutputScript = List(OP_DUP,OP_HASH160,ConstantImpl("e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b"),OP_EQUALVERIFY, OP_CHECKSIG)
}
