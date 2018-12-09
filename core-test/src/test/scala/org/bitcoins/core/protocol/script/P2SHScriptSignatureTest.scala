package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.script.constant.{
  BytesToPushOntoStack,
  OP_0,
  ScriptConstant
}
import org.bitcoins.core.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 3/8/16.
  */
class P2SHScriptSignatureTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger

  "P2SHScriptSignature" must "find the public keys embedded inside of the redeemScript" in {
    val rawP2SHScriptSig = TestUtil.rawP2shInputScript2Of2
    val p2shScriptSig: P2SHScriptSignature =
      ScriptSignature(rawP2SHScriptSig) match {
        case x: P2SHScriptSignature => x
        case y                      => throw new RuntimeException("Must be p2sh script sig: " + y)
      }
    p2shScriptSig.publicKeys must be(
      Seq(
        ECPublicKey(
          "0369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b83"),
        ECPublicKey(
          "02480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f721")
      ))

  }

  it must "return a p2sh scriptSig with no serialized redeemScript" in {

    val p2shScriptSig = TestUtil.p2shInputScript2Of2 match {
      case s: P2SHScriptSignature => s
      case _                      => throw new RuntimeException("Should be p2sh scriptSig")
    }

    p2shScriptSig.scriptSignatureNoRedeemScript.asm must be(
      Seq(
        OP_0,
        BytesToPushOntoStack(71),
        ScriptConstant(
          "304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101"),
        BytesToPushOntoStack(72),
        ScriptConstant(
          "3045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c801")
      ))
  }

  it must "coreclty parse a EscrowTimeoutScriptSig and it's redeem script" in {
    val hex =
      "ba00473044022051737de0cf47b6c367011ea0a9f164e878c228bbea1f4ea1b9a98183c24c34ce022050455ca8729d2275d3d9d4dfb712703c8e2ecd0adfe34fa333cf3b020c8e15d683514c6e63522103ebbcabd4878fe28998d518324587b3950eb868ef0bd25ff45e0a3649ee630c3b21038333b9faf7629d8b2e514ea8ec0d0645774a3c0ea66d797d45607069b0ec08c952ae67089ceb36b39e806a65b27576a91429dd76a8b34f5d6eafe1f24e3b255768ea28310b88ac68"
    val p2sh = P2SHScriptSignature(hex)
    p2sh.redeemScript.isInstanceOf[EscrowTimeoutScriptPubKey] must be(true)
    p2sh.scriptSignatureNoRedeemScript
      .isInstanceOf[EscrowTimeoutScriptSignature] must be(true)
  }

}
