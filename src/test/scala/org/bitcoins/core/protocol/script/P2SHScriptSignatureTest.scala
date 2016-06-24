package org.bitcoins.core.protocol.script


import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, OP_0, ScriptConstant}
import org.bitcoins.core.util.{BitcoinSLogger, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/8/16.
 */
class P2SHScriptSignatureTest extends FlatSpec with MustMatchers with BitcoinSLogger {

  "P2SHScriptSignature" must "find the public keys embedded inside of the redeemScript" in {
    val rawP2SHScriptSig = TestUtil.rawP2shInputScript2Of2
    val p2shScriptSig : P2SHScriptSignature = ScriptSignature(rawP2SHScriptSig) match {
      case x : P2SHScriptSignature => x
      case y => throw new RuntimeException("Must be p2sh script sig: " + y)
    }
    p2shScriptSig.publicKeys must be (Seq(
      ECPublicKey("0369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b83"),
      ECPublicKey("02480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f721")
    ))

  }

  it must "return a p2sh scriptSig with no serialized redeemScript" in {

    val p2shScriptSig = TestUtil.p2shInputScript2Of2 match {
      case s : P2SHScriptSignature => s
      case _ => throw new RuntimeException("Should be p2sh scriptSig")
    }

    p2shScriptSig.scriptSignatureNoRedeemScript.asm must be (Seq(
      OP_0, BytesToPushOntoStack(71), ScriptConstant("304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101"),
      BytesToPushOntoStack(72),
      ScriptConstant("3045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c801")
    ))
  }


  it must "create a p2sh script signature out of a large multisignature script signature & multiSignature scriptPubKey" in {
    val multiSigScriptSig = MultiSignatureScriptSignature("00473045022100867aa98fe0210eef1dff246de2ada03f4846ac9ec006ab24c1d868e6f08fb4b30220131acbe68c3302ed7bf82949810d6daf780703021cdcf481f5142f76fe31af04")
    logger.info("==============================================================")
    val multiSigScriptPubKey = MultiSignatureScriptPubKey("542103a1d052fcc371fc80d0d268169620ee125167335d9895f1639897c774deb80e1221038d83a6ed9f25ada86473f00200e31e11e7f5756c2a152e58f12ba078536d86a921031078d342d0ad318d865b83aa8eab7b747e92382e1f361f897f280560c984b426210255715a21757122db811b3b806ab2a3f5c4b04e229907c5a9497d30a7e6803441210244fe1d3ec95e0c3c9aa796686fffd9ba98ae778b80fc1038f171ced12d9b5f662102fcaf377e285e759bc0f0afc0d43b9f1f76cb8cd1279542e739261b9f577f5954210335e6efec824fdd539132a63696dd1dc719fbfc5efffa3d2576601c98eae6619821028af3bf77385560fc68b2932b66cae607052b0c53ae74c015fecf2d7ee9837e5f2102e1404987c0ee03803487923998f862700dcf9bdf4518d744a94fa208ff1f33d02103eaea267ef814f1ba57f43fe8f8dd8a1e8ab99d9d0e34e9d0a144e2a5e4c70efc21035e7bdaeba5d370bf5565937d3734c6e5bc0746d9d9bbf6846c7f3219a406a0da21026eb05e10608a3543b182a69d1dc651bdbab69e684be98260041a89aed1c4e1e62102e407986723ccdf2858c0d2116ed450d7b9ee6609d96603018c31470b065569eb2103f4ca93db51e4c602eda84fbb6dd285b66b12e53f1606a992bbc5bde0323ee64621022c83ca35993a8e18d76c386bb1d8e21c2575b2f675528e162b4cf81e7502745e5fae")
    val p2shScriptSig = P2SHScriptSignature(multiSigScriptSig, multiSigScriptPubKey)

  }
}
