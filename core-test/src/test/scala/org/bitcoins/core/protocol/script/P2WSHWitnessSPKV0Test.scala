package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant.{OP_1, ScriptConstant, ScriptNumber}
import org.bitcoins.core.script.crypto.{OP_CHECKMULTISIG, OP_CHECKSIG}
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.crypto.ECPrivateKeyBytes
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class P2WSHWitnessSPKV0Test extends BitcoinSUnitTest {
  val uncompressed = ECPrivateKeyBytes.freshPrivateKey(false).publicKeyBytes
  val pushOps = BitcoinScriptUtil.calculatePushOp(uncompressed.bytes)
  val asmP2PK = pushOps ++ Seq(ScriptConstant(uncompressed.bytes), OP_CHECKSIG)
  val p2pk = P2PKScriptPubKey(asmP2PK)

  val asmMultisig =
    Seq(OP_1) ++ pushOps ++ Seq(ScriptConstant(uncompressed.bytes),
                                OP_1,
                                OP_CHECKMULTISIG)
  val multisig = MultiSignatureScriptPubKey.fromAsm(asmMultisig)

  "P2WPKHWitnessSPKV0" must "fail to be created with an uncompressed public key" in {
    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(p2pk)
    }
  }

  it must "fail for a multisig script with an uncompressed public key" in {

    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(multisig)
    }
  }

  it must "fail with a locktime script" in {
    val cltv = CLTVScriptPubKey(ScriptNumber.zero, multisig)
    intercept[IllegalArgumentException] {
      P2WSHWitnessSPKV0(cltv)
    }
  }
}
