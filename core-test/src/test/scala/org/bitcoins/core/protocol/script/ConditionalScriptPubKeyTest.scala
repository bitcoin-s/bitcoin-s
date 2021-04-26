package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class ConditionalScriptPubKeyTest extends BitcoinSJvmTest {

  behavior of "ConditionalScriptPubKey"

  it must "be able to parse a conditional spk with a nested p2sh script" in {
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/2962
    val scriptSigHex =
      "48304502207ffb30631d837895ac1b415408b70a809090b25d4070198043299fe247f62d2602210089b52f0d243dea1e4313ef67723b0d0642ff77bb6ca05ea85296b2539c797f5c81513d632103184b16f5d6c01e2d6ded3f8a292e5b81608318ecb8e93aa3747bc88b8dbf256cac67a914f5862841f254a1483eab66909ae588e45d617c5e8768"

    val scriptSig = ScriptSignature.fromAsmHex(scriptSigHex)

    scriptSig match {
      case p2sh: P2SHScriptSignature =>
        assert(p2sh.redeemScript.isInstanceOf[IfConditionalScriptPubKey])
      case x => fail(s"Did not parse a p2sh script sig, got=$x")
    }
  }

  it must "consider a redeem script that contains OP_IF in it with a nested p2sh script pubkey in it" in {
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/2962
    val hex =
      "632103184b16f5d6c01e2d6ded3f8a292e5b81608318ecb8e93aa3747bc88b8dbf256cac67a914f5862841f254a1483eab66909ae588e45d617c5e8768"
    val scriptPubKey = RawScriptPubKey.fromAsmHex(hex)

    scriptPubKey match {
      case conditional: IfConditionalScriptPubKey =>
        assert(
          conditional.secondSPK.isInstanceOf[NonStandardScriptPubKey],
          s"Although we have same op codes as p2sh spk, " +
            s"this combination isn't used in a standalone output, rather a redeemScript"
        )
      case x => fail(s"Incorrect type for the spk, got=$x")
    }
    val constant = ScriptConstant.fromHex(hex)

    assert(P2SHScriptSignature.isRedeemScript(constant))

    //we should be able to parse the tx that contains it with no problem
    val txHex =
      "010000000193983cb2f1f3d83615f10e1ca33f49c72250acbebd840793aaa8bc7a6b28b76b000000008848304502207ffb30631d837895ac1b415408b70a809090b25d4070198043299fe247f62d2602210089b52f0d243dea1e4313ef67723b0d0642ff77bb6ca05ea85296b2539c797f5c81513d632103184b16f5d6c01e2d6ded3f8a292e5b81608318ecb8e93aa3747bc88b8dbf256cac67a914f5862841f254a1483eab66909ae588e45d617c5e8768ffffffff01a0860100000000001976a914a07aa8415d34d0db44f220ff9522609641c0bfbf88ac00000000"
    assert(Transaction.fromHexT(txHex).isSuccess)
  }
}
