package org.bitcoins.core.crypto

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DummyECDigitalSignature,
  ECPublicKey
}
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TxSigComponentTest extends BitcoinSUnitTest {
  behavior of "TxSigComponent"

  it should "correctly construct P2SHTxSigComponent" in {
    val p2shNoNest = P2SHScriptPubKey(EmptyScriptPubKey)
    val pubKey = ECPublicKey.freshPublicKey
    val btx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(
          TransactionOutPoint(DoubleSha256Digest.empty, UInt32.zero),
          P2SHScriptSignature(scriptSig =
                                ConditionalScriptSignature(EmptyScriptSignature,
                                                           condition = true),
                              redeemScript = P2WPKHWitnessSPKV0(pubKey)),
          TransactionConstants.sequence
        )),
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey)),
      UInt32.zero
    )

    val nonWitnessP2SH =
      P2SHTxSigComponent(btx,
                         UInt32.zero,
                         TransactionOutput(Satoshis.one, p2shNoNest),
                         Policy.standardFlags)
    assert(!nonWitnessP2SH.isInstanceOf[WitnessTxSigComponentP2SH])
  }

  it should "correctly construct WitnessTxSigComponentP2SH" in {
    val pubKey = ECPublicKey.freshPublicKey
    val wspk = P2WPKHWitnessSPKV0(pubKey)
    val p2shNested = P2SHScriptPubKey(wspk)

    val wtx = WitnessTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(
          TransactionOutPoint(DoubleSha256Digest.empty, UInt32.zero),
          P2SHScriptSignature(wspk),
          TransactionConstants.sequence
        )),
      Vector(TransactionOutput(Satoshis.one, EmptyScriptPubKey)),
      UInt32.zero,
      TransactionWitness(
        Vector(P2WPKHWitnessV0(pubKey, DummyECDigitalSignature)))
    )

    val witnessP2SH =
      P2SHTxSigComponent(wtx,
                         UInt32.zero,
                         TransactionOutput(Satoshis.one, p2shNested),
                         Policy.standardFlags)

    assert(witnessP2SH.isInstanceOf[WitnessTxSigComponentP2SH])
  }
}
