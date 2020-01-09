package org.bitcoins.core.wallet.signer

import org.bitcoins.core.protocol.script.WitnessScriptPubKey
import org.bitcoins.core.wallet.utxo.{
  P2WPKHV0UTXOSpendingInfo,
  P2WSHV0UTXOSpendingInfo,
  UnassignedSegwitNativeUTXOSpendingInfo
}
import org.bitcoins.testkit.core.gen.{
  CreditingTxGen,
  GenUtil,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.ExecutionContext

class SignerTest extends BitcoinSAsyncTest {

  implicit val ec: ExecutionContext = ExecutionContext.global

  behavior of "Signer"

  it should "fail to sign a UnassignedSegwit UTXO" in {
    val p2wpkh = GenUtil.sample(CreditingTxGen.p2wpkhOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    val spendingInfo = UnassignedSegwitNativeUTXOSpendingInfo(
      p2wpkh.outPoint,
      p2wpkh.amount,
      p2wpkh.scriptPubKey.asInstanceOf[WitnessScriptPubKey],
      p2wpkh.signers,
      p2wpkh.hashType,
      p2wpkh.scriptWitnessOpt.get,
      p2wpkh.conditionalPath
    )
    assertThrows[UnsupportedOperationException](
      BitcoinUTXOSigner.sign(spendingInfo, tx, isDummySignature = false))
  }

  it should "fail to sign a P2SH UTXO" in {
    val p2sh = GenUtil.sample(CreditingTxGen.p2shOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    assertThrows[IllegalArgumentException](
      BitcoinUTXOSigner.sign(p2sh, tx, isDummySignature = false))
  }

  it should "fail if there are inconsistent P2WPKH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wpkh = GenUtil
      .sample(CreditingTxGen.p2wpkhOutput)
      .asInstanceOf[P2WPKHV0UTXOSpendingInfo]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    recoverToSucceededIf[IllegalArgumentException] {
      P2WPKHUTXOSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wpkh)
    }
  }

  it should "fail if there are inconsistent P2WSH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wsh = GenUtil
      .sample(CreditingTxGen.p2wshOutput)
      .asInstanceOf[P2WSHV0UTXOSpendingInfo]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    recoverToSucceededIf[IllegalArgumentException] {
      P2WSHUTXOSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wsh)
    }
  }
}
