package org.bitcoins.core.wallet.signer

import org.bitcoins.core.protocol.script.WitnessScriptPubKey
import org.bitcoins.core.wallet.utxo.{
  P2SHSpendingInfo,
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfo,
  UnassignedSegwitNativeUTXOSpendingInfo
}
import org.bitcoins.testkit.core.gen.{CreditingTxGen, TransactionGenerators}
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}

import scala.concurrent.ExecutionContext

class SignerTest extends BitcoinSAsyncTest {

  implicit val ec: ExecutionContext = ExecutionContext.global

  behavior of "Signer"

  it should "fail to sign a UnassignedSegwit UTXO" in {
    val p2wpkh = CreditingTxGen.p2wpkhOutput.sample.get
    val tx = TransactionGenerators.baseTransaction.sample.get
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
      BitcoinSigner.sign(spendingInfo, tx, isDummySignature = false))
  }

  it should "fail to sign a P2SH UTXO" in {
    val p2sh = CreditingTxGen.p2shOutput.sample.get
    val tx = TransactionGenerators.baseTransaction.sample.get
    assertThrows[IllegalArgumentException](
      BitcoinSigner.sign(p2sh, tx, isDummySignature = false))
  }

  it should "fail if there are inconsistent P2WPKH spending infos" in {
    val dumbSpendingInfo = CreditingTxGen.output.sample.get
    val p2wpkh =
      CreditingTxGen.p2wpkhOutput.sample.get.asInstanceOf[P2WPKHV0SpendingInfo]
    val tx = TransactionGenerators.baseTransaction.sample.get
    recoverToSucceededIf[IllegalArgumentException] {
      P2WPKHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wpkh)
    }
  }

  it should "fail if there are inconsistent P2WSH spending infos" in {
    val dumbSpendingInfo = CreditingTxGen.output.sample.get
    val p2wsh =
      CreditingTxGen.p2wshOutput.sample.get.asInstanceOf[P2WSHV0SpendingInfo]
    val tx = TransactionGenerators.baseTransaction.sample.get
    recoverToSucceededIf[IllegalArgumentException] {
      P2WSHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wsh)
    }
  }
}
