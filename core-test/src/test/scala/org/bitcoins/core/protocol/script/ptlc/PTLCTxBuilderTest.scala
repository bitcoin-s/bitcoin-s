package org.bitcoins.core.protocol.script.ptlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  ECPrivateKey,
  ECPublicKey,
  WitnessTxSigComponentP2SH,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  ConditionalScriptPubKey,
  EmptyScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2PKWithTimeoutScriptPubKey,
  P2SHScriptPubKey,
  P2SHScriptSignature,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  UnassignedWitnessScriptPubKey,
  WitnessCommitment,
  WitnessScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput,
  WitnessTransaction
}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfoFull,
  UTXOSpendingInfo
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future

class PTLCTxBuilderTest extends BitcoinSAsyncTest {
  behavior of "PTLCTxBuilder"

  val paymentAmt: CurrencyUnit = CurrencyUnits.oneMBTC

  val inputPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKey: ECPublicKey = inputPrivKey.publicKey

  val payerPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val payerPubKey: ECPublicKey = payerPrivKey.publicKey
  val receiverPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val receiverPubKey: ECPublicKey = receiverPrivKey.publicKey

  val blockTimeToday: UInt32 =
    UInt32(System.currentTimeMillis() / 1000)

  val inputTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(TransactionOutput(paymentAmt * 2, P2WPKHWitnessSPKV0(inputPubKey))),
    UInt32.zero
  )

  val inputUTXO: P2WPKHV0SpendingInfo = P2WPKHV0SpendingInfo(
    outPoint = TransactionOutPoint(inputTx.txId, UInt32.zero),
    amount = paymentAmt * 2,
    scriptPubKey = P2WPKHWitnessSPKV0(inputPubKey),
    signer = inputPrivKey,
    hashType = HashType.sigHashAll,
    scriptWitness = P2WPKHWitnessV0(inputPubKey)
  )

  val changeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val timeout: UInt32 = UInt32(blockTimeToday.toLong + 1)

  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis.one)

  val finalAddress: Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey), RegTest)

  val refundAddress: Bech32Address =
    Bech32Address(P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey), RegTest)

  def constructPTLCTxBuilders: Future[(PTLCTxBuilder, PTLCTxBuilder)] = {
    val payerPTLCTxBuilder: PTLCTxBuilder =
      PTLCTxBuilder(paymentAmt,
                    payerPubKey,
                    receiverPubKey,
                    Some(Vector(inputUTXO)),
                    None,
                    feeRate,
                    changeSPK,
                    RegTest)

    payerPTLCTxBuilder.unsignedFundingTransaction.map { unsignedFundingTx =>
      val receiverPTLCTxBuilder = PTLCTxBuilder(paymentAmt,
                                                payerPubKey,
                                                receiverPubKey,
                                                None,
                                                Some(unsignedFundingTx),
                                                feeRate,
                                                changeSPK,
                                                RegTest)

      (payerPTLCTxBuilder, receiverPTLCTxBuilder)
    }
  }

  def verifyScript(
      tx: Transaction,
      utxos: Vector[UTXOSpendingInfo]): Boolean = {
    val programs: Vector[PreExecutionScriptProgram] =
      tx.inputs.zipWithIndex.toVector.map {
        case (input: TransactionInput, idx: Int) =>
          val outpoint = input.previousOutput

          val creditingTx =
            utxos.find(u => u.outPoint.txId == outpoint.txId).get

          val output = creditingTx.output

          val spk = output.scriptPubKey

          val amount = output.value

          val txSigComponent = spk match {
            case witSPK: WitnessScriptPubKeyV0 =>
              val o = TransactionOutput(amount, witSPK)
              WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],
                                       UInt32(idx),
                                       o,
                                       Policy.standardFlags)
            case _: UnassignedWitnessScriptPubKey => ???
            case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
                _: WitnessCommitment | _: CSVScriptPubKey |
                _: CLTVScriptPubKey | _: ConditionalScriptPubKey |
                _: NonStandardScriptPubKey | EmptyScriptPubKey) =>
              val o = TransactionOutput(CurrencyUnits.zero, x)
              BaseTxSigComponent(tx, UInt32(idx), o, Policy.standardFlags)

            case _: P2SHScriptPubKey =>
              val p2shScriptSig =
                tx.inputs(idx).scriptSignature.asInstanceOf[P2SHScriptSignature]
              p2shScriptSig.redeemScript match {

                case _: WitnessScriptPubKey =>
                  WitnessTxSigComponentP2SH(
                    transaction = tx.asInstanceOf[WitnessTransaction],
                    inputIndex = UInt32(idx),
                    output = output,
                    flags = Policy.standardFlags)

                case _ =>
                  BaseTxSigComponent(tx,
                                     UInt32(idx),
                                     output,
                                     Policy.standardFlags)
              }
          }

          PreExecutionScriptProgram(txSigComponent)
      }
    ScriptInterpreter.runAllVerify(programs)
  }

  it should "execute everything correctly" in {
    for {
      (payerTxBuilder, receiverTxBuilder) <- constructPTLCTxBuilders
      payerSig <- payerTxBuilder.createAdaptorSig(finalAddress, payerPrivKey)
      refundSig <- receiverTxBuilder.createRefundSig(refundAddress,
                                                     receiverPrivKey,
                                                     timeout)
      fundingTx <- payerTxBuilder.signedFundingTransaction
      spendingTx <- receiverTxBuilder.createNormalSpendingTx(payerSig,
                                                             finalAddress,
                                                             receiverPrivKey)
      refundTx <- payerTxBuilder.createRefundTx(refundSig,
                                                refundAddress,
                                                payerPrivKey,
                                                timeout)
    } yield {
      assert(payerTxBuilder.fundingSPK == receiverTxBuilder.fundingSPK)
      assert(verifyScript(fundingTx, Vector(inputUTXO)))

      val spendingInfo = P2WSHV0SpendingInfoFull(
        TransactionOutPoint(fundingTx.txIdBE, UInt32.zero),
        fundingTx.outputs.head.value,
        P2WSHWitnessSPKV0(payerTxBuilder.fundingSPK),
        Vector(payerPrivKey, receiverPrivKey),
        HashType.sigHashAll,
        P2WSHWitnessV0(payerTxBuilder.fundingSPK),
        ConditionalPath.NoConditionsLeft
      )

      assert(verifyScript(spendingTx, Vector(spendingInfo)))
      assert(verifyScript(refundTx, Vector(spendingInfo)))
    }
  }
}
