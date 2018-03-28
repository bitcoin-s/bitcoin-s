package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ BaseTxSigComponent, WitnessTxSigComponentRaw }
import org.bitcoins.core.currency.{ CurrencyUnits, Satoshis }
import org.bitcoins.core.gen.{ ChainParamsGenerator, CreditingTxGen, ScriptGenerators, TransactionGenerators }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.{ PreExecutionScriptProgram, ScriptProgram }
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ ScriptOk, ScriptResult }
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

import org.bitcoins.core.wallet.signer.Signer
import org.bitcoins.core.wallet.utxo.{ BitcoinUTXOSpendingInfo, UTXOSpendingInfo }
import org.scalacheck.{ Prop, Properties }

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.Try
class BitcoinTxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants
  val timeout = 10.seconds
  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c._1.outputs(c._2))
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAllNoShrink(TransactionGenerators.smallOutputs(totalAmount), ScriptGenerators.scriptPubKey, ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
            val builder = BitcoinTxBuilder(destinations, outpointsWithKeys, fee, changeSPK._1, network)
            val tx = Await.result(builder.flatMap(_.sign), timeout)
            val noRedeem: Seq[(Transaction, Int)] = creditingTxsInfo.map(c => (c._1, c._2))
            verifyScript(tx, noRedeem)
        }
    }
  }

  property("sign a mix of p2sh/p2wsh in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.nestedOutputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c._1.outputs(c._2))
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAll(TransactionGenerators.smallOutputs(totalAmount), ScriptGenerators.scriptPubKey, ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
            val builder = BitcoinTxBuilder(destinations, outpointsWithKeys, fee, changeSPK._1, network)
            val tx = Await.result(builder.flatMap(_.sign), timeout)
            val noRedeem = creditingTxsInfo.map(c => (c._1, c._2))
            verifyScript(tx, noRedeem)
        }
    }
  }

  property("random fuzz test for tx builder") = {
    Prop.forAllNoShrink(CreditingTxGen.randoms) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c._1.outputs(c._2))
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAllNoShrink(TransactionGenerators.smallOutputs(totalAmount), ScriptGenerators.scriptPubKey, ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
            val builder = BitcoinTxBuilder(destinations, outpointsWithKeys, fee, changeSPK._1, network)
            val result = Try(Await.result(builder.flatMap(_.sign), timeout))
            val noRedeem = creditingTxsInfo.map(c => (c._1, c._2))
            if (result.isFailure) true else !verifyScript(result.get, noRedeem)
        }
    }
  }

  private def buildCreditingTxInfo(info: Seq[CreditingTxGen.CreditingTxInfo]): BitcoinTxBuilder.UTXOMap = {
    @tailrec
    def loop(
      rem: Seq[CreditingTxGen.CreditingTxInfo],
      accum: BitcoinTxBuilder.UTXOMap): BitcoinTxBuilder.UTXOMap = rem match {
      case Nil => accum
      case (tx, idx, signers, redeemScriptOpt, scriptWitOpt, hashType) :: t =>
        val o = TransactionOutPoint(tx.txId, UInt32(idx))
        val output = tx.outputs(idx)
        val outPointsSpendingInfo = BitcoinUTXOSpendingInfo(o, output, signers, redeemScriptOpt, scriptWitOpt, hashType)
        loop(t, accum.updated(o, outPointsSpendingInfo))
    }
    loop(info, Map.empty)
  }

  def verifyScript(tx: Transaction, creditingTxsInfo: Seq[(Transaction, Int)]): Boolean = {
    val programs: Seq[PreExecutionScriptProgram] = tx.inputs.zipWithIndex.map {
      case (input: TransactionInput, idx: Int) =>
        val outpoint = input.previousOutput
        val creditingTx = creditingTxsInfo.find(_._1.txId == outpoint.txId).get
        val output = creditingTx._1.outputs(creditingTx._2)
        val spk = output.scriptPubKey
        val amount = output.value
        val txSigComponent = spk match {
          case witSPK: WitnessScriptPubKeyV0 => WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction], UInt32(idx),
            witSPK, Policy.standardFlags, amount)
          case _: UnassignedWitnessScriptPubKey => ???
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: WitnessCommitment
            | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | _: EscrowTimeoutScriptPubKey
            | EmptyScriptPubKey) =>
            BaseTxSigComponent(tx, UInt32(idx), x, Policy.standardFlags)
          case p2sh: P2SHScriptPubKey =>
            BaseTxSigComponent(tx, UInt32(idx), p2sh, Policy.standardFlags)
        }
        ScriptProgram(txSigComponent)
    }
    ScriptInterpreter.runAllVerify(programs)
  }
}