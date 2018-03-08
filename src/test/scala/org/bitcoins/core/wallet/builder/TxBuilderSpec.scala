package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{BaseTxSigComponent, WitnessTxSigComponentRaw}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.{CreditingTxGen, ScriptGenerators, TransactionGenerators}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptOk, ScriptResult}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.signer.Signer
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants
  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c._1.outputs(c._2))
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAll(TransactionGenerators.smallOutputs(totalAmount), ScriptGenerators.scriptPubKey) {
          case (destinations: Seq[TransactionOutput], changeSPK) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys = buildCreditingTxInfo(creditingTxsInfo)
            val builder = TxBuilder(destinations, creditingTxsInfo.map(_._1), outpointsWithKeys, fee, changeSPK._1)
            val result = builder.left.flatMap(_.sign(false))
            result match {
              case Left(tx) =>
                val noRedeem = creditingTxsInfo.map(c => (c._1, c._2))
                verifyScript(tx, noRedeem)
              case Right(err) =>
                //incompatible locktime case can happen when we have > 1 CLTVSPK that we are trying to spend
                logger.warn("err: " + err)
                err == TxBuilderError.IncompatibleLockTimes
            }
        }
    }
  }

  private def buildCreditingTxInfo(info: Seq[TxBuilderSpec.CreditingTxInfo]): TxBuilderSpec.OutPointMap = {
    @tailrec
    def loop(rem: Seq[TxBuilderSpec.CreditingTxInfo],
             accum: TxBuilderSpec.OutPointMap): TxBuilderSpec.OutPointMap = rem match {
      case Nil => accum
      case (tx,idx,signers,redeemScriptOpt,scriptWitOpt, hashType) :: t =>
        val o = TransactionOutPoint(tx.txId,UInt32(idx))
        val output = tx.outputs(idx)
        val outPointsSpendingInfo = (output, signers, redeemScriptOpt, scriptWitOpt, hashType)
        loop(t,accum.updated(o,outPointsSpendingInfo))
    }
    loop(info,Map.empty)
  }

  def verifyScript(tx: Transaction, creditingTxsInfo: Seq[(Transaction, Int)]): Boolean = {
    val results: Seq[ScriptResult] = tx.inputs.zipWithIndex.map { case (input: TransactionInput,idx: Int) =>
      val outpoint = input.previousOutput
      val creditingTx = creditingTxsInfo.find(_._1.txId == outpoint.txId).get
      val output = creditingTx._1.outputs(creditingTx._2)
      val spk =  output.scriptPubKey
      val amount = output.value
      val txSigComponent = spk match {
        case witSPK: WitnessScriptPubKeyV0 => WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],UInt32(idx),
          witSPK, Policy.standardFlags, amount)
        case _: UnassignedWitnessScriptPubKey => ???
        case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: WitnessCommitment
          | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | _: EscrowTimeoutScriptPubKey
          | EmptyScriptPubKey) =>
          BaseTxSigComponent(tx,UInt32(idx),x,Policy.standardFlags)
        case p2sh: P2SHScriptPubKey =>
          //TODO: This is probably going to need to be changed with P2WSH
          BaseTxSigComponent(tx,UInt32(idx),p2sh,Policy.standardFlags)
      }
      val program = ScriptProgram(txSigComponent)
      val result = ScriptInterpreter.run(program)
      result
    }
    !results.exists(_ != ScriptOk)
  }
}

object TxBuilderSpec {
  type CreditingTxInfo = (Transaction, Int, Seq[Signer.Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)
  type OutPointMap = Map[TransactionOutPoint, (TransactionOutput, Seq[Signer.Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)]

}