package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, EmptyDigitalSignature, TransactionSignatureCreator, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.result.ScriptError
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.WTxSigComponentHelper
import org.bitcoins.core.wallet.signer.{MultiSigSigner, P2PKHSigner, P2PKSigner, P2WPKHSigner}

import scala.annotation.tailrec
import scala.util.Try

/** High level class to create a signed transaction that spends a set of
  * unspent transaction outputs.
  */
sealed abstract class TxBuilder {
  private val logger = BitcoinSLogger.logger
  type OutputInfo = (TransactionOutPoint, TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])
  type OutPointMap = Map[TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]
  def destinations: Seq[TransactionOutput]

  def destinationSPKs = destinations.map(_.scriptPubKey)

  def destinationAmounts = destinations.map(_.value)

  def totalDestinationAmount = destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  def creditingTxs: Seq[Transaction]

  /** The unspent transaction outputs we are spending in the new transaction we are building */
  private val creditingUtxos: OutPointMap  = {
    outPointsSpendingInfo.map { case (o,(keys, redeemScriptOpt, scriptWitOpt)) =>
      //this must exist because of our invariant in TransactionBuilderImpl()
      val tx = creditingTxs.find(tx => tx.txId == o.txId).get
      (o,(tx.outputs(o.vout.toInt), keys, redeemScriptOpt, scriptWitOpt))
    }
  }

  /** The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]s we are attempting to spend
    * and the keys that are needed to sign the utxo we are spending.
    */
  def outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]

  def privKeys: Seq[ECPrivateKey] = outPointsSpendingInfo.values.flatMap(_._1).toSeq

  def outPoints: Seq[TransactionOutPoint] = outPointsSpendingInfo.keys.toSeq

  def redeemScriptOpt: Seq[Option[ScriptPubKey]] = outPointsSpendingInfo.values.map(_._2).toSeq

  /** Signs the given transaction and then returns a signed tx
    * Checks the given invariants when the signing process is done
    * An example of some invariants is that the fee on the signed transaction below a certain amount,
    * or that RBF is enabled on the signed transaction.
    * */
  def sign(invariants: Transaction => Boolean): Either[Transaction, TxBuilderError] = {
    @tailrec
    def loop(remaining: List[OutputInfo],
             txInProgress: Transaction): Either[Transaction,TxBuilderError] = remaining match {
      case Nil => Left(txInProgress)
      case info :: t =>
        val partiallySigned = sign(info, txInProgress, HashType.sigHashAll)
        partiallySigned match {
          case Left(tx) => loop(t,tx)
          case Right(err) => Right(err)
        }
    }
    val utxos: List[OutputInfo] = creditingUtxos.map { c : (TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])) =>
      (c._1, c._2._1, c._2._2, c._2._3, c._2._4)
    }.toList
    val outpoints = utxos.map(_._1)
    val scriptWitOpt = utxos.map(_._5)
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt)
    val tc = TransactionConstants
    val inputs = outpoints.map(o => TransactionInput(o,EmptyScriptSignature,tc.sequence))
    val unsigned = unsignedTxWit match {
      case EmptyWitness => Transaction(tc.version,inputs,destinations,tc.lockTime)
      case wit: TransactionWitness => WitnessTransaction(tc.version,inputs,destinations,tc.lockTime,wit)
    }
    val signedTx = loop(utxos, unsigned)
    signedTx match {
      case l: Left[Transaction,TxBuilderError] =>
        if (!invariants(l.a)) {
          Right(TxBuilderError.FailedUserInvariants)
        } else {
          l
        }
      case r: Right[Transaction, TxBuilderError] => r
    }
  }

  /** Creates a newly signed input and adds it to the given tx */
  private def sign(info: OutputInfo, unsignedTx: Transaction, hashType: HashType,
                   sequence: UInt32 = TransactionConstants.sequence): Either[Transaction, TxBuilderError] = {
    val outpoint = info._1
    val output = info._2
    val keys = info._3
    val redeemScriptOpt = info._4
    val scriptWitnessOpt = info._5
    val inputIndex = UInt32(unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint).get._2)
    output.scriptPubKey match {
      case _: P2PKScriptPubKey =>
        P2PKSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case _: P2PKHScriptPubKey => P2PKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case _: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case Some(redeemScript) =>
            val input = TransactionInput(outpoint,EmptyScriptSignature,sequence)
            val updatedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,unsignedTx.inputs.updated(inputIndex.toInt,input),btx.outputs,btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,unsignedTx.inputs.updated(inputIndex.toInt,input),wtx.outputs,wtx.lockTime,wtx.witness)
            }
            val updatedOutput = TransactionOutput(output.value,redeemScript)
            val signedTxEither: Either[Transaction, TxBuilderError] = sign((outpoint,updatedOutput,keys,None, scriptWitnessOpt),updatedTx,hashType,sequence)
            signedTxEither.left.map { signedTx =>
              val i = signedTx.inputs(inputIndex.toInt)
              val p2sh = P2SHScriptSignature(i.scriptSignature,redeemScript)
              val signedInput = TransactionInput(i.previousOutput,p2sh,i.sequence)
              val signedInputs = signedTx.inputs.updated(inputIndex.toInt,signedInput)
              signedTx match {
                case btx: BaseTransaction =>
                  BaseTransaction(btx.version,signedInputs,btx.outputs,btx.lockTime)
                case wtx: WitnessTransaction =>
                  WitnessTransaction(wtx.version,signedInputs,wtx.outputs,wtx.lockTime,wtx.witness)
              }

            }
          case None => Right(TxBuilderError.NoRedeemScript)
        }
      case _: NonStandardScriptPubKey => Right(TxBuilderError.NonStandardSPK)
      case _: WitnessScriptPubKeyV0 =>
        scriptWitnessOpt match {
          case Some(_) =>
            //TODO: this only accounts for a p2wpkh spk, NOT P2WSH
            if (keys.size != 1) {
              Right(TxBuilderError.TooManyKeys)
            } else {
              val signed = P2WPKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType)
              signed.left.map(_.transaction)
            }
          case None => Right(TxBuilderError.NoWitness)
        }


    }
  }
}


object TxBuilder {
  private case class TransactionBuilderImpl(destinations: Seq[TransactionOutput],
                                            creditingTxs: Seq[Transaction],
                                            outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]) extends TxBuilder {
    require(outPoints.exists(o => creditingTxs.exists(_.txId == o.txId)))
  }

  def apply(destinations: Seq[TransactionOutput], creditingTxs: Seq[Transaction],
            outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]): Try[TxBuilder] = {
    Try(TransactionBuilderImpl(destinations,creditingTxs,outPointsSpendingInfo))
  }
}
