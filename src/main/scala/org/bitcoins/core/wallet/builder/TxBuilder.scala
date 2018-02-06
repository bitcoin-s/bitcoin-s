package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.P2PKHHelper

import scala.annotation.tailrec
import scala.util.{Success, Try}

/** High level class to create a signed transaction that spends a set of
  * unspent transaction outputs.
  */
sealed abstract class TxBuilder {
  type OutputInfo = (TransactionOutPoint, TransactionOutput, Seq[ECPrivateKey])

  def destinations: Seq[TransactionOutput]

  def destinationSPKs = destinations.map(_.scriptPubKey)

  def destinationAmounts = destinations.map(_.value)

  def totalDestinationAmount = destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  def creditingTxs: Seq[Transaction]

  /** The unspent transaction outputs we are spending in the new transaction we are building */
  private val creditingUtxos: Map[TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey])] = {
    outPointsWithKeys.map { case (o,keys) =>
      //this must exist because of our invariant in TransactionBuilderImpl()
      val tx = creditingTxs.find(tx => tx.txId == o.txId).get
      (o,(tx.outputs(o.vout.toInt),keys))
    }
  }

  /** The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]s we are attempting to spend
    * and the keys that are needed to sign the utxo we are spending.
    */
  def outPointsWithKeys: Map[TransactionOutPoint, Seq[ECPrivateKey]]

  def privKeys: Seq[ECPrivateKey] = outPointsWithKeys.values.flatten.toSeq

  def outPoints: Seq[TransactionOutPoint] = outPointsWithKeys.keys.toSeq

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
        val partiallySigned = sign(info,txInProgress, HashType.sigHashAll)
        partiallySigned match {
          case Left(tx) => loop(t,tx)
          case Right(err) => Right(err)
        }
    }
    val utxos: List[OutputInfo] = creditingUtxos.map { c : (TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey])) =>
      (c._1, c._2._1, c._2._2)
    }.toList
    val signedTx = loop(utxos, EmptyTransaction)
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
  private def sign(info: OutputInfo, tx: Transaction, hashType: HashType, sequence: UInt32 = TransactionConstants.sequence): Either[Transaction, TxBuilderError] = {
    val outpoint = info._1
    val output = info._2
    val keys = info._3
    output.scriptPubKey match {
      case p2pkh: ScriptPubKey =>
        if (keys.size != 1) Right(TxBuilderError.TooManyKeys)
        else {
          val txComponent = P2PKHHelper.sign(keys.head,p2pkh,outpoint,tx.inputs,tx.outputs,hashType,tx.version,sequence,tx.lockTime)
          Left(txComponent.transaction)
        }
    }
  }
}


object TxBuilder {
  private case class TransactionBuilderImpl(destinations: Seq[TransactionOutput],
                                            creditingTxs: Seq[Transaction],
                                            outPointsWithKeys: Map[TransactionOutPoint, Seq[ECPrivateKey]]) extends TxBuilder {
    require(outPoints.exists(o => creditingTxs.exists(_.txId == o.txId)))
  }

  def apply(destinations: Seq[TransactionOutput], creditingTxs: Seq[Transaction], outPointsWithKeys: Map[TransactionOutPoint, Seq[ECPrivateKey]]): Try[TxBuilder] = {
    Try(TransactionBuilderImpl(destinations,creditingTxs,outPointsWithKeys))
  }


}
