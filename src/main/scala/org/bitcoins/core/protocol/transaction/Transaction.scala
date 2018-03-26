package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.transaction.{ RawBaseTransactionParser, RawWitnessTransactionParser }
import org.bitcoins.core.util.{ BitcoinSUtil, CryptoUtil, Factory }

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 7/14/15.
 */
sealed abstract class Transaction extends NetworkElement {
  /**
   * The sha256(sha256(tx)) of this transaction
   * Note that this is the big endian encoding of the hash NOT the little endian encoding displayed on block explorers
   */
  def txId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /** The version number for this transaction */
  def version: UInt32

  /** The inputs for this transaction */
  def inputs: Seq[TransactionInput]

  /** The outputs for this transaction */
  def outputs: Seq[TransactionOutput]

  /** The locktime for this transaction */
  def lockTime: UInt32

  /**
   * This is used to indicate how 'expensive' the transction is on the blockchain.
   * This use to be a simple calculation before segwit (BIP141). Each byte in the transaction
   * counted as 4 'weight' units. Now with segwit, the [[TransactionWitness]] is counted as 1 weight unit per byte,
   * while other parts of the transaction (outputs, inputs, locktime etc) count as 4 weight units.
   * As we add more witness versions, this may be subject to change
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
   * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
   */
  def weight: Long

  /**
   * The transaction's virtual size
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
   */
  def vsize: Long = Math.ceil(weight / 4.0).toLong

  /**
   * Base transaction size is the size of the transaction serialised with the witness data stripped
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
   */
  def baseSize: Long = this match {
    case btx: BaseTransaction    => btx.size
    case wtx: WitnessTransaction => BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime).baseSize
  }

  def totalSize: Long = bytes.size

  /** Determines if this transaction is a coinbase transaction. */
  def isCoinbase: Boolean = inputs.size match {
    case 1 => inputs.head match {
      case coinbase: CoinbaseInput => true
      case _: TransactionInput     => false
    }
    case _: Int => false
  }
}

sealed abstract class BaseTransaction extends Transaction {
  override def bytes = RawBaseTransactionParser.write(this)
  override def weight = size * 4
}

case object EmptyTransaction extends BaseTransaction {
  override def txId = CryptoUtil.emptyDoubleSha256Hash
  override def version = TransactionConstants.version
  override def inputs = Nil
  override def outputs = Nil
  override def lockTime = TransactionConstants.lockTime
}

sealed abstract class WitnessTransaction extends Transaction {
  /** The txId for the witness transaction from satoshi's original serialization */
  override def txId: DoubleSha256Digest = {
    val btx = BaseTransaction(version, inputs, outputs, lockTime)
    btx.txId
  }

  /**
   * The witness used to evaluate [[org.bitcoins.core.protocol.script.ScriptSignature]]/[[org.bitcoins.core.protocol.script.ScriptPubKey]]s inside of a segwit tx
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki]]
   */
  def witness: TransactionWitness

  /**
   * The witness transaction id as defined by BIP141
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id]]
   */
  def wTxId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /**
   * Weight calculation in bitcoin for witness txs
   * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
   * @return
   */
  override def weight: Long = {
    val base = BaseTransaction(version, inputs, outputs, lockTime)
    base.size * 3 + size
  }
  override def bytes = RawWitnessTransactionParser.write(this)

}

object Transaction extends Factory[Transaction] {

  def fromBytes(bytes: Seq[Byte]): Transaction = {
    val wtxTry = Try(RawWitnessTransactionParser.read(bytes))
    wtxTry match {
      case Success(wtx) =>
        wtx
      case Failure(f) =>
        val btx = RawBaseTransactionParser.read(bytes)
        btx
    }
  }
}

object BaseTransaction extends Factory[BaseTransaction] {
  private case class BaseTransactionImpl(version: UInt32, inputs: Seq[TransactionInput],
                                         outputs: Seq[TransactionOutput], lockTime: UInt32) extends BaseTransaction

  override def fromBytes(bytes: Seq[Byte]): BaseTransaction = RawBaseTransactionParser.read(bytes)

  def apply(version: UInt32, inputs: Seq[TransactionInput],
            outputs: Seq[TransactionOutput], lockTime: UInt32): BaseTransaction = BaseTransactionImpl(version, inputs, outputs, lockTime)
}

object WitnessTransaction extends Factory[WitnessTransaction] {
  private case class WitnessTransactionImpl(version: UInt32, inputs: Seq[TransactionInput],
                                            outputs: Seq[TransactionOutput], lockTime: UInt32,
                                            witness: TransactionWitness) extends WitnessTransaction

  def apply(version: UInt32, inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput],
            lockTime: UInt32, witness: TransactionWitness): WitnessTransaction =
    WitnessTransactionImpl(version, inputs, outputs, lockTime, witness)

  override def fromBytes(bytes: Seq[Byte]): WitnessTransaction = RawWitnessTransactionParser.read(bytes)

}