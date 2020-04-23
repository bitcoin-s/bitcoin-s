package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.{DoubleSha256DigestBE, Sign}
import org.bitcoins.core.hd.{
  HDPath,
  LegacyHDPath,
  NestedSegWitHDPath,
  SegWitHDPath
}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  ConditionalPath,
  TxoState
}
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.keymanager.bip39.BIP39KeyManager

/**
  * DB representation of a native V0
  * SegWit UTXO
  */
case class SegwitV0SpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: SegWitHDPath,
    scriptWitness: ScriptWitness,
    txid: DoubleSha256DigestBE,
    state: TxoState,
    id: Option[Long] = None,
    blockHash: Option[DoubleSha256DigestBE]
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath
  override type SpendingInfoType = SegwitV0SpendingInfo

  override def copyWithState(state: TxoState): SegwitV0SpendingInfo =
    copy(state = state)

  override def copyWithId(id: Long): SegwitV0SpendingInfo =
    copy(id = Some(id))

  /** Updates the `blockHash` field */
  override def copyWithBlockHash(
      blockHash: DoubleSha256DigestBE): SegwitV0SpendingInfo =
    copy(blockHash = Some(blockHash))
}

/**
  * DB representation of a legacy UTXO
  */
case class LegacySpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: LegacyHDPath,
    state: TxoState,
    txid: DoubleSha256DigestBE,
    blockHash: Option[DoubleSha256DigestBE],
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override def scriptWitnessOpt: Option[ScriptWitness] = None

  override type PathType = LegacyHDPath
  type SpendingInfoType = LegacySpendingInfo

  override def copyWithId(id: Long): LegacySpendingInfo =
    copy(id = Some(id))

  override def copyWithState(state: TxoState): LegacySpendingInfo =
    copy(state = state)

  override def copyWithBlockHash(
      blockHash: DoubleSha256DigestBE): LegacySpendingInfo =
    copy(blockHash = Some(blockHash))
}

/**
  * DB representation of a nested segwit V0
  * SegWit UTXO
  */
case class NestedSegwitV0SpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: NestedSegWitHDPath,
    redeemScript: ScriptPubKey,
    scriptWitness: ScriptWitness,
    txid: DoubleSha256DigestBE,
    state: TxoState,
    blockHash: Option[DoubleSha256DigestBE],
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = NestedSegWitHDPath
  override type SpendingInfoType = NestedSegwitV0SpendingInfo

  override def copyWithState(state: TxoState): NestedSegwitV0SpendingInfo =
    copy(state = state)

  override def copyWithId(id: Long): NestedSegwitV0SpendingInfo =
    copy(id = Some(id))

  /** Updates the `blockHash` field */
  override def copyWithBlockHash(
      blockHash: DoubleSha256DigestBE): NestedSegwitV0SpendingInfo =
    copy(blockHash = Some(blockHash))
}

/**
  * The database level representation of a UTXO.
  * When storing a UTXO we don't want to store
  * sensitive material such as private keys.
  * We instead store the necessary information
  * we need to derive the private keys, given
  * the root wallet seed.
  */
sealed trait SpendingInfoDb extends DbRowAutoInc[SpendingInfoDb] {

  state match {
    case TxoState.ConfirmedSpent | TxoState.ConfirmedReceived =>
      require(blockHash.isDefined,
              "Transaction cannot be confirmed without a blockHash")
    case TxoState.DoesNotExist | TxoState.PendingConfirmationsSpent |
        TxoState.PendingConfirmationsReceived | TxoState.Reserved =>
      ()
  }

  protected type PathType <: HDPath

  /** This type is here to ensure copyWithSpent returns the same
    * type as the one it was called on.
    */
  protected type SpendingInfoType <: SpendingInfoDb

  def id: Option[Long]
  def outPoint: TransactionOutPoint
  def output: TransactionOutput
  def privKeyPath: PathType
  def redeemScriptOpt: Option[ScriptPubKey]
  def scriptWitnessOpt: Option[ScriptWitness]

  val hashType: HashType = HashType.sigHashAll

  /** The current [[org.bitcoins.core.wallet.utxo.TxoState state]] of the utxo */
  def state: TxoState

  /** The TXID of the transaction this output was received in */
  def txid: DoubleSha256DigestBE

  /** The hash of the block in which the transaction was included */
  def blockHash: Option[DoubleSha256DigestBE]

  /** Converts the UTXO to the canonical `txid:vout` format */
  def toHumanReadableString: String =
    s"${outPoint.txId.flip.hex}:${outPoint.vout.toInt}"

  /** Updates the `spent` field */
  def copyWithState(state: TxoState): SpendingInfoType

  /** Updates the `blockHash` field */
  def copyWithBlockHash(blockHash: DoubleSha256DigestBE): SpendingInfoType

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toUTXOSpendingInfo(
      keyManager: BIP39KeyManager): BitcoinUTXOSpendingInfoFull = {

    val sign: Sign = keyManager.toSign(privKeyPath = privKeyPath)

    toUTXOSpendingInfo(sign = sign)
  }

  def toUTXOSpendingInfo(sign: Sign): BitcoinUTXOSpendingInfoFull = {
    BitcoinUTXOSpendingInfoFull(
      outPoint,
      output,
      Vector(sign),
      redeemScriptOpt,
      scriptWitnessOpt,
      hashType,
      ConditionalPath.NoConditionsLeft) // TODO: Migrate to add the Column for this (default: NoConditionsLeft)
  }

}
