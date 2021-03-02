package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  ScriptSignatureParams,
  TxoState
}
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sign}

/** DB representation of a native V0
  * SegWit UTXO
  */
case class SegwitV0SpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: SegWitHDPath,
    scriptWitness: ScriptWitness,
    txid: DoubleSha256DigestBE,
    state: TxoState,
    spendingTxIdOpt: Option[DoubleSha256DigestBE],
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath
  override type SpendingInfoType = SegwitV0SpendingInfo

  override def copyWithState(state: TxoState): SegwitV0SpendingInfo =
    copy(state = state)

  override def copyWithId(id: Long): SegwitV0SpendingInfo =
    copy(id = Some(id))

  /** Updates the `spendingTxId` field */
  override def copyWithSpendingTxId(
      txId: DoubleSha256DigestBE): SegwitV0SpendingInfo =
    copy(spendingTxIdOpt = Some(txId))
}

/** DB representation of a legacy UTXO
  */
case class LegacySpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: LegacyHDPath,
    state: TxoState,
    txid: DoubleSha256DigestBE,
    spendingTxIdOpt: Option[DoubleSha256DigestBE],
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

  /** Updates the `spendingTxId` field */
  override def copyWithSpendingTxId(
      txId: DoubleSha256DigestBE): LegacySpendingInfo =
    copy(spendingTxIdOpt = Some(txId))
}

/** DB representation of a nested segwit V0
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
    spendingTxIdOpt: Option[DoubleSha256DigestBE],
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

  /** Updates the `spendingTxId` field */
  override def copyWithSpendingTxId(
      txId: DoubleSha256DigestBE): NestedSegwitV0SpendingInfo =
    copy(spendingTxIdOpt = Some(txId))
}

/** The database level representation of a UTXO.
  * When storing a UTXO we don't want to store
  * sensitive material such as private keys.
  * We instead store the necessary information
  * we need to derive the private keys, given
  * the root wallet seed.
  */
sealed trait SpendingInfoDb extends DbRowAutoInc[SpendingInfoDb] {

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

  def isChange: Boolean = privKeyPath.chain.chainType == HDChainType.Change

  /** The current [[org.bitcoins.core.wallet.utxo.TxoState state]] of the utxo */
  def state: TxoState

  /** The TXID of the transaction this output was received in */
  def txid: DoubleSha256DigestBE

  /** TxId of the transaction that this output was spent by */
  def spendingTxIdOpt: Option[DoubleSha256DigestBE]

  /** Converts the UTXO to the canonical `txid:vout` format */
  def toHumanReadableString: String =
    s"${outPoint.txId.flip.hex}:${outPoint.vout.toInt}"

  /** Updates the `spent` field */
  def copyWithState(state: TxoState): SpendingInfoType

  /** Updates the `spendingTxId` field */
  def copyWithSpendingTxId(txId: DoubleSha256DigestBE): SpendingInfoType

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toUTXOInfo(
      keyManager: BIP39KeyManagerApi,
      prevTransaction: Transaction): ScriptSignatureParams[InputInfo] = {

    val sign: Sign = keyManager.toSign(privKeyPath = privKeyPath)

    toUTXOInfo(sign = sign, prevTransaction)
  }

  def toUTXOInfo(
      sign: Sign,
      prevTransaction: Transaction): ScriptSignatureParams[InputInfo] = {
    ScriptSignatureParams(
      InputInfo(
        outPoint,
        output,
        redeemScriptOpt,
        scriptWitnessOpt,
        ConditionalPath.NoCondition, // TODO: Migrate to add the Column for this (default: NoConditionsLeft)
        Vector(sign.publicKey)
      ),
      prevTransaction,
      Vector(sign),
      hashType
    )
  }

}
