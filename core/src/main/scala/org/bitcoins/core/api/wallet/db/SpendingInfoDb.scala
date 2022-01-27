package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script.{
  P2SHScriptPubKey,
  RawScriptPubKey,
  ScriptPubKey,
  ScriptWitness,
  WitnessScriptPubKey
}
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

  if (TxoState.spentStates.contains(state)) {
    require(
      spendingTxIdOpt.isDefined,
      s"If we have spent a spendinginfodb, the spendingTxId must be defined. Outpoint=${outPoint.toString}")
  }

  /** This type is here to ensure copyWithSpent returns the same
    * type as the one it was called on.
    */
  protected type SpendingInfoType <: SpendingInfoDb

  def id: Option[Long]
  def outPoint: TransactionOutPoint
  def output: TransactionOutput
  def privKeyPath: HDPath
  def redeemScriptOpt: Option[ScriptPubKey]
  def scriptWitnessOpt: Option[ScriptWitness]

  val hashType: HashType = HashType.sigHashAll

  def isChange: Boolean = privKeyPath.chain.chainType == HDChainType.Change

  /** The current [[org.bitcoins.core.wallet.utxo.TxoState state]] of the utxo */
  def state: TxoState

  /** The TXID of the transaction this output was received in */
  def txid: DoubleSha256DigestBE

  require(
    txid == outPoint.txIdBE,
    s"Cannot have different outpoint txId and txId outpoint=${outPoint.txIdBE.hex} txId=${txid.hex}")

  /** TxId of the transaction that this output was spent by */
  def spendingTxIdOpt: Option[DoubleSha256DigestBE]

  require(
    spendingTxIdOpt.map(_ != txid).getOrElse(true),
    s"txid and the spendingTxId cannot be the same, txid=${txid.hex} spendingTxId=${spendingTxIdOpt.get.hex}"
  )

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

object SpendingInfoDb {

  def apply(
      id: Option[Long],
      outpoint: TransactionOutPoint,
      output: TransactionOutput,
      hdPath: HDPath,
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      state: TxoState,
      txId: DoubleSha256DigestBE,
      spendingTxIdOpt: Option[DoubleSha256DigestBE]): SpendingInfoDb = {
    require(
      txId == outpoint.txIdBE,
      s"Outpoint and crediting txid not the same, got=$txId expected=${outpoint.txIdBE}")

    output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        if (scriptWitnessOpt.isDefined) {
          require(
            hdPath.isInstanceOf[NestedSegWitHDPath],
            s"hdPath must be SegwitHdPath for SegwitV0SpendingInfo, got=$hdPath")
          NestedSegwitV0SpendingInfo(outpoint,
                                     output,
                                     hdPath.asInstanceOf[NestedSegWitHDPath],
                                     redeemScriptOpt.get,
                                     scriptWitnessOpt.get,
                                     txId,
                                     state,
                                     spendingTxIdOpt,
                                     id)
        } else {
          require(
            hdPath.isInstanceOf[LegacyHDPath],
            s"hdPath must be LegacyHDPath for LegacySpendingInfo, got=$hdPath")
          LegacySpendingInfo(outPoint = outpoint,
                             output = output,
                             privKeyPath = hdPath.asInstanceOf[LegacyHDPath],
                             state = state,
                             txid = txId,
                             spendingTxIdOpt = spendingTxIdOpt,
                             id = id)
        }
      case _: WitnessScriptPubKey =>
        require(
          hdPath.isInstanceOf[SegWitHDPath],
          s"hdPath must be SegwitHdPath for SegwitV0SpendingInfo, got=$hdPath")
        require(scriptWitnessOpt.isDefined,
                s"ScriptWitness must be defined for SegwitV0SpendingInfo")
        SegwitV0SpendingInfo(
          outPoint = outpoint,
          output = output,
          privKeyPath = hdPath.asInstanceOf[SegWitHDPath],
          scriptWitness = scriptWitnessOpt.get,
          txid = txId,
          state = state,
          spendingTxIdOpt = spendingTxIdOpt,
          id = id
        )
      case _: RawScriptPubKey =>
        require(
          hdPath.isInstanceOf[LegacyHDPath],
          s"hdPath must be LegacyHDPath for LegacySpendingInfo, got=$hdPath")
        LegacySpendingInfo(outPoint = outpoint,
                           output = output,
                           privKeyPath = hdPath.asInstanceOf[LegacyHDPath],
                           state = state,
                           txid = txId,
                           spendingTxIdOpt = spendingTxIdOpt,
                           id = id)
    }
  }
}
