package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{DoubleSha256DigestBE, Sign}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{HDPath, LegacyHDPath, SegWitHDPath}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalPath,
  TxoState
}
import org.bitcoins.db.{DbRowAutoInc, TableAutoInc}
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

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

// TODO add case for nested segwit
/**
  * The database level representation of a UTXO.
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
      account: AccountDb,
      keyManager: BIP39KeyManager,
      network: NetworkParameters): BitcoinUTXOSpendingInfo = {

    val sign: Sign = keyManager.toSign(privKeyPath = privKeyPath)

    BitcoinUTXOSpendingInfo(
      outPoint,
      output,
      List(sign),
      redeemScriptOpt,
      scriptWitnessOpt,
      hashType,
      ConditionalPath.NoConditionsLeft) // TODO: Migrate to add the Column for this (default: NoConditionsLeft)
  }

}

/**
  * This table stores the necessary information to spend
  * a transaction output (TXO) at a later point in time. It
  * also stores how many confirmations it has, whether
  * or not it is spent (i.e. if it is a UTXO or not) and the
  * TXID of the transaction that created this output.
  */
case class SpendingInfoTable(tag: Tag)
    extends TableAutoInc[SpendingInfoDb](tag, "txo_spending_info") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def outPoint: Rep[TransactionOutPoint] =
    column("tx_outpoint")

  def txid: Rep[DoubleSha256DigestBE] = column("txid")

  def state: Rep[TxoState] = column("txo_state")

  def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")

  def value: Rep[CurrencyUnit] = column("value")

  def privKeyPath: Rep[HDPath] = column("hd_privkey_path")

  def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
    column("redeem_script")

  def scriptWitnessOpt: Rep[Option[ScriptWitness]] = column("script_witness")

  def blockHash: Rep[Option[DoubleSha256DigestBE]] = column("block_hash")

  /** All UTXOs must have a SPK in the wallet that gets spent to */
  def fk_scriptPubKey = {
    val addressTable = TableQuery[AddressTable]
    foreignKey("fk_scriptPubKey",
               sourceColumns = scriptPubKey,
               targetTableQuery = addressTable)(_.scriptPubKey)
  }

  private type UTXOTuple = (
      Option[Long], // ID
      TransactionOutPoint,
      ScriptPubKey, // output SPK
      CurrencyUnit, // output value
      HDPath,
      Option[ScriptPubKey], // ReedemScript
      Option[ScriptWitness],
      TxoState, // state
      DoubleSha256DigestBE, // TXID
      Option[DoubleSha256DigestBE] // block hash
  )

  private val fromTuple: UTXOTuple => SpendingInfoDb = {
    case (id,
          outpoint,
          spk,
          value,
          path: SegWitHDPath,
          None, // ReedemScript
          Some(scriptWitness),
          state,
          txid,
          blockHash) =>
      SegwitV0SpendingInfo(
        outPoint = outpoint,
        output = TransactionOutput(value, spk),
        privKeyPath = path,
        scriptWitness = scriptWitness,
        id = id,
        state = state,
        txid = txid,
        blockHash = blockHash
      )

    case (id,
          outpoint,
          spk,
          value,
          path: LegacyHDPath,
          None, // RedeemScript
          None, // ScriptWitness
          state,
          txid,
          blockHash) =>
      LegacySpendingInfo(outPoint = outpoint,
                         output = TransactionOutput(value, spk),
                         privKeyPath = path,
                         id = id,
                         state = state,
                         txid = txid,
                         blockHash = blockHash)
    case (id,
          outpoint,
          spk,
          value,
          path,
          spkOpt,
          swOpt,
          spent,
          txid,
          blockHash) =>
      throw new IllegalArgumentException(
        "Could not construct UtxoSpendingInfoDb from bad tuple:"
          + s" ($id, $outpoint, $spk, $value, $path, $spkOpt, $swOpt, $spent, $txid, $blockHash)."
          + " Note: Nested Segwit is not implemented")

  }

  private val toTuple: SpendingInfoDb => Option[UTXOTuple] =
    utxo =>
      Some(
        (utxo.id,
         utxo.outPoint,
         utxo.output.scriptPubKey,
         utxo.output.value,
         utxo.privKeyPath,
         utxo.redeemScriptOpt,
         utxo.scriptWitnessOpt,
         utxo.state,
         utxo.txid,
         utxo.blockHash))

  def * : ProvenShape[SpendingInfoDb] =
    (id.?,
     outPoint,
     scriptPubKey,
     value,
     privKeyPath,
     redeemScriptOpt,
     scriptWitnessOpt,
     state,
     txid,
     blockHash) <> (fromTuple, toTuple)
}
