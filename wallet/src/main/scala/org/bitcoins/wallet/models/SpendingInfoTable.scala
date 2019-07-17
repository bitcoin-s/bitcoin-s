package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.db.{DbRowAutoInc, TableAutoInc}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.core.hd.HDPath

import org.bitcoins.core.hd.SegWitHDPath
import org.bitcoins.core.crypto.BIP39Seed
import org.bitcoins.core.hd.LegacyHDPath
import org.bitcoins.core.crypto.DoubleSha256DigestBE

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
    spent: Boolean,
    confirmations: Int,
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath
  override type SpendingInfoType = SegwitV0SpendingInfo

  def copyWithSpent(spent: Boolean): SegwitV0SpendingInfo = copy(spent = spent)

  def copyWithConfirmations(confirmations: Int): SegwitV0SpendingInfo =
    copy(confirmations = confirmations)

  override def copyWithId(id: Long): SegwitV0SpendingInfo =
    copy(id = Some(id))
}

/**
  * DB representation of a legacy UTXO
  */
case class LegacySpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: LegacyHDPath,
    confirmations: Int,
    spent: Boolean,
    txid: DoubleSha256DigestBE,
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override def scriptWitnessOpt: Option[ScriptWitness] = None

  override type PathType = LegacyHDPath
  type SpendingInfoType = LegacySpendingInfo

  override def copyWithId(id: Long): LegacySpendingInfo =
    copy(id = Some(id))

  def copyWithSpent(spent: Boolean): LegacySpendingInfo = copy(spent = spent)

  def copyWithConfirmations(confirmations: Int): LegacySpendingInfo =
    copy(confirmations = confirmations)
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

  /** How many confirmations this output has */
  // MOVE ME
  require(confirmations >= 0,
          s"Confirmations cannot be negative! Got: $confirmations")
  def confirmations: Int

  /** Whether or not this TXO is spent from our wallet */
  def spent: Boolean

  /** The TXID of the transaction this output was received in */
  def txid: DoubleSha256DigestBE

  /** Converts the UTXO to the canonical `txid:vout` format */
  def toHumanReadableString: String =
    s"${outPoint.txId.flip.hex}:${outPoint.vout.toInt}"

  /** Updates the `spent` field */
  def copyWithSpent(spent: Boolean): SpendingInfoType

  /** Updates the `confirmations` field */
  def copyWithConfirmations(confirmations: Int): SpendingInfoType

  /** Converts a non-sensitive DB representation of a UTXO into
    * a signable (and sensitive) real-world UTXO
    */
  def toUTXOSpendingInfo(
      account: AccountDb,
      walletSeed: BIP39Seed): BitcoinUTXOSpendingInfo = {

    val rootXpriv = walletSeed.toExtPrivateKey(account.xprivVersion)
    val xprivAtPath = rootXpriv.deriveChildPrivKey(privKeyPath)
    val privKey = xprivAtPath.key
    val pubAtPath = privKey.publicKey

    val sign: Sign = Sign(privKey.signFunction, pubAtPath)

    BitcoinUTXOSpendingInfo(outPoint,
                            output,
                            List(sign),
                            redeemScriptOpt,
                            scriptWitnessOpt,
                            hashType)
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

  def confirmations: Rep[Int] = column("confirmations")

  def spent: Rep[Boolean] = column("spent")

  def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key")

  def value: Rep[CurrencyUnit] = column("value")

  def privKeyPath: Rep[HDPath] = column("hd_privkey_path")

  def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
    column("redeem_script")

  def scriptWitnessOpt: Rep[Option[ScriptWitness]] = column("script_witness")

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
      Int, // confirmations
      Boolean, // spent
      DoubleSha256DigestBE // TXID
  )

  private val fromTuple: UTXOTuple => SpendingInfoDb = {
    case (id,
          outpoint,
          spk,
          value,
          path: SegWitHDPath,
          None, // ReedemScript
          Some(scriptWitness),
          confirmations,
          spent,
          txid) =>
      SegwitV0SpendingInfo(
        outPoint = outpoint,
        output = TransactionOutput(value, spk),
        privKeyPath = path,
        scriptWitness = scriptWitness,
        id = id,
        confirmations = confirmations,
        spent = spent,
        txid = txid
      )

    case (id,
          outpoint,
          spk,
          value,
          path: LegacyHDPath,
          None, // RedeemScript
          None, // ScriptWitness
          confirmations,
          spent,
          txid) =>
      LegacySpendingInfo(outPoint = outpoint,
                         output = TransactionOutput(value, spk),
                         privKeyPath = path,
                         id = id,
                         confirmations = confirmations,
                         spent = spent,
                         txid = txid)
    case (id, outpoint, spk, value, path, spkOpt, swOpt, confs, spent, txid) =>
      throw new IllegalArgumentException(
        "Could not construct UtxoSpendingInfoDb from bad tuple:"
          + s" ($id, $outpoint, $spk, $value, $path, $spkOpt, $swOpt, $confs, $spent, $txid)."
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
         utxo.confirmations,
         utxo.spent,
         utxo.txid))

  def * : ProvenShape[SpendingInfoDb] =
    (id.?,
     outPoint,
     scriptPubKey,
     value,
     privKeyPath,
     redeemScriptOpt,
     scriptWitnessOpt,
     confirmations,
     spent,
     txid) <> (fromTuple, toTuple)
}
