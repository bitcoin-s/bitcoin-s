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
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.hd.LegacyHDPath

/**
  * DB representation of a native V0
  * SegWit UTXO
  */
case class NativeV0UTXOSpendingInfoDb(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: SegWitHDPath,
    scriptWitness: ScriptWitness,
    incomingTxId: Long,
    id: Option[Long] = None
) extends UTXOSpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath

  override def copyWithId(id: Long): NativeV0UTXOSpendingInfoDb =
    copy(id = Some(id))
}

case class LegacyUTXOSpendingInfoDb(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: LegacyHDPath,
    incomingTxId: Long,
    id: Option[Long] = None
) extends UTXOSpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override def scriptWitnessOpt: Option[ScriptWitness] = None

  override type PathType = LegacyHDPath

  override def copyWithId(id: Long): LegacyUTXOSpendingInfoDb =
    copy(id = Some(id))
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
sealed trait UTXOSpendingInfoDb
    extends DbRowAutoInc[UTXOSpendingInfoDb]
    with BitcoinSLogger {

  protected type PathType <: HDPath

  def id: Option[Long]
  def outPoint: TransactionOutPoint
  def output: TransactionOutput
  def privKeyPath: PathType
  def redeemScriptOpt: Option[ScriptPubKey]
  def scriptWitnessOpt: Option[ScriptWitness]

  val hashType: HashType = HashType.sigHashAll

  def value: CurrencyUnit = output.value

  /** The ID of the transaction this UTXO was received in */
  def incomingTxId: Long

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

    logger.info({
      val shortStr = s"${outPoint.txId.hex}:${outPoint.vout.toInt}"
      val detailsStr =
        s"scriptPubKey=${output.scriptPubKey}, amount=${output.value}, keyPath=${privKeyPath}, pubKey=${pubAtPath}"
      s"Converting DB UTXO $shortStr ($detailsStr) to spending info"
    })

    BitcoinUTXOSpendingInfo(outPoint,
                            output,
                            List(sign),
                            redeemScriptOpt,
                            scriptWitnessOpt,
                            hashType)
  }

}

case class UTXOSpendingInfoTable(tag: Tag)
    extends TableAutoInc[UTXOSpendingInfoDb](tag, "utxos") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def outPoint: Rep[TransactionOutPoint] =
    column[TransactionOutPoint]("tx_outpoint")

  def output: Rep[TransactionOutput] =
    column[TransactionOutput]("tx_output")

  def privKeyPath: Rep[HDPath] = column[HDPath]("hd_privkey_path")

  def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
    column[Option[ScriptPubKey]]("nullable_redeem_script")

  def scriptWitnessOpt: Rep[Option[ScriptWitness]] =
    column[Option[ScriptWitness]]("script_witness")

  /** The ID of the incoming transaction corresponding to this UTXO */
  def incomingTxId: Rep[Long] = column("incoming_tx_id")

  def fk_incomingTx =
    foreignKey("fk_incoming_tx",
               sourceColumns = incomingTxId,
               targetTableQuery = TableQuery[IncomingTransactionTable]) {
      _.id
    }

  private type UTXOTuple = (
      Option[Long],
      TransactionOutPoint,
      TransactionOutput,
      HDPath,
      Option[ScriptPubKey],
      Option[ScriptWitness],
      Long // incoming TX ID
  )

  private val fromTuple: UTXOTuple => UTXOSpendingInfoDb = {
    case (id,
          outpoint,
          output,
          path: SegWitHDPath,
          None, // ReedemScript
          Some(scriptWitness),
          txId) =>
      NativeV0UTXOSpendingInfoDb(outpoint,
                                 output,
                                 path,
                                 scriptWitness,
                                 txId,
                                 id)

    case (id,
          outpoint,
          output,
          path: LegacyHDPath,
          None, // RedeemScript
          None, // ScriptWitness
          txId) =>
      LegacyUTXOSpendingInfoDb(outpoint, output, path, txId, id)
    case (id, outpoint, output, path, spkOpt, swOpt, txId) =>
      throw new IllegalArgumentException(
        "Could not construct UtxoSpendingInfoDb from bad tuple:"
          + s" ($outpoint, $output, $path, $spkOpt, $swOpt, $txId, $id) . Note: Nested Segwit is not implemented")

  }

  private val toTuple: UTXOSpendingInfoDb => Option[UTXOTuple] =
    utxo =>
      Some(
        (utxo.id,
         utxo.outPoint,
         utxo.output,
         utxo.privKeyPath,
         utxo.redeemScriptOpt,
         utxo.scriptWitnessOpt,
         utxo.incomingTxId))

  def * : ProvenShape[UTXOSpendingInfoDb] =
    (id.?,
     outPoint,
     output,
     privKeyPath,
     redeemScriptOpt,
     scriptWitnessOpt,
     incomingTxId) <> (fromTuple, toTuple)
}
