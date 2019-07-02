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
case class SegwitV0SpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: SegWitHDPath,
    scriptWitness: ScriptWitness,
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath

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
    id: Option[Long] = None
) extends SpendingInfoDb {
  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override def scriptWitnessOpt: Option[ScriptWitness] = None

  override type PathType = LegacyHDPath

  override def copyWithId(id: Long): LegacySpendingInfo =
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
sealed trait SpendingInfoDb
    extends DbRowAutoInc[SpendingInfoDb]
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

/**
  * This table stores the necessary information to spend
  * a TXO at a later point in time.
  *
  * It does not contain informations about whether or not
  * it is spent, how many (if any) confirmations it has
  * or which block/transaction it was included in.
  *
  * That is rather handled by
  * [[org.bitcoins.wallet.models.WalletTXOTable WalletTXOTable]].
  */
case class SpendingInfoTable(tag: Tag)
    extends TableAutoInc[SpendingInfoDb](tag, "txo_spending_info") {
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

  private type UTXOTuple = (
      Option[Long],
      TransactionOutPoint,
      TransactionOutput,
      HDPath,
      Option[ScriptPubKey],
      Option[ScriptWitness]
  )

  private val fromTuple: UTXOTuple => SpendingInfoDb = {
    case (id,
          outpoint,
          output,
          path: SegWitHDPath,
          None, // ReedemScript
          Some(scriptWitness)) =>
      SegwitV0SpendingInfo(outpoint, output, path, scriptWitness, id)

    case (id,
          outpoint,
          output,
          path: LegacyHDPath,
          None, // RedeemScript
          None // ScriptWitness
        ) =>
      LegacySpendingInfo(outpoint, output, path, id)
    case (id, outpoint, output, path, spkOpt, swOpt) =>
      throw new IllegalArgumentException(
        "Could not construct UtxoSpendingInfoDb from bad tuple:"
          + s" ($outpoint, $output, $path, $spkOpt, $swOpt, $id) . Note: Nested Segwit is not implemented")

  }

  private val toTuple: SpendingInfoDb => Option[UTXOTuple] =
    utxo =>
      Some(
        (utxo.id,
         utxo.outPoint,
         utxo.output,
         utxo.privKeyPath,
         utxo.redeemScriptOpt,
         utxo.scriptWitnessOpt))

  def * : ProvenShape[SpendingInfoDb] =
    (id.?, outPoint, output, privKeyPath, redeemScriptOpt, scriptWitnessOpt) <> (fromTuple, toTuple)
}
