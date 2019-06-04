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
import org.bitcoins.core.hd.NestedSegWitHDPath

case class SegWitUTOXSpendingInfodb(
    id: Option[Long],
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: SegWitHDPath,
    scriptWitness: ScriptWitness
) extends UTXOSpendingInfoDb {
  override def redeemScriptOpt: Option[ScriptPubKey] = None
  override def scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)

  override type PathType = SegWitHDPath

  override def copyWithId(id: Long): SegWitUTOXSpendingInfodb =
    copy(id = Some(id))
}

// TODO add case for nested segwit
// and legacy
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

  private type UTXOTuple = (
      Option[Long],
      TransactionOutPoint,
      TransactionOutput,
      HDPath,
      Option[ScriptPubKey],
      Option[ScriptWitness])

  private val fromTuple: UTXOTuple => UTXOSpendingInfoDb = {
    case (id,
          outpoint,
          output,
          path: SegWitHDPath,
          None,
          Some(scriptWitness)) =>
      SegWitUTOXSpendingInfodb(id, outpoint, output, path, scriptWitness)
        .asInstanceOf[UTXOSpendingInfoDb]

    case (id,
          outpoint,
          output,
          path @ (_: LegacyHDPath | _: NestedSegWitHDPath),
          spkOpt,
          swOpt) =>
      throw new IllegalArgumentException(
        "Could not construct UtxoSpendingInfoDb from bad tuple:"
          + s" ($id, $outpoint, $output, $path, $spkOpt, $swOpt) . Note: Only Segwit is implemented")

  }

  private val toTuple: UTXOSpendingInfoDb => Option[UTXOTuple] =
    utxo =>
      Some(
        (utxo.id,
         utxo.outPoint,
         utxo.output,
         utxo.privKeyPath,
         utxo.redeemScriptOpt,
         utxo.scriptWitnessOpt))

  def * : ProvenShape[UTXOSpendingInfoDb] =
    (id.?, outPoint, output, privKeyPath, redeemScriptOpt, scriptWitnessOpt) <> (fromTuple, toTuple)
}
