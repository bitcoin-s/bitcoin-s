package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.crypto.bip44.BIP44Path
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.db.{DbRowAutoInc, TableAutoInc}
import org.bitcoins.wallet.api.UnlockedWalletApi
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

case class UTXOSpendingInfoDb(
    id: Option[Long],
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    privKeyPath: BIP44Path,
    redeemScriptOpt: Option[ScriptPubKey],
    scriptWitnessOpt: Option[ScriptWitness]
) extends DbRowAutoInc[UTXOSpendingInfoDb] {
  val hashType: HashType = HashType.sigHashAll

  def value: CurrencyUnit = output.value

  def toUTXOSpendingInfo(wallet: UnlockedWalletApi): BitcoinUTXOSpendingInfo = {

    val privAtPath = wallet.xpriv.deriveChildPrivKey(privKeyPath).key

    val sign: Sign = Sign(privAtPath.signFunction, privAtPath.publicKey)

    BitcoinUTXOSpendingInfo(outPoint,
                            output,
                            List(sign),
                            redeemScriptOpt,
                            scriptWitnessOpt,
                            hashType)
  }

  override def copyWithId(id: Long): UTXOSpendingInfoDb = copy(id = Some(id))
}

case class UTXOSpendingInfoTable(tag: Tag)
    extends TableAutoInc[UTXOSpendingInfoDb](tag, "utxos") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def outPoint: Rep[TransactionOutPoint] =
    column[TransactionOutPoint]("tx_outpoint")

  def output: Rep[TransactionOutput] =
    column[TransactionOutput]("tx_output")

  def privKeyPath: Rep[BIP44Path] = column[BIP44Path]("bip44_privkey_path")

  def redeemScriptOpt: Rep[Option[ScriptPubKey]] =
    column[Option[ScriptPubKey]]("nullable_redeem_script")

  def scriptWitnessOpt: Rep[Option[ScriptWitness]] =
    column[Option[ScriptWitness]]("script_witness")

  def * : ProvenShape[UTXOSpendingInfoDb] =
    (id.?, outPoint, output, privKeyPath, redeemScriptOpt, scriptWitnessOpt) <> (UTXOSpendingInfoDb.tupled, UTXOSpendingInfoDb.unapply)
}
