package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{
  HDPath,
  LegacyHDPath,
  NestedSegWitHDPath,
  SegWitHDPath
}
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.DoubleSha256DigestBE

case class UTXORecord(
    outpoint: TransactionOutPoint,
    txid: DoubleSha256DigestBE, // TXID
    state: TxoState, // state
    scriptPubKeyId: Long, // output SPK
    value: CurrencyUnit, // output value
    path: HDPath,
    redeemScript: Option[ScriptPubKey], // RedeemScript
    scriptWitness: Option[ScriptWitness],
    blockHash: Option[DoubleSha256DigestBE], // block hash
    id: Option[Long] = None
) extends DbRowAutoInc[UTXORecord] {
  override def copyWithId(id: Long): UTXORecord = copy(id = Option(id))

  def copyWithState(state: TxoState): UTXORecord = copy(state = state)

  def toSpendingInfoDb(scriptPubKey: ScriptPubKey): SpendingInfoDb =
    (path, redeemScript, scriptWitness) match {
      case (path: SegWitHDPath, None, Some(scriptWitness)) =>
        SegwitV0SpendingInfo(
          outPoint = outpoint,
          output = TransactionOutput(value, scriptPubKey),
          privKeyPath = path,
          scriptWitness = scriptWitness,
          id = id,
          state = state,
          txid = txid,
          blockHash = blockHash
        )

      case (path: LegacyHDPath, None, None) =>
        LegacySpendingInfo(outPoint = outpoint,
                           output = TransactionOutput(value, scriptPubKey),
                           privKeyPath = path,
                           id = id,
                           state = state,
                           txid = txid,
                           blockHash = blockHash)

      case (path: NestedSegWitHDPath, Some(redeemScript), Some(scriptWitness))
          if WitnessScriptPubKey.isValidAsm(redeemScript.asm) =>
        NestedSegwitV0SpendingInfo(outpoint,
                                   TransactionOutput(value, scriptPubKey),
                                   path,
                                   redeemScript,
                                   scriptWitness,
                                   txid,
                                   state,
                                   blockHash,
                                   id)

      case _ =>
        throw new IllegalArgumentException(
          s"Could not construct SpendingInfoDb from bad record: $this.")
    }
}

object UTXORecord {

  def fromSpendingInfoDb(
      spendingInfoDb: SpendingInfoDb,
      scriptPubKeyId: Long): UTXORecord =
    UTXORecord(
      spendingInfoDb.outPoint,
      spendingInfoDb.txid, // TXID
      spendingInfoDb.state, // state
      scriptPubKeyId, // output SPK
      spendingInfoDb.output.value, // output value
      spendingInfoDb.privKeyPath,
      spendingInfoDb.redeemScriptOpt, // ReedemScript
      spendingInfoDb.scriptWitnessOpt,
      spendingInfoDb.blockHash, // block hash
      spendingInfoDb.id
    )
}
