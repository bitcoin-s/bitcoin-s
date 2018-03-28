package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.protocol.script.{ ScriptPubKey, ScriptWitness }
import org.bitcoins.core.protocol.transaction.{ TransactionOutPoint, TransactionOutput }
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.signer.Signer

/**
 * Contains the information required to spend a unspent transaction output (UTXO)
 * on a blockchain.
 */
sealed abstract class UTXOSpendingInfo {
  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint
  /** the actual output itself we are spending */
  def output: TransactionOutput
  /** the signers needed to spend from the output above */
  def signers: Seq[Signer.Sign]
  /** a redeemScript, if required, to spend the output above */
  def redeemScriptOpt: Option[ScriptPubKey]
  /** the scriptWitness, if required, to spend the output above */
  def scriptWitnessOpt: Option[ScriptWitness]

  def hashType: HashType
}

case class BitcoinUTXOSpendingInfo(outPoint: TransactionOutPoint, output: TransactionOutput,
  signers: Seq[Signer.Sign],
  redeemScriptOpt: Option[ScriptPubKey],
  scriptWitnessOpt: Option[ScriptWitness],
  hashType: HashType) extends UTXOSpendingInfo

