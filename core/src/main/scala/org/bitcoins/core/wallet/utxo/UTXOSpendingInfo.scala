package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, ScriptPubKey, ScriptWitnessV0, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, TransactionOutput}
import org.bitcoins.core.script.crypto.HashType

/**
  * Contains the information required to spend a unspent transaction output (UTXO)
  * on a blockchain.
  */
sealed abstract class UTXOSpendingInfo {

  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  /** the actual output itself we are spending */
  def output: TransactionOutput = {
    TransactionOutput(value = amount,
      scriptPubKey = scriptPubKey)
  }

  /** the signers needed to spend from the output above */
  def signers: Seq[Sign]

  def hashType: HashType
}

sealed trait BitcoinUTXOSpendingInfo extends UTXOSpendingInfo

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
case class RawScriptUTXOSpendingInfo(outPoint: TransactionOutPoint,
                                     amount: CurrencyUnit,
                                     scriptPubKey: ScriptPubKey,
                                     signers: Seq[Sign],
                                     hashType: HashType) extends BitcoinUTXOSpendingInfo


/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
case class SegwitV0NativeUTXOSpendingInfo(outPoint: TransactionOutPoint,
                                          amount: CurrencyUnit,
                                          scriptPubKey: WitnessScriptPubKeyV0,
                                          signers: Seq[Sign],
                                          hashType: HashType,
                                          scriptWitness: ScriptWitnessV0) extends BitcoinUTXOSpendingInfo

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHSpendingInfo(outPoint: TransactionOutPoint,
                            amount: CurrencyUnit,
                            scriptPubKey: P2SHScriptPubKey,
                            signers: Seq[Sign],
                            hashType: HashType,
                            redeemScript: ScriptPubKey) extends BitcoinUTXOSpendingInfo {
  require(P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}")
}


/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNestedSegwitV0UTXOSpendingInfo(outPoint: TransactionOutPoint,
                                              amount: CurrencyUnit,
                                                scriptPubKey: P2SHScriptPubKey,
                                               signers: Seq[Sign],
                                               hashType: HashType,
                                               redeemScript: WitnessScriptPubKeyV0,
                                               scriptWitness: ScriptWitnessV0
) extends BitcoinUTXOSpendingInfo {
  require(P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}")

  //can we put an invariant here with scriptWitness and the witness spk redeemScript?
}