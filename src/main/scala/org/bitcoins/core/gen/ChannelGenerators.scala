package org.bitcoins.core.gen

import org.bitcoins.core.channels.{AnchorTransaction, PaymentChannel, PaymentChannelAwaitingAnchorTx, PaymentChannelInProgress}
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, ScriptWitness, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionOutput}
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.scalacheck.Gen

import scala.util.Try

/**
  * Created by chris on 4/18/17.
  */
trait ChannelGenerators extends BitcoinSLogger {

  def anchorTx: Gen[(AnchorTransaction, EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (redeemScript,privKeys) <- ScriptGenerators.escrowTimeoutScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis.suchThat(_ >= Policy.minPaymentChannelAmount)
    wit = WitnessScriptPubKeyV0(redeemScript)
    (aTx,_) = TransactionGenerators.buildCreditingTransaction(UInt32(2),wit,amount)
  } yield (AnchorTransaction(aTx),redeemScript,privKeys)

  def paymentChannelAwaitingAnchorTx: Gen[(PaymentChannelAwaitingAnchorTx, Seq[ECPrivateKey])] = for {
    (aTx,redeemScript,privKeys) <- anchorTx
  } yield (PaymentChannelAwaitingAnchorTx(aTx,redeemScript,Policy.confirmations).get,privKeys)

  def freshPaymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (awaiting,privKeys) <- paymentChannelAwaitingAnchorTx
    hashType <- CryptoGenerators.hashType
    (s1,_) <- ScriptGenerators.scriptPubKey
    (s2,_) <- ScriptGenerators.scriptPubKey
  } yield (awaiting.createInProgress(s1,s2,privKeys,hashType).get,privKeys)


  def paymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (old,privKeys) <- freshPaymentChannelInProgress
    amount = CurrencyUnits.one
    updatedChannel = old.increment(amount,privKeys,HashType.sigHashAll)
  } yield (updatedChannel.get, privKeys)


}

object ChannelGenerators extends ChannelGenerators
