package org.bitcoins.core.gen

import org.bitcoins.core.channels._
import org.bitcoins.core.crypto.{ECDigitalSignature, ECPrivateKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.scalacheck.Gen

import scala.annotation.tailrec
import scala.util.Try

/**
  * Created by chris on 4/18/17.
  */
trait ChannelGenerators extends BitcoinSLogger {

  def anchorTx: Gen[(AnchorTransaction, EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (redeemScript,privKeys) <- ScriptGenerators.escrowTimeoutScriptPubKey2Of2
    amount <- CurrencyUnitGenerator.satoshis.suchThat(_ >= Policy.minPaymentChannelAmount)
    p2sh = P2SHScriptPubKey(redeemScript)
    (aTx,_) = TransactionGenerators.buildCreditingTransaction(TransactionConstants.validLockVersion,p2sh,amount)
  } yield (AnchorTransaction(aTx),redeemScript,privKeys)

  def paymentChannelAwaitingAnchorTx: Gen[(PaymentChannelAwaitingAnchorTx, Seq[ECPrivateKey])] = for {
    (aTx,redeemScript,privKeys) <- anchorTx
  } yield (PaymentChannelAwaitingAnchorTx(aTx,redeemScript,Policy.confirmations).get,privKeys)

  def freshPaymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (awaiting,privKeys) <- paymentChannelAwaitingAnchorTx
    (s1,_) <- ScriptGenerators.scriptPubKey
    amount = Policy.minPaymentChannelAmount
    clientSigned = awaiting.clientSign(s1,amount,privKeys.head).get
    fullySigned = clientSigned.serverSign(privKeys(1))
  } yield (fullySigned.get,privKeys)


  def paymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (old,privKeys) <- freshPaymentChannelInProgress
    runs <- Gen.choose(1,10)
    amount = Policy.dustThreshold
    inProgress = simulate(runs,old,amount,privKeys.head,privKeys(1))
  } yield (inProgress.get, privKeys)

  /** Generator for a payment channel that opened, simulated, then closed */
  def paymentChannelClosed: Gen[(PaymentChannelClosed, Seq[ECPrivateKey])] = for {
    (inProgress, privKeys) <- paymentChannelInProgress
    (serverScriptPubKey,_) <- ScriptGenerators.scriptPubKey
    (clientKey,serverKey) = (privKeys.head, privKeys(1))
    amount = Policy.dustThreshold
    fee = amount
    clientSigned = inProgress.clientSign(amount,clientKey)
    closed = clientSigned.flatMap(_.close(serverScriptPubKey,serverKey,fee))
  } yield (closed.get,privKeys)


  def simulate(runs: Int, inProgress: PaymentChannelInProgress, amount: CurrencyUnit,
                       clientKey: ECPrivateKey, serverKey: ECPrivateKey): Try[PaymentChannelInProgress] = {
    @tailrec
    def loop(old: Try[PaymentChannelInProgress], remaining: Int): Try[PaymentChannelInProgress] = {
      if (old.isFailure || remaining == 0) old
      else {
        val clientSigned = old.flatMap(_.clientSign(amount,clientKey))
        val serverSigned = clientSigned.flatMap(c => c.serverSign(serverKey))
        loop(serverSigned,remaining - 1)
      }
    }
    loop(Try(inProgress),runs)

  }
}

object ChannelGenerators extends ChannelGenerators
