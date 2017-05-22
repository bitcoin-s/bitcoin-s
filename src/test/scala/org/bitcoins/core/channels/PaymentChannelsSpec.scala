package org.bitcoins.core.channels

import org.bitcoins.core.crypto.{BaseTxSigComponent, ECPrivateKey, TxSigComponent, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.{ChannelGenerators, ScriptGenerators}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Prop, Properties}

import scala.annotation.tailrec
import scala.util.Try

/**
  * Created by chris on 4/18/17.
  */
class PaymentChannelsSpec extends Properties("PaymentChannelProperties") with BitcoinSLogger {

  property("spend a anchor transaction with the first spendingTx in a payment channel") = {
    Prop.forAllNoShrink(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,_) =>
      val p = ScriptProgram(inProgress.current)
      val result = ScriptInterpreter.run(p)
      result == ScriptOk
    }
  }

  property("fail to increment a payment channel when the values are larger than the locked output") = {
    Prop.forAllNoShrink(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,privKeys) =>
      val inc = inProgress.clientSign(inProgress.lockedAmount + Satoshis.one, privKeys.head)
      inc.isFailure
    }
  }

  property("increment a payment channel, then close it") = {
    Prop.forAllNoShrink(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,privKeys) =>
      val num = Gen.choose(1,20).sample.get
      val serverScriptPubKey = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
      val amount = Satoshis.one
      val fee = Satoshis(Int64(100))
      val (clientKey,serverKey) = (privKeys.head, privKeys(1))
      val simulated = ChannelGenerators.simulate(num,inProgress,amount,clientKey,serverKey)
      val clientSigned = simulated.flatMap(_.clientSign(amount,clientKey))
      val closedTry = clientSigned.flatMap(_.close(serverScriptPubKey,serverKey,fee))
      val result = closedTry.map(closed => verifyPaymentChannel(closed,amount,fee))
      if (result.isFailure) {
        throw result.failed.get
      } else result.get
    }
  }

  def verifyPaymentChannel(p: PaymentChannelClosed, amount: CurrencyUnit, fee: CurrencyUnit): Boolean = {
    @tailrec
    def loop(last: BaseTxSigComponent, remaining: Seq[BaseTxSigComponent]): Boolean = {
      if (remaining.isEmpty) true
      else {
        val current = remaining.head
        val program = ScriptProgram(current)
        val interpreterResult = ScriptInterpreter.run(program)
        val isValidTx = interpreterResult == ScriptOk
        if (!isValidTx) logger.error("Invalid tx when verifying payment channel, got error: " + interpreterResult)
        val lastClientOutput = last.transaction.outputs.head
        val currentClientOutput = current.transaction.outputs.head
        val expectedClientOutput = TransactionOutput(lastClientOutput,lastClientOutput.value - amount)
        val serverOutputIsValid = if (remaining.size == 1) {
          //check server output
          val serverOutput = current.transaction.outputs(1)
          val expectedServerAmount = (amount * Satoshis(Int64(p.old.size + 1))) - fee
          expectedServerAmount == serverOutput.value
        } else {
          true
        }
        val result = (isValidTx && currentClientOutput == expectedClientOutput && serverOutputIsValid)
        if (result) loop(current,remaining.tail)
        else false
      }
    }
    val inOrder = p.old.reverse
    loop(inOrder.head, inOrder.tail ++ Seq(p.finalTx))
  }
}
