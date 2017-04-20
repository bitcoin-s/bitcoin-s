package org.bitcoins.core.channels

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.gen.ChannelGenerators
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

/**
  * Created by chris on 4/18/17.
  */
class PaymentChannelsSpec extends Properties("PaymentChannelProperties") with BitcoinSLogger {

  property("spend a anchor transaction with the first spendingTx in a payment channel") =
    Prop.forAll(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,_) =>
        val p = ScriptProgram(inProgress.current)
        val result = ScriptInterpreter.run(p)
        result == ScriptOk
    }

  property("spend a current transaction where it is not the first payment in the channel") =
    Prop.forAll(ChannelGenerators.paymentChannelInProgress) { case (inProgress,_) =>
        val p = ScriptProgram(inProgress.current)
        val result = ScriptInterpreter.run(p)
        result == ScriptOk && inProgress.old.nonEmpty
    }


  property("increment a payment channel, then close it") =
    Prop.forAll(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,privKeys) =>
        val num = Gen.choose(1,20).sample.get
        val amount = Satoshis(Int64(1))
        val closed = simulate(num,inProgress,amount,privKeys)
        verifyPaymentChannel(closed,amount)
    }


  private def simulate(runs: Int, inProgress: PaymentChannelInProgress, amount: CurrencyUnit, privKeys: Seq[ECPrivateKey]): PaymentChannelClosed = {
    @tailrec
    def loop(old: PaymentChannelInProgress, remaining: Int): PaymentChannelInProgress = {
      if (remaining == 0) old
      else {
        val inc = old.increment(amount,privKeys,HashType.sigHashAll)
        loop(inc,remaining - 1)
      }
    }
    loop(inProgress,runs).close
  }

  def verifyPaymentChannel(p: PaymentChannelClosed, amount: CurrencyUnit): Boolean = {
    @tailrec
    def loop(last: WitnessTxSigComponent, remaining: Seq[WitnessTxSigComponent]): Boolean = {
      if (remaining.isEmpty) true
      else {
        val current = remaining.head
        val p = ScriptProgram(current)
        val isValidTx = ScriptInterpreter.run(p) == ScriptOk
        val Seq(lastClientOutput, lastServerOutput) = last.transaction.outputs.take(2)
        val Seq(currentClientOutput, currentServerOutput) = current.transaction.outputs.take(2)
        val expectedClientOutput = TransactionOutput(lastClientOutput,lastClientOutput.value - amount)
        val expectedServerOutput = TransactionOutput(lastServerOutput, lastServerOutput.value + amount)
        val result = (isValidTx &&
          currentClientOutput == expectedClientOutput &&
          currentServerOutput == expectedServerOutput)
        if (result) loop(current,remaining.tail)
        else false

      }
    }
    val inOrder = p.old.reverse
    loop(inOrder.head, inOrder.tail ++ Seq(p.finalTx))
  }
}
