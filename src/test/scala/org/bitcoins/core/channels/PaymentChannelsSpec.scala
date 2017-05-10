package org.bitcoins.core.channels

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
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
import scala.util.Try

/**
  * Created by chris on 4/18/17.
  */
class PaymentChannelsSpec extends Properties("PaymentChannelProperties") with BitcoinSLogger {

  property("spend a anchor transaction with the first spendingTx in a payment channel") = {
    Prop.forAll(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,_) =>
      val p = ScriptProgram(inProgress.current)
      val result = ScriptInterpreter.run(p)
      result == ScriptOk
    }
  }

  property("fail to increment a payment channel when the values are larger than the locked output") = {
    Prop.forAll(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,privKeys) =>
      val inc = inProgress.clientSign(inProgress.lockedAmount + Satoshis.one, privKeys.head,HashType.sigHashAll)
      inc.isFailure
    }
  }

  property("increment a payment channel, then close it") = {
    Prop.forAllNoShrink(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,privKeys) =>
      val num = Gen.choose(1,20).sample.get
      val amount = Satoshis(Int64(1))
      val closedTry = ChannelGenerators.simulate(num,inProgress,amount,privKeys.head,privKeys(1)).map(_.close)
      val result = closedTry.map(closed => verifyPaymentChannel(closed,amount))
      if (result.isFailure) {
        throw result.failed.get
      } else result.get
    }
  }

  def verifyPaymentChannel(p: PaymentChannelClosed, amount: CurrencyUnit): Boolean = {
    @tailrec
    def loop(last: WitnessTxSigComponent, remaining: Seq[WitnessTxSigComponent]): Boolean = {
      if (remaining.isEmpty) true
      else {
        val current = remaining.head
        val p = ScriptProgram(current)
        val interpreterResult = ScriptInterpreter.run(p)
        val isValidTx = interpreterResult == ScriptOk
        if (!isValidTx) logger.error("Invalid tx when verifying payment channel, got error: " + interpreterResult)
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
