package org.bitcoins.core.channels

import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.currency.{ CurrencyUnit, Satoshis }
import org.bitcoins.core.gen.{ ChannelGenerators, ScriptGenerators }
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{ Gen, Prop, Properties }

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
/**
 * Created by chris on 4/18/17.
 */
class ChannelsSpec extends Properties("ChannelProperties") {
  private val logger = BitcoinSLogger.logger
  val timeout = 5.seconds
  property("spend a anchor transaction with the first spendingTx in a payment channel") = {
    Prop.forAllNoShrink(ChannelGenerators.freshChannelInProgress) {
      case (inProgress, _) =>
        val p = ScriptProgram(inProgress.current)
        val result = ScriptInterpreter.run(p)
        result == ScriptOk
    }
  }

  property("fail to increment a payment channel when the values are larger than the locked output") = {
    Prop.forAllNoShrink(ChannelGenerators.freshChannelInProgress) {
      case (inProgress, privKeys) =>
        val inc = Try(Await.result(inProgress.clientSign(inProgress.lockedAmount + Satoshis.one, privKeys.head), timeout))
        inc.isFailure
    }
  }

  property("increment a payment channel, then close it") = {
    Prop.forAllNoShrink(ChannelGenerators.freshChannelInProgress) {
      case (inProgress, privKeys) =>
        val num = Gen.choose(1, 20).sample.get
        val serverScriptPubKey = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
        val amount = Policy.dustThreshold
        val fee = Satoshis(Int64(100))
        val (clientKey, serverKey) = (privKeys.head, privKeys(1))
        val simulated = ChannelGenerators.simulate(num, inProgress, amount, clientKey, serverKey)
        val clientSigned = simulated.flatMap(_.clientSign(amount, clientKey))
        val closedFuture = clientSigned.flatMap(_.close(serverScriptPubKey, serverKey, fee))
        val closed = Await.result(closedFuture.map(closed => verifyChannel(closed, amount, fee)), timeout)
        closed
    }
  }

  property("close a payment channel awaiting anchor tx with the timeout branch") = {
    Prop.forAllNoShrink(ChannelGenerators.channelAwaitingAnchorTxNotConfirmed) {
      case (awaiting, privKeys) =>
        val channelClosedWithTimeout = awaiting.closeWithTimeout(EmptyScriptPubKey, privKeys(2), Satoshis.one)
        logger.info("closed.inputs: " + channelClosedWithTimeout.map(_.current.transaction.inputs))
        val program = channelClosedWithTimeout.map(c => ScriptProgram(c.current))
        val result = Await.result(program.map(p => ScriptInterpreter.run(p)), timeout)
        result == ScriptOk
    }
  }

  property("close a payment channel in progress with the timeout branch") = {
    Prop.forAllNoShrink(ChannelGenerators.baseInProgress) {
      case (inProgress, privKeys) =>
        val channelClosedWithTimeout = inProgress.closeWithTimeout(privKeys(2), Satoshis.one)
        val program = channelClosedWithTimeout.map(c => ScriptProgram(c.current))
        val result = Await.result(program.map(p => ScriptInterpreter.run(p)), timeout)
        result == ScriptOk
    }
  }

  def verifyChannel(p: ChannelClosed, amount: CurrencyUnit, fee: CurrencyUnit): Boolean = {
    @tailrec
    def loop(last: TxSigComponent, remaining: Seq[TxSigComponent]): Boolean = {
      if (remaining.isEmpty) true
      else {
        val current = remaining.head
        val program = ScriptProgram(current)
        val interpreterResult = ScriptInterpreter.run(program)
        val isValidTx = interpreterResult == ScriptOk
        if (!isValidTx) logger.error("Invalid tx when verifying payment channel, got error: " + interpreterResult)
        val lastClientOutput = last.transaction.outputs.head
        val currentClientOutput = current.transaction.outputs.head
        val expectedClientOutput = TransactionOutput(lastClientOutput.value - amount, lastClientOutput.scriptPubKey)
        val serverOutputIsValid = if (remaining.size == 1) {
          //check server output
          val serverOutput = current.transaction.outputs(1)
          // + Policy.minChannelAmount is for the first payment to the server
          val expectedServerAmount = ((amount * Satoshis(Int64(p.old.size))) - fee) + Policy.minChannelAmount
          expectedServerAmount == serverOutput.value
        } else {
          true
        }
        val result = (isValidTx && currentClientOutput == expectedClientOutput && serverOutputIsValid)
        if (result) loop(current, remaining.tail)
        else false
      }
    }
    val inOrder = p.old.reverse
    loop(inOrder.head, inOrder.tail ++ Seq(p.current))
  }
}
