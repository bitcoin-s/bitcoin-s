package org.bitcoins.core.wallet.builder

import org.bitcoinj.wallet.Protos.TransactionOutput
import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.gen.{CurrencyUnitGenerator, ScriptGenerators, TransactionGenerators, TxOutputGen}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{TransactionOutPoint, TransactionOutput}
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

class TxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  property("sign a p2pkh tx and then have it verified") = {
    Prop.forAll(TxOutputGen.p2pkhOutput, TransactionGenerators.outputs) {
      case ((creditingTx,idx,key),destinations) =>
        val spk = creditingTx.outputs(idx).scriptPubKey
        val outpointsWithKeys = Map(TransactionOutPoint(creditingTx.txId,UInt32(idx)) -> Seq(key))
        val builder = TxBuilder(destinations, Seq(creditingTx),outpointsWithKeys)
        val result = builder.get.sign({_ => true})
        result match {
          case Left(tx) =>
            val txSigComponent = TxSigComponent(tx,UInt32.zero,spk,Policy.standardFlags)
            val program = ScriptProgram(txSigComponent)
            ScriptInterpreter.run(program) == ScriptOk
          case Right(err) =>
            logger.info("error with p2pkh txoutputgen: " + err)
            false
        }
    }
  }
}
