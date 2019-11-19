package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.testkit.core.gen.{
  ChainParamsGenerator,
  CreditingTxGen,
  ScriptGenerators,
  TransactionGenerators
}
import org.scalacheck.{Prop, Properties}

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

class BitcoinTxBuilderSpec extends Properties("TxBuilderSpec") {
  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants
  val timeout = 10.seconds

  property("sign a mix of spks in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.outputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c.output)
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAllNoShrink(TransactionGenerators.smallOutputs(totalAmount),
                            ScriptGenerators.scriptPubKey,
                            ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys =
              buildCreditingTxInfo(creditingTxsInfo.toList)
            val builder = BitcoinTxBuilder(destinations,
                                           outpointsWithKeys,
                                           fee,
                                           changeSPK._1,
                                           network)
            val tx = Await.result(builder.flatMap(_.sign), timeout)
            BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo)
        }
    }
  }

  property("sign a mix of p2sh/p2wsh in a tx and then have it verified") = {
    Prop.forAllNoShrink(CreditingTxGen.nestedOutputs) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c.output)
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAll(TransactionGenerators.smallOutputs(totalAmount),
                    ScriptGenerators.scriptPubKey,
                    ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerByte(Satoshis(Int64(1000)))
            val outpointsWithKeys =
              buildCreditingTxInfo(creditingTxsInfo.toList)
            val builder = BitcoinTxBuilder(destinations,
                                           outpointsWithKeys,
                                           fee,
                                           changeSPK._1,
                                           network)
            val tx = Await.result(builder.flatMap(_.sign), timeout)
            BitcoinScriptUtil.verifyScript(tx, creditingTxsInfo)
        }
    }
  }
  /*
  property("random fuzz test for tx builder") = {
    Prop.forAllNoShrink(CreditingTxGen.randoms) {
      case creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c.output)
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)
        Prop.forAllNoShrink(TransactionGenerators.smallOutputs(totalAmount),
                            ScriptGenerators.scriptPubKey,
                            ChainParamsGenerator.bitcoinNetworkParams) {
          case (destinations: Seq[TransactionOutput], changeSPK, network) =>
            val fee = SatoshisPerVirtualByte(Satoshis(Int64(1000)))
            val outpointsWithKeys =
              buildCreditingTxInfo(creditingTxsInfo.toList)
            val builder = BitcoinTxBuilder(destinations,
                                           outpointsWithKeys,
                                           fee,
                                           changeSPK._1,
                                           network)
            val result = Try(Await.result(builder.flatMap(_.sign), timeout))
            if (result.isFailure) true
            else !verifyScript(result.get, creditingTxsInfo)
        }
    }
  }
   */
  private def buildCreditingTxInfo(
      info: List[BitcoinUTXOSpendingInfo]): BitcoinTxBuilder.UTXOMap = {
    @tailrec
    def loop(rem: List[BitcoinUTXOSpendingInfo],
             accum: BitcoinTxBuilder.UTXOMap): BitcoinTxBuilder.UTXOMap =
      rem match {
        case Nil => accum
        case BitcoinUTXOSpendingInfo(txOutPoint,
                                     txOutput,
                                     signers,
                                     redeemScriptOpt,
                                     scriptWitOpt,
                                     hashType,
                                     conditionalPath) :: t =>
          val o = txOutPoint
          val output = txOutput
          val outPointsSpendingInfo = BitcoinUTXOSpendingInfo(o,
                                                              output,
                                                              signers,
                                                              redeemScriptOpt,
                                                              scriptWitOpt,
                                                              hashType,
                                                              conditionalPath)
          loop(t, accum.updated(o, outPointsSpendingInfo))
      }
    loop(info, Map.empty)
  }
}
