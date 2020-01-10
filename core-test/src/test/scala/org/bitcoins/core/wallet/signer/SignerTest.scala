package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptSignature,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  P2WPKHV0SpendingInfo,
  P2WSHV0SpendingInfoFull,
  UTXOSpendingInfoSingle,
  UnassignedSegwitNativeUTXOSpendingInfo
}
import org.bitcoins.testkit.core.gen.{
  ChainParamsGenerator,
  CreditingTxGen,
  GenUtil,
  ScriptGenerators,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.{ExecutionContext, Future}

class SignerTest extends BitcoinSAsyncTest {

  implicit val ec: ExecutionContext = ExecutionContext.global

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "Signer"

  it should "fail to sign a UnassignedSegwit UTXO" in {
    val p2wpkh = GenUtil.sample(CreditingTxGen.p2wpkhOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    val spendingInfo = UnassignedSegwitNativeUTXOSpendingInfo(
      p2wpkh.outPoint,
      p2wpkh.amount,
      p2wpkh.scriptPubKey.asInstanceOf[WitnessScriptPubKey],
      p2wpkh.signers,
      p2wpkh.hashType,
      p2wpkh.scriptWitnessOpt.get,
      p2wpkh.conditionalPath
    )
    assertThrows[UnsupportedOperationException](
      BitcoinSigner.sign(spendingInfo, tx, isDummySignature = false))
  }

  it should "fail to sign a P2SH UTXO" in {
    val p2sh = GenUtil.sample(CreditingTxGen.p2shOutput)
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    assertThrows[IllegalArgumentException](
      BitcoinSigner.sign(p2sh, tx, isDummySignature = false))
  }

  it should "fail if there are inconsistent P2WPKH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wpkh = GenUtil
      .sample(CreditingTxGen.p2wpkhOutput)
      .asInstanceOf[P2WPKHV0SpendingInfo]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    recoverToSucceededIf[IllegalArgumentException] {
      P2WPKHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wpkh)
    }
  }

  it should "fail if there are inconsistent P2WSH spending infos" in {
    val dumbSpendingInfo = GenUtil.sample(CreditingTxGen.output)
    val p2wsh = GenUtil
      .sample(CreditingTxGen.p2wshOutput)
      .asInstanceOf[P2WSHV0SpendingInfoFull]
    val tx = GenUtil.sample(TransactionGenerators.baseTransaction)
    recoverToSucceededIf[IllegalArgumentException] {
      P2WSHSigner.sign(dumbSpendingInfo, tx, isDummySignature = false, p2wsh)
    }
  }

  private val outputGen = CreditingTxGen.outputs
    .flatMap { creditingTxsInfo =>
      val creditingOutputs = creditingTxsInfo.map(c => c.output)
      val creditingOutputsAmt = creditingOutputs.map(_.value)
      val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)

      TransactionGenerators.smallOutputs(totalAmount).map { destinations =>
        (creditingTxsInfo, destinations)
      }
    }
    .suchThat(_._1.nonEmpty)

  it must "sign a mix of spks in a tx and then verify that single signing agrees" in {
    forAllAsync(outputGen,
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfos, destinations), changeSPK, network) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))

        for {
          builder <- BitcoinTxBuilder(destinations,
                                      creditingTxsInfos,
                                      fee,
                                      changeSPK._1,
                                      network)
          unsignedTx <- builder.unsignedTx
          signedTx <- builder.sign

          singleSigs: Vector[Vector[ECDigitalSignature]] <- {
            val singleInfosVec: Vector[Vector[UTXOSpendingInfoSingle]] =
              creditingTxsInfos.toVector.map(_.toSingles)
            val sigVecFs = singleInfosVec.map { singleInfos =>
              val sigFs = singleInfos.map { singleInfo =>
                val keyAndSigF =
                  BitcoinSignerSingle.signSingle(singleInfo,
                                                 unsignedTx,
                                                 isDummySignature = false)

                keyAndSigF.map(_._2)
              }

              Future.sequence(sigFs)
            }

            Future.sequence(sigVecFs)
          }
        } yield {
          signedTx.inputs.zipWithIndex.foreach {
            case (input, inputIndex) =>
              val infoAndIndexOpt = creditingTxsInfos.zipWithIndex
                .find(_._1.outPoint == input.previousOutput)
              assert(infoAndIndexOpt.isDefined)
              val (info, index) = infoAndIndexOpt.get
              val sigs = singleSigs(index)

              val expectedSigs = if (info.scriptWitnessOpt.isEmpty) {
                input.scriptSignature.signatures
              } else {
                signedTx
                  .asInstanceOf[WitnessTransaction]
                  .witness
                  .witnesses(inputIndex) match {
                  case p2wpkh: P2WPKHWitnessV0 => Vector(p2wpkh.signature)
                  case p2wsh: P2WSHWitnessV0   => p2wsh.signatures
                  case EmptyScriptWitness      => Vector.empty
                }
              }

              assert(sigs.length == expectedSigs.length)
              assert(sigs.forall(expectedSigs.contains))
          }

          succeed
        }
    }
  }
}
