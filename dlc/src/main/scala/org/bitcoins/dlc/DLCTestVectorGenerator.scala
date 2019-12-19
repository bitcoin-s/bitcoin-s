package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrNonce
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BlockStampWithFuture
import org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import play.api.libs.json.{JsString, JsValue, Json}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object DLCTestVectorGenerator {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  def generateTest(
      possibleOutcomes: Vector[String],
      realOutcome: String,
      oracleKey: ECPrivateKey,
      oracleKValue: SchnorrNonce,
      localPayouts: Map[String, CurrencyUnit],
      localExtPrivKey: ExtPrivateKey,
      localInput: CurrencyUnit,
      localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
      localChangeSPK: WitnessScriptPubKeyV0,
      remoteExtPrivKey: ExtPrivateKey,
      remoteInput: CurrencyUnit,
      remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
      remoteChangeSPK: WitnessScriptPubKeyV0,
      timeout: BlockStampWithFuture,
      feeRate: SatoshisPerByte): Future[JsValue] = {
    require(possibleOutcomes.contains(realOutcome), "Outcome must be possible")
    require(possibleOutcomes.length == 2,
            "Currently only Binary DLCs are supported")
    require(localPayouts.keys.toVector == possibleOutcomes,
            "All outcomes must have specified payouts")
    require(localPayouts.values.forall(_ < localInput + remoteInput),
            "Payouts must be less than total input")
    require(localFundingUtxos
              .map(_.amount.satoshis.toLong)
              .sum > localInput.satoshis.toLong,
            "Local does not have enough to fund")
    require(remoteFundingUtxos
              .map(_.amount.satoshis.toLong)
              .sum > remoteInput.satoshis.toLong,
            "Remote does not have enough to fund")

    val dlc = BinaryOutcomeDLCWithSelf(
      outcomeWin = possibleOutcomes.head,
      outcomeLose = possibleOutcomes.last,
      oraclePubKey = oracleKey.publicKey,
      preCommittedR = oracleKValue.publicKey,
      localExtPrivKey = localExtPrivKey,
      remoteExtPrivKey = remoteExtPrivKey,
      localInput = localInput,
      remoteInput = remoteInput,
      localFundingUtxos = localFundingUtxos,
      remoteFundingUtxos = remoteFundingUtxos,
      localWinPayout = localPayouts(possibleOutcomes.head),
      localLosePayout = localPayouts(possibleOutcomes.last),
      timeout = timeout,
      feeRate = feeRate,
      localChangeSPK = localChangeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = RegTest
    )

    val outcomeHash = CryptoUtil.sha256(ByteVector(realOutcome.getBytes))
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oracleKey, oracleKValue)

    for {
      setup <- dlc.setupDLC()
      outcome <- dlc.executeUnilateralDLC(setup,
                                          Future.successful(oracleSig),
                                          local = true)
    } yield {
      val inputs = Vector(
        possibleOutcomes,
        realOutcome,
        oracleKey.hex,
        oracleKValue.hex,
        localPayouts.values.map(_.satoshis.toLong),
        localExtPrivKey.hex,
        localInput.satoshis.toLong,
        localFundingUtxos,
        localChangeSPK.hex,
        remoteExtPrivKey.hex,
        remoteInput.satoshis.toLong,
        remoteFundingUtxos,
        remoteChangeSPK.hex,
        timeout.toUInt32.toLong,
        feeRate.toLong
      )

      val outputs = Vector(
        setup.fundingTx.hex,
        setup.cetWinLocal.hex,
        setup.cetLoseLocal.hex,
        setup.cetWinRemote.hex,
        setup.cetLoseRemote.hex,
        outcome.localClosingTx.hex,
        outcome.remoteClosingTx.hex
      )

      val jsInputs: Vector[JsValue] = inputs.map {
        case vec: Vector[String] => Json.toJson(vec)
        case vec: Vector[Long]   => Json.toJson(vec)
        case str: String         => Json.toJson(str)
        case num: Long           => Json.toJson(num)
        case utxos: Vector[BitcoinUTXOSpendingInfo] =>
          val jsUtxos = utxos.map { utxo =>
            Vector(
              JsString(utxo.outPoint.hex),
              JsString(utxo.output.hex),
              Json.toJson(utxo.signers.map(_.asInstanceOf[ECPrivateKey].hex)),
              JsString(utxo.hashType.byte.toHexString),
              Json.toJson(utxo.redeemScriptOpt.map(_.hex)),
              Json.toJson(utxo.scriptWitnessOpt.map(_.hex))
            )
          }

          Json.toJson(jsUtxos)
      }
      val jsOutputs: Vector[JsValue] = outputs.map(Json.toJson)

      Json.toJson(Vector(jsInputs, jsOutputs))
    }
  }
}
