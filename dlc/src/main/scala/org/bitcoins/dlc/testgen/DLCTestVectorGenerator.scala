package org.bitcoins.dlc.testgen

import java.io.{File, PrintWriter}

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.BlockStampWithFuture
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  P2WPKHV0SpendingInfo,
  SegwitV0NativeUTXOSpendingInfo
}
import org.bitcoins.dlc.BinaryOutcomeDLCWithSelf
import play.api.libs.json.{JsString, JsValue, Json}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object DLCTestVectorGenerator {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private def writeToFile(json: JsValue, outFile: File): Unit = {
    val writer = new PrintWriter(outFile)
    writer.print(json.toString)
    writer.close()
  }

  def generateRandomTestVector(): Future[JsValue] = {
    val localPayouts = Map(
      "WIN" -> (CurrencyUnits.oneBTC * 2 - CurrencyUnits.oneMBTC),
      "LOSE" -> CurrencyUnits.oneMBTC)
    val realOutcome = "WIN"
    val oracleKey = ECPrivateKey.freshPrivateKey
    val oracleKValue = SchnorrNonce.freshNonce

    val localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
    val localInput = CurrencyUnits.oneBTC
    val inputPrivKeyLocal = ECPrivateKey.freshPrivateKey
    val localFundingUtxos = Vector(
      P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        amount = localInput * 2,
        scriptPubKey = P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey),
        signer = inputPrivKeyLocal,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(inputPrivKeyLocal.publicKey)
      )
    )
    val localChangeSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)

    val remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
    val remoteInput = CurrencyUnits.oneBTC
    val inputPrivKeyRemote = ECPrivateKey.freshPrivateKey
    val remoteFundingUtxos = Vector(
      P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        amount = remoteInput * 2,
        scriptPubKey = P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey),
        signer = inputPrivKeyRemote,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(inputPrivKeyRemote.publicKey)
      )
    )
    val remoteChangeSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)

    val timeout = BlockTime(UInt32(System.currentTimeMillis() / 1000))
    val feeRate = SatoshisPerByte(Satoshis.one)

    generateTest(
      localPayouts = localPayouts,
      realOutcome = realOutcome,
      oracleKey = oracleKey,
      oracleKValue = oracleKValue,
      localExtPrivKey = localExtPrivKey,
      localInput = localInput,
      localFundingUtxos = localFundingUtxos,
      localChangeSPK = localChangeSPK,
      remoteExtPrivKey = remoteExtPrivKey,
      remoteInput = remoteInput,
      remoteFundingUtxos = remoteFundingUtxos,
      remoteChangeSPK = remoteChangeSPK,
      timeout = timeout,
      feeRate = feeRate
    )
  }

  def generateTest(localPayouts: Map[String, CurrencyUnit],
                   realOutcome: String,
                   oracleKey: ECPrivateKey,
                   oracleKValue: SchnorrNonce,
                   localExtPrivKey: ExtPrivateKey,
                   localInput: CurrencyUnit,
                   localFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfo],
                   localChangeSPK: WitnessScriptPubKeyV0,
                   remoteExtPrivKey: ExtPrivateKey,
                   remoteInput: CurrencyUnit,
                   remoteFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfo],
                   remoteChangeSPK: WitnessScriptPubKeyV0,
                   timeout: BlockStampWithFuture,
                   feeRate: SatoshisPerByte): Future[JsValue] = {
    require(localPayouts.keySet.contains(realOutcome),
            "Outcome must be possible")
    require(localPayouts.size == 2, "Currently only Binary DLCs are supported")
    require(localPayouts.values.forall(_ <= localInput + remoteInput),
            "Payouts must be less than total input")
    require(localFundingUtxos
              .map(_.amount.satoshis.toLong)
              .sum > localInput.satoshis.toLong,
            "Local does not have enough to fund")
    require(remoteFundingUtxos
              .map(_.amount.satoshis.toLong)
              .sum > remoteInput.satoshis.toLong,
            "Remote does not have enough to fund")

    val possibleOutcomes = localPayouts.keySet.toVector

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

    val outcomeHash = CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oracleKey, oracleKValue)

    for {
      setup <- dlc.setupDLC()
      outcome <- dlc.executeUnilateralDLC(setup,
                                          Future.successful(oracleSig),
                                          local = true)
    } yield {
      val inputs = Vector(
        localPayouts,
        realOutcome,
        oracleKey.hex,
        oracleKValue.hex,
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

      val localCets = Vector(setup.cetWinLocal.hex, setup.cetLoseLocal.hex)
      val remoteCets = Vector(setup.cetWinRemote.hex, setup.cetLoseRemote.hex)

      val jsOutputs = Vector(
        JsString(setup.fundingTx.hex),
        Json.toJson(localCets),
        Json.toJson(remoteCets),
        JsString(setup.refundTx.hex),
        JsString(outcome.localClosingTx.hex),
        JsString(outcome.remoteClosingTx.hex)
      )

      val jsInputs: Vector[JsValue] = inputs.map {
        case map: Map[_, _] =>
          val localPayoutsMap = map.asInstanceOf[Map[String, CurrencyUnit]]
          val underlyingMap = localPayoutsMap.map {
            case (outcome, payout) => (outcome, payout.satoshis.toLong)
          }
          Json.toJson(underlyingMap)
        case vec: Vector[_] =>
          val utxos = vec.asInstanceOf[Vector[SegwitV0NativeUTXOSpendingInfo]]
          val jsUtxos = utxos.map { utxo =>
            Vector(
              JsString(utxo.outPoint.hex),
              JsString(utxo.output.hex),
              Json.toJson(utxo.signers.map(_.asInstanceOf[ECPrivateKey].hex)),
              JsString(utxo.hashType.byte.toHexString),
              JsString(utxo.scriptWitness.hex)
            )
          }

          Json.toJson(jsUtxos)
        case str: String => Json.toJson(str)
        case num: Long   => Json.toJson(num)
      }

      Json.toJson(Vector(jsInputs, jsOutputs))
    }
  }
}
