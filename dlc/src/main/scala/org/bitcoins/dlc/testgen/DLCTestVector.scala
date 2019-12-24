package org.bitcoins.dlc.testgen

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{BlockStamp, BlockStampWithFuture}
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitnessV0,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  SegwitV0NativeUTXOSpendingInfo
}
import org.bitcoins.dlc.BinaryOutcomeDLCWithSelf
import play.api.libs.json.{
  JsArray,
  JsResult,
  JsString,
  JsSuccess,
  JsValue,
  Json,
  Reads
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

case class DLCTestVector(
    localPayouts: Map[String, CurrencyUnit],
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
    feeRate: SatoshisPerByte,
    fundingTx: Transaction,
    localWinCet: Transaction,
    localLoseCet: Transaction,
    remoteWinCet: Transaction,
    remoteLoseCet: Transaction,
    refundTx: Transaction,
    localClosingTx: Transaction,
    remoteClosingTx: Transaction
) {
  require(localPayouts.keySet.contains(realOutcome), "Outcome must be possible")
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

  implicit private val ec: ExecutionContext = ExecutionContext.global

  val possibleOutcomes: Vector[String] = localPayouts.keySet.toVector

  lazy val dlc: BinaryOutcomeDLCWithSelf = BinaryOutcomeDLCWithSelf(
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

  val outcomeHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip

  val oracleSig: SchnorrDigitalSignature =
    Schnorr.signWithNonce(outcomeHash.bytes, oracleKey, oracleKValue)

  def regenerate: Future[DLCTestVector] = {
    DLCTestVector.fromInputs(
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

  /** Tests that regenerating from inputs yields same outputs */
  def test(): Future[Boolean] = {
    regenerate.map(_ == this)
  }

  def toJson: JsValue = {
    val inputs = Vector(
      localPayouts,
      realOutcome,
      oracleKey.hex,
      oracleKValue.hex,
      localExtPrivKey.toString,
      localInput.satoshis.toLong,
      localFundingUtxos,
      localChangeSPK.hex,
      remoteExtPrivKey.toString,
      remoteInput.satoshis.toLong,
      remoteFundingUtxos,
      remoteChangeSPK.hex,
      timeout.toUInt32.toLong,
      feeRate.toLong
    )

    val localCets = Vector(localWinCet.hex, localLoseCet.hex)
    val remoteCets = Vector(remoteWinCet.hex, remoteLoseCet.hex)

    val jsOutputs = Vector(
      JsString(fundingTx.hex),
      Json.toJson(localCets),
      Json.toJson(remoteCets),
      JsString(refundTx.hex),
      JsString(localClosingTx.hex),
      JsString(remoteClosingTx.hex)
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

object DLCTestVector {

  def fromJson(json: JsValue): JsResult[DLCTestVector] = {
    import org.bitcoins.rpc.serializers.JsonSerializers._

    def vectorReads[T](fromJson: JsValue => JsResult[T]): Reads[Vector[T]] = {
      Reads[Vector[T]] { json =>
        for {
          arr <- json.validate[JsArray]
          vec <- arr.value
            .foldLeft[JsResult[Vector[T]]](JsSuccess(Vector.empty)) {
              case (jsResultAccum, json) =>
                jsResultAccum.flatMap { accum =>
                  fromJson(json).map { t =>
                    accum :+ t
                  }
                }
            }
        } yield vec
      }
    }

    case class SerializedSegwitSpendingInfo(
        outPoint: TransactionOutPoint,
        output: TransactionOutput,
        signers: Vector[ECPrivateKey],
        hashType: HashType,
        scriptWitness: ScriptWitnessV0) {
      def toSpendingInfo: SegwitV0NativeUTXOSpendingInfo = {
        SegwitV0NativeUTXOSpendingInfo(
          outPoint = outPoint,
          amount = output.value,
          scriptPubKey = output.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0],
          signers = signers,
          hashType = hashType,
          scriptWitness = scriptWitness,
          conditionalPath = ConditionalPath.NoConditionsLeft
        )
      }
    }

    def serializedSegwitSpendingInfoFromJson(
        json: JsValue): JsResult[SerializedSegwitSpendingInfo] = {
      for {
        arr <- json.validate[JsArray]
        seq = arr.value
        outPointStr <- seq.head.validate[String]
        outPoint = TransactionOutPoint.fromHex(outPointStr)
        outputStr <- seq(1).validate[String]
        output = TransactionOutput.fromHex(outputStr)
        signers <- seq(2).validate[Vector[ECPrivateKey]](
          vectorReads(_.validate[String].map(ECPrivateKey.fromHex)))
        hashTypeStr <- seq(3).validate[String]
        hashType = HashType.fromHex(hashTypeStr)
        scriptWitnessStr <- seq(4).validate[String]
        scriptWitness = RawScriptWitnessParser
          .read(scriptWitnessStr)
          .asInstanceOf[ScriptWitnessV0]
      } yield {
        SerializedSegwitSpendingInfo(outPoint,
                                     output,
                                     signers,
                                     hashType,
                                     scriptWitness)
      }
    }

    for {
      arr <- json.validate[JsArray]
      jsInputsArr <- arr.value.head.validate[JsArray]
      jsInputs = jsInputsArr.value
      jsOuptutsArr <- arr.value.last.validate[JsArray]
      jsOutputs = jsOuptutsArr.value
      localPayoutsLong <- jsInputs.head.validate[Map[String, Long]]
      localPayouts = localPayoutsLong.map {
        case (outcome, sats) => (outcome, Satoshis(sats))
      }
      realOutcome <- jsInputs(1).validate[String]
      oracleKeyStr <- jsInputs(2).validate[String]
      oracleKey = ECPrivateKey.fromHex(oracleKeyStr)
      oracleKValueStr <- jsInputs(3).validate[String]
      oracleKValue = SchnorrNonce.fromBytes(
        BitcoinSUtil.decodeHex(oracleKValueStr))
      localExtPrivKey <- jsInputs(4).validate[ExtPrivateKey]
      localInputLong <- jsInputs(5).validate[Long]
      localInput = Satoshis(localInputLong)
      localFundingUtxosSerialized <- jsInputs(6)
        .validate[Vector[SerializedSegwitSpendingInfo]](
          vectorReads(serializedSegwitSpendingInfoFromJson))
      localFundingUtxos = localFundingUtxosSerialized.map(_.toSpendingInfo)
      localChangeSPKStr <- jsInputs(7).validate[String]
      localChangeSPK = ScriptPubKey
        .fromHex(localChangeSPKStr)
        .asInstanceOf[WitnessScriptPubKeyV0]
      remoteExtPrivKey <- jsInputs(8).validate[ExtPrivateKey]
      remoteInputLong <- jsInputs(9).validate[Long]
      remoteInput = Satoshis(remoteInputLong)
      remoteFundingUtxosSerialized <- jsInputs(10)
        .validate[Vector[SerializedSegwitSpendingInfo]](
          vectorReads(serializedSegwitSpendingInfoFromJson))
      remoteFundingUtxos = remoteFundingUtxosSerialized.map(_.toSpendingInfo)
      remoteChangeSPKStr <- jsInputs(11).validate[String]
      remoteChangeSPK = ScriptPubKey
        .fromHex(remoteChangeSPKStr)
        .asInstanceOf[WitnessScriptPubKeyV0]
      timeoutUInt32 <- jsInputs(12).validate[UInt32]
      timeout = BlockStamp(timeoutUInt32.toInt)
      feeRateLong <- jsInputs(13).validate[Long]
      feeRate = SatoshisPerByte(Satoshis(feeRateLong))
      fundingTx <- jsOutputs.head.validate[Transaction]
      localCets <- jsOutputs(1).validate[JsArray]
      localCetWin <- localCets.value.head.validate[Transaction]
      localCetLose <- localCets.value.last.validate[Transaction]
      remoteCets <- jsOutputs(2).validate[JsArray]
      remoteCetWin <- remoteCets.value.head.validate[Transaction]
      remoteCetLose <- remoteCets.value.last.validate[Transaction]
      refundTx <- jsOutputs(3).validate[Transaction]
      localClosingTx <- jsOutputs(4).validate[Transaction]
      remoteClosingTx <- jsOutputs(5).validate[Transaction]
    } yield {
      DLCTestVector(
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
        feeRate = feeRate,
        fundingTx = fundingTx,
        localWinCet = localCetWin,
        localLoseCet = localCetLose,
        remoteWinCet = remoteCetWin,
        remoteLoseCet = remoteCetLose,
        refundTx = refundTx,
        localClosingTx = localClosingTx,
        remoteClosingTx = remoteClosingTx
      )
    }
  }

  def fromInputs(
      localPayouts: Map[String, CurrencyUnit],
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
      feeRate: SatoshisPerByte)(
      implicit ec: ExecutionContext): Future[DLCTestVector] = {
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
      DLCTestVector(
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
        feeRate = feeRate,
        fundingTx = setup.fundingTx,
        localWinCet = setup.cetWinLocal,
        localLoseCet = setup.cetLoseLocal,
        remoteWinCet = setup.cetWinRemote,
        remoteLoseCet = setup.cetLoseRemote,
        refundTx = setup.refundTx,
        localClosingTx = outcome.localClosingTx,
        remoteClosingTx = outcome.remoteClosingTx
      )
    }
  }
}
