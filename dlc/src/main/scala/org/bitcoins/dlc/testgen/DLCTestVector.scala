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
import org.bitcoins.core.number.Int32
import org.bitcoins.core.protocol.{
  BlockStamp,
  BlockStampWithFuture,
  NetworkElement
}
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitnessV0,
  WitnessScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{
  BitcoinSUtil,
  BitcoinScriptUtil,
  CryptoUtil,
  Factory
}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  SegwitV0NativeUTXOSpendingInfo
}
import org.bitcoins.dlc.BinaryOutcomeDLCWithSelf
import play.api.libs.json.{
  JsNumber,
  JsResult,
  JsString,
  JsValue,
  Json,
  Reads,
  Writes
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
    SerializedDLCTestVector.fromDLCTestVector(this).toJson
  }
}

object DLCTestVector {

  def fromJson(json: JsValue): JsResult[DLCTestVector] = {
    json
      .validate[SerializedDLCTestVector](
        SerializedDLCTestVectorSerializers.serializedDLCTestVectorReads)
      .map(_.dlcTestVector)
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

case class SerializedDLCTestVector(
    inputs: SerializedDLCInputs,
    outputs: SerializedDLCOutputs) {
  lazy val dlcTestVector: DLCTestVector = {
    DLCTestVector(
      localPayouts = inputs.localPayouts,
      realOutcome = inputs.realOutcome,
      oracleKey = inputs.oracleKey,
      oracleKValue = inputs.oracleKValue,
      localExtPrivKey = inputs.localExtPrivKey,
      localInput = inputs.localInput,
      localFundingUtxos = inputs.localFundingUtxos.map(_.toSpendingInfo),
      localChangeSPK = inputs.localChangeSPK,
      remoteExtPrivKey = inputs.remoteExtPrivKey,
      remoteInput = inputs.remoteInput,
      remoteFundingUtxos = inputs.remoteFundingUtxos.map(_.toSpendingInfo),
      remoteChangeSPK = inputs.remoteChangeSPK,
      timeout = inputs.timeout,
      feeRate = inputs.feeRate,
      fundingTx = outputs.fundingTx,
      localWinCet = outputs.localCets.head,
      localLoseCet = outputs.localCets.last,
      remoteWinCet = outputs.remoteCets.head,
      remoteLoseCet = outputs.remoteCets.last,
      refundTx = outputs.refundTx,
      localClosingTx = outputs.localClosingTx,
      remoteClosingTx = outputs.remoteClosingTx
    )
  }

  def toJson: JsValue = {
    Json.toJson(this)(
      SerializedDLCTestVectorSerializers.serializedDLCTestVectorWrites)
  }
}

object SerializedDLCTestVector {

  def fromDLCTestVector(testVector: DLCTestVector): SerializedDLCTestVector = {
    val inputs = SerializedDLCInputs(
      localPayouts = testVector.localPayouts,
      realOutcome = testVector.realOutcome,
      oracleKey = testVector.oracleKey,
      oracleKValue = testVector.oracleKValue,
      localExtPrivKey = testVector.localExtPrivKey,
      localInput = testVector.localInput,
      localFundingUtxos = testVector.localFundingUtxos.map(
        SerializedSegwitSpendingInfo.fromSpendingInfo),
      localChangeSPK = testVector.localChangeSPK,
      remoteExtPrivKey = testVector.remoteExtPrivKey,
      remoteInput = testVector.remoteInput,
      remoteFundingUtxos = testVector.remoteFundingUtxos.map(
        SerializedSegwitSpendingInfo.fromSpendingInfo),
      remoteChangeSPK = testVector.remoteChangeSPK,
      timeout = testVector.timeout,
      feeRate = testVector.feeRate
    )
    val outputs = SerializedDLCOutputs(
      fundingTx = testVector.fundingTx,
      localCets = Vector(testVector.localWinCet, testVector.localLoseCet),
      remoteCets = Vector(testVector.remoteWinCet, testVector.remoteLoseCet),
      refundTx = testVector.refundTx,
      localClosingTx = testVector.localClosingTx,
      remoteClosingTx = testVector.remoteClosingTx
    )

    SerializedDLCTestVector(inputs, outputs)
  }
}

object SerializedDLCTestVectorSerializers {

  def hexWrites[T <: NetworkElement]: Writes[T] = Writes[T] { element =>
    JsString(element.hex)
  }

  implicit val transactionOutPointWrites: Writes[TransactionOutPoint] =
    hexWrites[TransactionOutPoint]
  implicit val transactionOutputWrites: Writes[TransactionOutput] =
    hexWrites[TransactionOutput]
  implicit val privKeyWrites: Writes[ECPrivateKey] = hexWrites[ECPrivateKey]
  implicit val hashTypeWrites: Writes[HashType] = Writes[HashType] { hashType =>
    JsNumber(hashType.num.toInt)
  }
  implicit val scriptWitnessWrites: Writes[ScriptWitnessV0] =
    hexWrites[ScriptWitnessV0]
  implicit val schnorrNonceWrites: Writes[SchnorrNonce] =
    hexWrites[SchnorrNonce]
  implicit val extPrivKeyWrites: Writes[ExtPrivateKey] =
    hexWrites[ExtPrivateKey]
  implicit val currencyUnitWrites: Writes[CurrencyUnit] = Writes[CurrencyUnit] {
    currencyUnit =>
      JsNumber(currencyUnit.satoshis.toLong)
  }
  implicit val scriptPubKeyWrites: Writes[ScriptPubKey] =
    hexWrites[ScriptPubKey]
  implicit val blockStampWithFutureWrites: Writes[BlockStampWithFuture] =
    Writes[BlockStampWithFuture] { blockStamp =>
      JsNumber(blockStamp.toUInt32.toLong)
    }
  implicit val satoshisPerByteWrites: Writes[SatoshisPerByte] =
    Writes[SatoshisPerByte] { fee =>
      JsNumber(fee.toLong)
    }
  implicit val transactionWrites: Writes[Transaction] = hexWrites[Transaction]

  implicit val serializedSegwitSpendingInfoWrites: Writes[
    SerializedSegwitSpendingInfo] =
    Json.writes[SerializedSegwitSpendingInfo]
  implicit val serializedDLCInputsWrites: Writes[SerializedDLCInputs] =
    Json.writes[SerializedDLCInputs]
  implicit val serializedDLCOutputsWrites: Writes[SerializedDLCOutputs] =
    Json.writes[SerializedDLCOutputs]
  implicit val serializedDLCTestVectorWrites: Writes[SerializedDLCTestVector] =
    Json.writes[SerializedDLCTestVector]

  def hexReads[T <: NetworkElement](factory: Factory[T]): Reads[T] = Reads[T] {
    json =>
      json.validate[String].map(factory.fromHex)
  }

  implicit val transactionOutPointReads: Reads[TransactionOutPoint] = hexReads(
    TransactionOutPoint)
  implicit val transactionOutputReads: Reads[TransactionOutput] = hexReads(
    TransactionOutput)
  implicit val extPrivKeyReads: Reads[ExtPrivateKey] = hexReads(ExtPrivateKey)
  implicit val blockStampWithFutureReads: Reads[BlockStampWithFuture] =
    Reads[BlockStampWithFuture] { json =>
      json.validate[Int].map(BlockStamp.apply)
    }
  implicit val hashTypeReads: Reads[HashType] = Reads[HashType] { json =>
    json.validate[Int].map(Int32.apply).map(HashType.fromNumber)
  }
  implicit val scriptWitnessReads: Reads[ScriptWitnessV0] =
    Reads[ScriptWitnessV0] { json =>
      json
        .validate[String]
        .map(BitcoinSUtil.decodeHex)
        .map(RawScriptWitnessParser.read)
        .map(_.asInstanceOf[ScriptWitnessV0])
    }
  implicit val currencyUnitReads: Reads[CurrencyUnit] = Reads[CurrencyUnit] {
    json =>
      json.validate[Long].map(Satoshis.apply)
  }
  implicit val privKeyReads: Reads[ECPrivateKey] = hexReads(ECPrivateKey)
  implicit val schnorrNonceReads: Reads[SchnorrNonce] = Reads[SchnorrNonce] {
    json =>
      json
        .validate[String]
        .map(BitcoinSUtil.decodeHex)
        .map(SchnorrNonce.fromBytes)
  }
  private val witnessScriptPubKeyV0FromAsm: Vector[ScriptToken] => WitnessScriptPubKeyV0 = {
    asm =>
      WitnessScriptPubKey.fromAsm(asm).get.asInstanceOf[WitnessScriptPubKeyV0]
  }
  implicit val scriptPubKeyReads: Reads[WitnessScriptPubKeyV0] =
    Reads[WitnessScriptPubKeyV0] { json =>
      json
        .validate[String]
        .map(BitcoinSUtil.decodeHex)
        .map(BitcoinScriptUtil.parseScript(_, witnessScriptPubKeyV0FromAsm))
    }
  implicit val satoshisPerByteReads: Reads[SatoshisPerByte] =
    Reads[SatoshisPerByte] { json =>
      json.validate[Long].map(Satoshis.apply).map(SatoshisPerByte.apply)
    }
  implicit val transactionReads: Reads[Transaction] = hexReads(Transaction)

  implicit val serializedSegwitSpendingInfoReads: Reads[
    SerializedSegwitSpendingInfo] =
    Json.reads[SerializedSegwitSpendingInfo]
  implicit val serializedDLCInputsReads: Reads[SerializedDLCInputs] =
    Json.reads[SerializedDLCInputs]
  implicit val serializedDLCOutputsReads: Reads[SerializedDLCOutputs] =
    Json.reads[SerializedDLCOutputs]
  implicit val serializedDLCTestVectorReads: Reads[SerializedDLCTestVector] =
    Json.reads[SerializedDLCTestVector]
}

case class SerializedDLCInputs(
    localPayouts: Map[String, CurrencyUnit],
    realOutcome: String,
    oracleKey: ECPrivateKey,
    oracleKValue: SchnorrNonce,
    localExtPrivKey: ExtPrivateKey,
    localInput: CurrencyUnit,
    localFundingUtxos: Vector[SerializedSegwitSpendingInfo],
    localChangeSPK: WitnessScriptPubKeyV0,
    remoteExtPrivKey: ExtPrivateKey,
    remoteInput: CurrencyUnit,
    remoteFundingUtxos: Vector[SerializedSegwitSpendingInfo],
    remoteChangeSPK: WitnessScriptPubKeyV0,
    timeout: BlockStampWithFuture,
    feeRate: SatoshisPerByte) {
  require(localPayouts.keySet.contains(realOutcome), "Outcome must be possible")
  require(localPayouts.size == 2, "Currently only Binary DLCs are supported")
  require(localPayouts.values.forall(_ <= localInput + remoteInput),
          "Payouts must be less than total input")
  require(localFundingUtxos
            .map(_.output.value.satoshis.toLong)
            .sum > localInput.satoshis.toLong,
          "Local does not have enough to fund")
  require(remoteFundingUtxos
            .map(_.output.value.satoshis.toLong)
            .sum > remoteInput.satoshis.toLong,
          "Remote does not have enough to fund")
}

case class SerializedSegwitSpendingInfo(
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    keys: Vector[ECPrivateKey],
    hashType: HashType,
    scriptWitness: ScriptWitnessV0) {

  def toSpendingInfo: SegwitV0NativeUTXOSpendingInfo = {
    SegwitV0NativeUTXOSpendingInfo(
      outPoint = outPoint,
      amount = output.value,
      scriptPubKey = output.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0],
      signers = keys,
      hashType = hashType,
      scriptWitness = scriptWitness,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
  }
}

object SerializedSegwitSpendingInfo {

  def fromSpendingInfo(
      spendingInfo: SegwitV0NativeUTXOSpendingInfo): SerializedSegwitSpendingInfo = {
    SerializedSegwitSpendingInfo(
      outPoint = spendingInfo.outPoint,
      output = spendingInfo.output,
      keys = spendingInfo.signers.toVector.map(_.asInstanceOf[ECPrivateKey]),
      hashType = spendingInfo.hashType,
      scriptWitness = spendingInfo.scriptWitness
    )
  }
}

case class SerializedDLCOutputs(
    fundingTx: Transaction,
    localCets: Vector[Transaction],
    remoteCets: Vector[Transaction],
    refundTx: Transaction,
    localClosingTx: Transaction,
    remoteClosingTx: Transaction) {
  require(localCets.length == 2,
          s"There must be two local CETs, got $localCets")
  require(remoteCets.length == 2,
          s"There must be two remote CETs, got $remoteCets")
}
