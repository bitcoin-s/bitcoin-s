package org.bitcoins.dlc.testgen

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.{
  Bech32Address,
  BlockStamp,
  BlockStampWithFuture,
  NetworkElement
}
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
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil, Factory}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  SegwitV0NativeUTXOSpendingInfoFull
}
import org.bitcoins.dlc.BinaryOutcomeDLCWithSelf
import play.api.libs.json.{
  JsNumber,
  JsObject,
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
    localFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfoFull],
    localChangeSPK: WitnessScriptPubKeyV0,
    remoteExtPrivKey: ExtPrivateKey,
    remoteInput: CurrencyUnit,
    remoteFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfoFull],
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
      localFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfoFull],
      localChangeSPK: WitnessScriptPubKeyV0,
      remoteExtPrivKey: ExtPrivateKey,
      remoteInput: CurrencyUnit,
      remoteFundingUtxos: Vector[SegwitV0NativeUTXOSpendingInfoFull],
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
    val outcomeHash =
      CryptoUtil.sha256(ByteVector(testVector.realOutcome.getBytes)).flip
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes,
                            testVector.oracleKey,
                            testVector.oracleKValue)

    val inputs = SerializedDLCInputs(
      localPayouts = testVector.localPayouts,
      realOutcome = testVector.realOutcome,
      oracleKey = testVector.oracleKey,
      oracleSig = oracleSig,
      oracleKValue = testVector.oracleKValue,
      oracleRValue = testVector.oracleKValue.publicKey,
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

  implicit val ecPublicKeyWrites: Writes[ECPublicKey] = hexWrites[ECPublicKey]
  implicit val schnorrDigitalSignatureWrites: Writes[SchnorrDigitalSignature] =
    hexWrites[SchnorrDigitalSignature]
  implicit val uInt32Writes: Writes[UInt32] = Writes[UInt32] { uint32 =>
    JsNumber(uint32.toLong)
  }
  implicit val doubleSha256DigestBEWrites: Writes[DoubleSha256DigestBE] =
    hexWrites[DoubleSha256DigestBE]
  implicit val transactionOutPointWrites: Writes[
    SerializedTransactionOutPoint] =
    Json.writes[SerializedTransactionOutPoint]
  implicit val serializedTransactionOutputWrites: Writes[
    SerializedTransactionOutput] =
    Writes[SerializedTransactionOutput](_.toJson)
  implicit val privKeyWrites: Writes[ECPrivateKey] = hexWrites[ECPrivateKey]
  implicit val hashTypeWrites: Writes[HashType] = Writes[HashType] { hashType =>
    JsNumber(hashType.num.toInt)
  }
  implicit val scriptWitnessWrites: Writes[ScriptWitnessV0] =
    hexWrites[ScriptWitnessV0]
  implicit val schnorrNonceWrites: Writes[SchnorrNonce] =
    hexWrites[SchnorrNonce]
  implicit val extPrivKeyWrites: Writes[ExtPrivateKey] = Writes[ExtPrivateKey] {
    extPrivKey =>
      JsString(extPrivKey.toString)
  }
  implicit val currencyUnitWrites: Writes[CurrencyUnit] = Writes[CurrencyUnit] {
    currencyUnit =>
      JsNumber(currencyUnit.satoshis.toLong)
  }
  implicit val scriptPubKeyWrites: Writes[WitnessScriptPubKeyV0] =
    Writes[WitnessScriptPubKeyV0] { wspk =>
      JsString(Bech32Address(wspk, RegTest).value)
    }
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

  implicit val ecPublicKeyReads: Reads[ECPublicKey] = hexReads(ECPublicKey)
  implicit val schnorrDigitalSignatureReads: Reads[SchnorrDigitalSignature] =
    hexReads(SchnorrDigitalSignature)
  implicit val uInt32Reads: Reads[UInt32] = Reads[UInt32] { json =>
    json.validate[Long].map(UInt32.apply)
  }
  implicit val doubleSha256DigestBEReads: Reads[DoubleSha256DigestBE] =
    hexReads(DoubleSha256DigestBE)
  implicit val serializedTransactionOutPointReads: Reads[
    SerializedTransactionOutPoint] =
    Json.reads[SerializedTransactionOutPoint]
  implicit val serializedTransactionOutputReads: Reads[
    SerializedTransactionOutput] =
    Reads[SerializedTransactionOutput](SerializedTransactionOutput.fromJson)
  implicit val extPrivKeyReads: Reads[ExtPrivateKey] = Reads[ExtPrivateKey] {
    json =>
      json
        .validate[String]
        .map(ExtKey.fromString)
        .map(_.get.asInstanceOf[ExtPrivateKey])
  }
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
  implicit val scriptPubKeyReads: Reads[WitnessScriptPubKeyV0] =
    Reads[WitnessScriptPubKeyV0] { json =>
      json
        .validate[String]
        .map(Bech32Address.fromString)
        .map(_.get.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0])
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
    oracleSig: SchnorrDigitalSignature,
    oracleKValue: SchnorrNonce,
    oracleRValue: ECPublicKey,
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

  private val outcomeHash =
    CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip
  private val expectedOracleSig =
    Schnorr.signWithNonce(outcomeHash.bytes, oracleKey, oracleKValue)
  require(oracleSig == expectedOracleSig,
          "Oracle Signature is inconsistent with keys and outcome")
  require(oracleRValue == oracleKValue.publicKey,
          "Oracle R value is inconsistent with k value")
}

case class SerializedSegwitSpendingInfo(
    outPoint: SerializedTransactionOutPoint,
    output: SerializedTransactionOutput,
    keys: Vector[ECPrivateKey],
    hashType: HashType,
    scriptWitness: ScriptWitnessV0) {

  def toSpendingInfo: SegwitV0NativeUTXOSpendingInfoFull = {
    SegwitV0NativeUTXOSpendingInfoFull(
      outPoint = outPoint.toOutPoint,
      amount = output.value,
      scriptPubKey = output.spk.asInstanceOf[WitnessScriptPubKeyV0],
      signers = keys,
      hashType = hashType,
      scriptWitness = scriptWitness,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
  }
}

object SerializedSegwitSpendingInfo {

  def fromSpendingInfo(
      spendingInfo: SegwitV0NativeUTXOSpendingInfoFull): SerializedSegwitSpendingInfo = {
    SerializedSegwitSpendingInfo(
      outPoint =
        SerializedTransactionOutPoint.fromOutPoint(spendingInfo.outPoint),
      output = SerializedTransactionOutput.fromOutput(spendingInfo.output),
      keys = spendingInfo.signers.map(_.asInstanceOf[ECPrivateKey]),
      hashType = spendingInfo.hashType,
      scriptWitness = spendingInfo.scriptWitness
    )
  }
}

case class SerializedTransactionOutPoint(
    txid: DoubleSha256DigestBE,
    vout: UInt32) {
  def toOutPoint: TransactionOutPoint = TransactionOutPoint(txid, vout)
}

object SerializedTransactionOutPoint {

  def fromOutPoint(
      outPoint: TransactionOutPoint): SerializedTransactionOutPoint = {
    SerializedTransactionOutPoint(outPoint.txIdBE, outPoint.vout)
  }
}

case class SerializedTransactionOutput(value: CurrencyUnit, spk: ScriptPubKey) {
  def toOutput: TransactionOutput = TransactionOutput(value, spk)

  def toJson: JsObject =
    JsObject(
      Map("value" -> JsNumber(value.satoshis.toLong),
          "spk" -> JsString(spk.hex)))
}

object SerializedTransactionOutput {

  def fromOutput(output: TransactionOutput): SerializedTransactionOutput = {
    SerializedTransactionOutput(output.value, output.scriptPubKey)
  }

  def fromJson(json: JsValue): JsResult[SerializedTransactionOutput] = {
    json.validate[Map[String, JsValue]].flatMap { outputMap =>
      val valueResult = outputMap("value").validate[Long].map(Satoshis.apply)
      val spkResult =
        outputMap("spk").validate[String].map(ScriptPubKey.fromHex)

      for {
        value <- valueResult
        spk <- spkResult
      } yield SerializedTransactionOutput(value, spk)
    }
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
