package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts,
  FundingSignatures
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{ExtKey, ExtPrivateKey}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{
  Bech32Address,
  BlockStamp,
  BlockStampWithFuture
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  P2WPKHV0SpendingInfo,
  SegwitV0NativeUTXOSpendingInfoFull
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.{
  DLCClient,
  UnilateralDLCOutcomeWithClosing,
  UnilateralDLCOutcomeWithDustClosing
}
import play.api.libs.json._
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future, Promise}

// TODO: Add test vectors to dlc_test.json where dust outputs are removed
case class DLCTestVector(
    localPayouts: Map[String, CurrencyUnit],
    realOutcome: String,
    oracleKey: ECPrivateKey,
    oracleKValue: ECPrivateKey,
    localExtPrivKey: ExtPrivateKey,
    localInput: CurrencyUnit,
    localFundingUtxos: Vector[P2WPKHV0SpendingInfo],
    localChangeSPK: WitnessScriptPubKeyV0,
    remoteExtPrivKey: ExtPrivateKey,
    remoteInput: CurrencyUnit,
    remoteFundingUtxos: Vector[P2WPKHV0SpendingInfo],
    remoteToRemoteSweepSPK: WitnessScriptPubKeyV0,
    remoteChangeSPK: WitnessScriptPubKeyV0,
    timeouts: DLCTimeouts,
    feeRate: SatoshisPerByte,
    fundingTx: Transaction,
    localWinCet: Transaction,
    localLoseCet: Transaction,
    remoteWinCet: Transaction,
    remoteLoseCet: Transaction,
    refundTx: Transaction,
    mutualCloseTx: Transaction,
    localClosingTxOpt: Option[Transaction],
    remoteClosingTxOpt: Option[Transaction]
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

  val outcomeHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip

  val oracleSig: SchnorrDigitalSignature =
    oracleKey.schnorrSignWithNonce(outcomeHash.bytes, oracleKValue)

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
      remoteToRemoteSweepSPK = remoteToRemoteSweepSPK,
      remoteChangeSPK = remoteChangeSPK,
      timeouts = timeouts,
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
      oracleKValue: ECPrivateKey,
      localExtPrivKey: ExtPrivateKey,
      localInput: CurrencyUnit,
      localFundingUtxos: Vector[P2WPKHV0SpendingInfo],
      localChangeSPK: WitnessScriptPubKeyV0,
      remoteExtPrivKey: ExtPrivateKey,
      remoteInput: CurrencyUnit,
      remoteFundingUtxos: Vector[P2WPKHV0SpendingInfo],
      remoteToRemoteSweepSPK: WitnessScriptPubKeyV0,
      remoteChangeSPK: WitnessScriptPubKeyV0,
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerByte)(
      implicit ec: ExecutionContext): Future[DLCTestVector] = {
    val possibleOutcomes = localPayouts.keySet.toVector

    val localFundingInputs = localFundingUtxos.map { info =>
      OutputReference(info.outPoint, info.output)
    }
    val remoteFundingInputs = remoteFundingUtxos.map { info =>
      OutputReference(info.outPoint, info.output)
    }

    val offerDLC = DLCClient(
      outcomeWin = possibleOutcomes.head,
      outcomeLose = possibleOutcomes.last,
      oraclePubKey = oracleKey.schnorrPublicKey,
      preCommittedR = oracleKValue.schnorrNonce,
      isInitiator = true,
      extPrivKey = localExtPrivKey,
      nextAddressIndex = 0,
      remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(remoteExtPrivKey,
                                                           nextAddressIndex = 0,
                                                           RegTest),
      input = localInput,
      remoteInput = remoteInput,
      fundingUtxos = localFundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      winPayout = localPayouts(possibleOutcomes.head),
      losePayout = localPayouts(possibleOutcomes.last),
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = localChangeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = RegTest
    )

    val acceptDLC = DLCClient(
      outcomeWin = possibleOutcomes.head,
      outcomeLose = possibleOutcomes.last,
      oraclePubKey = oracleKey.schnorrPublicKey,
      preCommittedR = oracleKValue.schnorrNonce,
      isInitiator = false,
      extPrivKey = remoteExtPrivKey,
      nextAddressIndex = 0,
      remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(localExtPrivKey,
                                                           nextAddressIndex = 0,
                                                           RegTest),
      input = remoteInput,
      remoteInput = localInput,
      fundingUtxos = remoteFundingUtxos,
      remoteFundingInputs = localFundingInputs,
      winPayout = localInput + remoteInput - localPayouts(
        possibleOutcomes.head),
      losePayout = localInput + remoteInput - localPayouts(
        possibleOutcomes.last),
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = remoteChangeSPK,
      remoteChangeSPK = localChangeSPK,
      network = RegTest
    )

    val outcomeHash = CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip
    val oracleSig =
      oracleKey.schnorrSignWithNonce(outcomeHash.bytes, oracleKValue)

    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP = Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
        FutureUtil.unit
    }

    val acceptSetupF = acceptDLC.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)
    val offerSetupF = offerDLC.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx =
                                               acceptSetupF.map(_.fundingTx))

    for {
      acceptSetup <- acceptSetupF
      offerSetup <- offerSetupF
      unilateralOutcome <- offerDLC.executeUnilateralDLC(
        offerSetup,
        Future.successful(oracleSig))
      toRemoteOutcome <- acceptDLC.executeRemoteUnilateralDLC(
        acceptSetup,
        unilateralOutcome.cet,
        remoteToRemoteSweepSPK)

      sigsP = Promise[(SchnorrDigitalSignature, PartialSignature)]()
      _ <- offerDLC.initiateMutualClose(offerSetup, oracleSig, {
        (sig, offerSig) =>
          sigsP.success((sig, offerSig))
          FutureUtil.unit
      }, Future.successful(EmptyTransaction))
      mutualOutcome <- acceptDLC.executeMutualClose(acceptSetup, sigsP.future)
    } yield {
      val localClosingTxOpt = unilateralOutcome match {
        case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
          Some(closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing => None
      }
      val remoteClosingTxOpt = toRemoteOutcome match {
        case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
          Some(closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing => None
      }

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
        remoteToRemoteSweepSPK = remoteToRemoteSweepSPK,
        remoteChangeSPK = remoteChangeSPK,
        timeouts = timeouts,
        feeRate = feeRate,
        fundingTx = offerSetup.fundingTx,
        localWinCet = offerSetup.cets.head._2.tx,
        localLoseCet = offerSetup.cets.last._2.tx,
        remoteWinCet = acceptSetup.cets.head._2.tx,
        remoteLoseCet = acceptSetup.cets.last._2.tx,
        refundTx = offerSetup.refundTx,
        mutualCloseTx = mutualOutcome.closingTx,
        localClosingTxOpt = localClosingTxOpt,
        remoteClosingTxOpt = remoteClosingTxOpt
      )
    }
  }
}

case class SerializedDLCTestVector(
    inputs: SerializedDLCInputs,
    outputs: SerializedDLCOutputs) {
  lazy val dlcTestVector: DLCTestVector = {
    val timeouts = DLCTimeouts(penaltyTimeout = inputs.penaltyTimeout,
                               contractMaturity = inputs.contractMaturity,
                               contractTimeout = inputs.contractTimeout)

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
      remoteToRemoteSweepSPK = inputs.remoteToRemoteSweepSPK,
      remoteChangeSPK = inputs.remoteChangeSPK,
      timeouts = timeouts,
      feeRate = inputs.feeRate,
      fundingTx = outputs.fundingTx,
      localWinCet = outputs.localCets.head,
      localLoseCet = outputs.localCets.last,
      remoteWinCet = outputs.remoteCets.head,
      remoteLoseCet = outputs.remoteCets.last,
      refundTx = outputs.refundTx,
      mutualCloseTx = outputs.mutualCloseTx,
      localClosingTxOpt = outputs.localClosingTx,
      remoteClosingTxOpt = outputs.remoteClosingTx
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
      testVector.oracleKey
        .schnorrSignWithNonce(outcomeHash.bytes, testVector.oracleKValue)

    val inputs = SerializedDLCInputs(
      localPayouts = testVector.localPayouts,
      realOutcome = testVector.realOutcome,
      oracleKey = testVector.oracleKey,
      oracleSig = oracleSig,
      oracleKValue = testVector.oracleKValue,
      oracleRValue = testVector.oracleKValue.schnorrNonce,
      localExtPrivKey = testVector.localExtPrivKey,
      localInput = testVector.localInput,
      localFundingUtxos = testVector.localFundingUtxos.map(
        SerializedSegwitSpendingInfo.fromSpendingInfo),
      localChangeSPK = testVector.localChangeSPK,
      remoteExtPrivKey = testVector.remoteExtPrivKey,
      remoteInput = testVector.remoteInput,
      remoteFundingUtxos = testVector.remoteFundingUtxos.map(
        SerializedSegwitSpendingInfo.fromSpendingInfo),
      remoteToRemoteSweepSPK = testVector.remoteToRemoteSweepSPK,
      remoteChangeSPK = testVector.remoteChangeSPK,
      penaltyTimeout = testVector.timeouts.penaltyTimeout,
      contractMaturity = testVector.timeouts.contractMaturity,
      contractTimeout = testVector.timeouts.contractTimeout,
      feeRate = testVector.feeRate
    )
    val outputs = SerializedDLCOutputs(
      fundingTx = testVector.fundingTx,
      localCets = Vector(testVector.localWinCet, testVector.localLoseCet),
      remoteCets = Vector(testVector.remoteWinCet, testVector.remoteLoseCet),
      refundTx = testVector.refundTx,
      mutualCloseTx = testVector.mutualCloseTx,
      localClosingTx = testVector.localClosingTxOpt,
      remoteClosingTx = testVector.remoteClosingTxOpt
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
      JsString(extPrivKey.toStringSensitive)
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
        .map(BytesUtil.decodeHex)
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
        .map(BytesUtil.decodeHex)
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
    oracleKValue: ECPrivateKey,
    oracleRValue: SchnorrNonce,
    localExtPrivKey: ExtPrivateKey,
    localInput: CurrencyUnit,
    localFundingUtxos: Vector[SerializedSegwitSpendingInfo],
    localChangeSPK: WitnessScriptPubKeyV0,
    remoteExtPrivKey: ExtPrivateKey,
    remoteInput: CurrencyUnit,
    remoteFundingUtxos: Vector[SerializedSegwitSpendingInfo],
    remoteToRemoteSweepSPK: WitnessScriptPubKeyV0,
    remoteChangeSPK: WitnessScriptPubKeyV0,
    penaltyTimeout: UInt32,
    contractMaturity: BlockStampWithFuture,
    contractTimeout: BlockStampWithFuture,
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

  require(oracleRValue == oracleKValue.schnorrNonce,
          "Oracle R value is inconsistent with k value")
  private val outcomeHash =
    CryptoUtil.sha256(ByteVector(realOutcome.getBytes)).flip
  private val expectedOracleSig =
    oracleKey.schnorrSignWithNonce(outcomeHash.bytes, oracleKValue)
  require(oracleSig == expectedOracleSig,
          "Oracle Signature is inconsistent with keys and outcome")
  require(oracleRValue == oracleKValue.schnorrNonce,
          "Oracle R value is inconsistent with k value")
}

case class SerializedSegwitSpendingInfo(
    outPoint: SerializedTransactionOutPoint,
    output: SerializedTransactionOutput,
    keys: Vector[ECPrivateKey],
    hashType: HashType,
    scriptWitness: ScriptWitnessV0) {

  def toSpendingInfo: P2WPKHV0SpendingInfo = {
    P2WPKHV0SpendingInfo(
      outPoint = outPoint.toOutPoint,
      amount = output.value,
      scriptPubKey = output.spk.asInstanceOf[P2WPKHWitnessSPKV0],
      signer = keys.head,
      hashType = hashType,
      scriptWitness = scriptWitness.asInstanceOf[P2WPKHWitnessV0]
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
    mutualCloseTx: Transaction,
    localClosingTx: Option[Transaction],
    remoteClosingTx: Option[Transaction]) {
  require(localCets.length == 2,
          s"There must be two local CETs, got $localCets")
  require(remoteCets.length == 2,
          s"There must be two remote CETs, got $remoteCets")
}
