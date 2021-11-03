package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  ScriptWitnessV0,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.util.sorted.{OrderedAnnouncements, OrderedNonces}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.io.Source

class DLCStaticTestVectors extends BitcoinSUnitTest {

  behavior of "DLCStaticTestVector"

  sealed trait TestCaseMessage[T <: DLCSetupTLV] {
    def serialized: String

    def msg: T

    def lnMsg: LnMessage[T] = LnMessage(msg)

    def tlv: T = {
      LnMessage.fromHex(serialized).tlv.asInstanceOf[T]
    }
  }

  case class OfferTestMessage(msg: DLCOfferTLV, serialized: String)
      extends TestCaseMessage[DLCOfferTLV]

  case class AcceptTestMessage(msg: DLCAcceptTLV, serialized: String)
      extends TestCaseMessage[DLCAcceptTLV]

  case class SignTestMessage(msg: DLCSignTLV, serialized: String)
      extends TestCaseMessage[DLCSignTLV]

  case class AllMessages(
      offerTestMessage: OfferTestMessage,
      acceptTestMessage: AcceptTestMessage,
      signTestMessage: SignTestMessage)

  it must "conform to the enum single test vector" in {
    val url = getClass.getResource("/enum_single_oracle_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    assert(offerMsg.tlv == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the single oracle numerical test" in {

    val url = getClass.getResource("/single_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the single oracle numerical hyperbola test" in {
    val url =
      getClass.getResource("/single_oracle_numerical_hyperbola_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_3_of_3_test.json test" in {
    val url =
      getClass.getResource("/enum_3_of_3_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the three_of_three_oracle_numerical_test.json test" in {
    val url =
      getClass.getResource("/three_of_three_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the three_of_three_oracle_numerical_with_diff_test.json test" in {
    val url =
      getClass.getResource(
        "/three_of_three_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the two_of_five_oracle_numerical_test.json test" in {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_test.json test" in {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_and_numerical_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_and_numerical_5_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_and_numerical_with_diff_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV.contractInfo == offerMsg.msg.contractInfo)

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  it must "conform to the enum_and_numerical_with_diff_5_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    val offerTLV = offerMsg.tlv

    assert(offerTLV.contractInfo == offerMsg.msg.contractInfo)

    assert(offerTLV == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
  }

  private def parseTestCase(jsonString: String): AllMessages = {
    val obj = ujson.read(jsonString).obj
    val offer = parseOfferMsg(obj)
    val accept = parseAcceptMsg(obj)
    val sign = parseSignMsg(obj)

    AllMessages(offer, accept, sign)
  }

  val offerMsgKey = "offer_message"
  val msgKey = "message"
  val messageKey = msgKey

  val contractFlagsKey = "contractFlags"
  val chainHashKey = "chainHash"

  val contractInfoKey = "contractInfo"
  val singleContractInfoKey = "singleContractInfo"
  val disjointContractInfoKey = "disjointContractInfo"
  val contractInfosKey = "contractInfos"

  val totalCollateralKey = "totalCollateral"
  val contractDescriptorKey = "contractDescriptor"
  val enumeratedContractDescriptorKey = "enumeratedContractDescriptor"
  val numericOutcomeContractDescriptorKey = "numericOutcomeContractDescriptor"
  val payoutsKey = "payouts"
  val localPayoutKey = "localPayout"

  //numeric contract descriptor
  val payFunctionKey = "payoutFunction"
  val payoutFunctionPiecesKey = "payoutFunctionPieces"
  val endPointKey = "endPoint"
  val eventOutcomeKey = "eventOutcome"
  val outcomePayoutKey = "outcomePayout"
  val extraPrecisionKey = "extraPrecision"

  val payoutCurvePieceKey = "payoutCurvePiece"
  val polynomialPayoutCurvePieceKey = "polynomialPayoutCurvePiece"
  val hyperbolaPayoutCurvePieceKey = "hyperbolaPayoutCurvePiece"
  val payoutPointsKey = "payoutPoints"

  val lastEndpointKey = "lastEndpoint"

  val roundingIntervalsKey = "roundingIntervals"
  val intervalsKey = "intervals"
  val beginIntervalKey = "beginInterval"
  val roundingModKey = "roundingMod"

  val usePositivePieceKey = "usePositivePiece"
  val translateOutcomeKey = "translateOutcome"
  val translatePayoutKey = "translatePayout"
  val aKey = "a"
  val bKey = "b"
  val cKey = "c"
  val dKey = "d"

  val oracleInfoKey = "oracleInfo"
  val singleKey = "single"
  val multiKey = "multi"
  val oracleParamsKey = "oracleParams"

  //oracle params
  val maxErrorExpKey = "maxErrorExp"
  val minFailExpKey = "minFailExp"
  val maximizeCoverageKey = "maximizeCoverage"

  val oracleAnnouncementKey = "oracleAnnouncement"
  val announcementSignatureKey = "announcementSignature"
  val oraclePublicKeyKey = "oraclePublicKey"
  val oracleEventKey = "oracleEvent"
  val oracleNoncesKey = "oracleNonces"
  val eventMaturityEpochKey = "eventMaturityEpoch"
  val eventDescriptorKey = "eventDescriptor"
  val enumEventKey = "enumEvent"
  val outcomesKey = "outcomes"
  val eventIdKey = "eventId"

  val thresholdKey = "threshold"
  val oracleAnnouncementsKey = "oracleAnnouncements"

  val fundingPubKeyKey = "fundingPubkey"
  val payoutSpkKey = "payoutSpk"
  val payoutSerialIdKey = "payoutSerialId"
  val offerCollateralKey = "offerCollateral"
  val fundingInputsKey = "fundingInputs"
  val changeSpkKey = "changeSpk"
  val changeSerialIdKey = "changeSerialId"
  val fundOutputSerialIdKey = "fundOutputSerialId"
  val feeRatePerKbKey = "feeRatePerVb"
  val cetLocktimeKey = "cetLocktime"
  val refundLocktimeKey = "refundLocktime"

  val acceptMessageKey = "accept_message"
  val temporaryContractIdKey = "temporaryContractId"
  val acceptCollateralKey = "acceptCollateral"
  val inputSerialIdKey = "inputSerialId"
  val prevTxKey = "prevTx"
  val prevTxVoutKey = "prevTxVout"
  val sequenceKey = "sequence"
  val maxWitnessLenKey = "maxWitnessLen"
  val redeemScriptKey = "redeemScript"
  val cetAdaptorSignaturesKey = "cetAdaptorSignatures"
  val ecdsaAdaptorSignaturesKey = "ecdsaAdaptorSignatures"
  val signatureKey = "signature"
  val refundSignatureKey = "refundSignature"

  val signMessageKey = "sign_message"
  val contractIdKey = "contractId"
  val fundingSignaturesKey = "fundingSignatures"
  val witnessElementsKey = "witnessElements"
  val witnessKey = "witness"
  val serializedKey = "serialized"

  private def parseOfferMsg(obj: ujson.Obj): OfferTestMessage = {
    val offerMsg = obj(offerMsgKey)
    val serialized = offerMsg(serializedKey).str
    val message = offerMsg.obj(msgKey)
    val protocolVersion = message(PicklerKeys.protocolVersionKey).num.toInt
    val contractFlags = message(contractFlagsKey)
    val chainHash = DoubleSha256Digest.fromHex(message(chainHashKey).str)
    val tempContractId =
      Sha256Digest.fromHex(message(temporaryContractIdKey).str)
    val contractInfo = parseContractInfo(message(contractInfoKey).obj)

    val fundingPubKey = ECPublicKey.fromHex(message(fundingPubKeyKey).str)
    val payoutSpk = ScriptPubKey.fromAsmHex(message(payoutSpkKey).str)
    val payoutSerialId = parseU64(message(payoutSerialIdKey).str)

    val offerCollateral = Satoshis(message(offerCollateralKey).num.toLong)

    val fundingInputs: Vector[FundingInputTLV] = {
      parseFundingInputs(message(fundingInputsKey).arr)

    }

    val changeSpk = ScriptPubKey.fromAsmHex(message(changeSpkKey).str)
    val changeSerialId = parseU64(message(changeSerialIdKey).str)
    val fundingOutputSerialId = parseU64(message(fundOutputSerialIdKey).str)

    val feeRatePerVb = SatoshisPerVirtualByte
      .fromLong(message(feeRatePerKbKey).num.toLong)
    val cetLocktime = message(cetLocktimeKey).num.toLong
    val refundLocktime = message(refundLocktimeKey).num.toLong

    val offerTLV = DLCOfferTLV(
      protocolVersionOpt = Some(protocolVersion),
      contractFlags = contractFlags.num.toByte,
      chainHash = chainHash,
      tempContractIdOpt = Some(tempContractId),
      contractInfo = contractInfo,
      fundingPubKey = fundingPubKey,
      payoutSPK = payoutSpk,
      payoutSerialId = payoutSerialId,
      offererCollateralSatoshis = offerCollateral,
      fundingInputs = fundingInputs,
      changeSPK = changeSpk,
      changeSerialId = changeSerialId,
      fundOutputSerialId = fundingOutputSerialId,
      feeRate = feeRatePerVb,
      contractMaturityBound = BlockTimeStamp.fromUInt32(UInt32(cetLocktime)),
      contractTimeout = BlockTimeStamp.fromUInt32(UInt32(refundLocktime))
    )

    OfferTestMessage(offerTLV, serialized = serialized)
  }

  private def parseContractInfo(obj: ujson.Obj): ContractInfoTLV = {
    val singleContractObjOpt = obj.value.get(singleContractInfoKey)
    val disjointContractObjOpt = obj.value.get(disjointContractInfoKey)
    if (singleContractObjOpt.isDefined) {
      val singleContractObj = singleContractObjOpt.get
      val contractInfoObj = singleContractObj(contractInfoKey).obj
      val totalCollateral: Satoshis = Satoshis(
        singleContractObj(totalCollateralKey).num.toLong)
      val contractDescriptorObj = contractInfoObj(contractDescriptorKey).obj
      val contractDescriptorTLV = parseContractDescriptor(contractDescriptorObj)
      val oracleInfoObj = contractInfoObj(oracleInfoKey).obj
      val oracleInfo = parseOracleInfo(oracleInfoObj)

      ContractInfoV0TLV(totalCollateral,
                        contractDescriptorTLV,
                        oracleInfo,
                        DLCSerializationVersion.current)
    } else if (disjointContractObjOpt.isDefined) {
      val disjointContractObj = disjointContractObjOpt.get
      val totalCollateral: Satoshis = Satoshis(
        disjointContractObj(totalCollateralKey).num.toLong)
      val contractInfosArr = disjointContractObj(contractInfosKey).arr
      val contractInfos = contractInfosArr.map { c =>
        val contractDescriptorObj = c(contractDescriptorKey).obj
        val descriptorTLV =
          parseContractDescriptor(contractDescriptorObj)
        val descriptor = ContractDescriptor.fromSubType(descriptorTLV)
        val oracleInfoObj = c(oracleInfoKey).obj
        val oracleInfoTLV = parseOracleInfo(oracleInfoObj)
        val oracleInfo = OracleInfo.fromSubType(oracleInfoTLV)
        val contractOraclePair =
          ContractOraclePair.fromDescriptorOracle(descriptor, oracleInfo)
        SingleContractInfo(totalCollateral, contractOraclePair)
      }
      DisjointUnionContractInfo(contractInfos.toVector).toSubType
    } else {
      sys.error(s"Contract info was not single or disjoint")
    }

  }

  private def parseContractDescriptor(obj: ujson.Obj): ContractDescriptorTLV = {
    val isEnum = obj.value.get(enumeratedContractDescriptorKey)
    val isNumeric = obj.value.get(numericOutcomeContractDescriptorKey)
    if (isEnum.isDefined) {
      parseEnumContractDescriptor(isEnum.get.obj).toSubType
    } else if (isNumeric.isDefined) {
      parseNumericContractDescriptor(isNumeric.get.obj).toSubType
    } else {
      sys.error(s"Not enum contract descriptor, got=$obj")
    }
  }

  private def parseEnumContractDescriptor(
      obj: ujson.Obj): EnumContractDescriptor = {
    val outcomes =
      obj(payoutsKey).arr.toVector.map {
        case payoutsObj: ujson.Obj =>
          val outcome = EnumOutcome(payoutsObj(PicklerKeys.outcomeKey).str)
          val payout = Satoshis(payoutsObj(localPayoutKey).num.toLong)
          (outcome, payout)
        case x: ujson.Value =>
          sys.error(s"Cannot parse payout from $x expected ujson.Obj")
      }

    EnumContractDescriptor(outcomes)
  }

  private def parseNumericContractDescriptor(
      obj: ujson.Obj): NumericContractDescriptor = {
    val payoutFunctionObj = obj(payFunctionKey).obj
    val payoutFunctionPieces = payoutFunctionObj(payoutFunctionPiecesKey).arr
    val lastEndPoint = parseTLVPoint(payoutFunctionObj(lastEndpointKey).obj)
    val payoutCurve = parseDLCPayoutCurve(
      arr = payoutFunctionPieces,
      lastEndPoint = lastEndPoint
    )

    val roundingIntervals: RoundingIntervals = {
      parseRoundingIntervals(obj(roundingIntervalsKey).obj)
    }

    val descriptor =
      NumericContractDescriptor(payoutCurve, numDigits = 10, roundingIntervals)

    descriptor
  }

  private def parseDLCPayoutCurve(
      arr: ujson.Arr,
      lastEndPoint: TLVPoint): DLCPayoutCurve = {
    val piecesWithPoints: Vector[(TLVPoint, PayoutCurvePieceTLV)] = {
      arr.value.toVector.map(value => parsePayoutFunctionPiece(value.obj))
    }

    val dlcCurvePieces: Vector[DLCPayoutCurvePiece] = {
      piecesWithPoints.zipWithIndex.map { case ((point, curve), idx) =>
        val rightEndpoint = {
          if (piecesWithPoints.length - 1 == idx) {
            lastEndPoint
          } else {
            piecesWithPoints(idx + 1)._1
          }
        }
        DLCPayoutCurvePiece.fromTLV(
          point,
          curve,
          rightEndpoint = rightEndpoint
        )
      }
    }

    DLCPayoutCurve(pieces = dlcCurvePieces,
                   serializationVersion = DLCSerializationVersion.current)
  }

  private def parsePayoutFunctionPiece(
      obj: ujson.Obj): (TLVPoint, PayoutCurvePieceTLV) = {
    val endPointObj =
      obj(endPointKey).obj
    val endPoint = parseTLVPoint(endPointObj)
    val curvePiece = obj(payoutCurvePieceKey).obj
    val parsedCurvePiece =
      if (curvePiece.get(polynomialPayoutCurvePieceKey).isDefined) {
        val polynomialCurvePiece = curvePiece(polynomialPayoutCurvePieceKey).obj
        val payoutPoints = polynomialCurvePiece(payoutPointsKey).arr
        val midPoints =
          payoutPoints.map(value => parseTLVPoint(value.obj)).toVector
        val polynomialPayoutCurvePieceTLV = PolynomialPayoutCurvePieceTLV(
          midPoints,
          DLCSerializationVersion.current)
        polynomialPayoutCurvePieceTLV
      } else if (curvePiece.get(hyperbolaPayoutCurvePieceKey).isDefined) {
        val hyperobla = curvePiece(hyperbolaPayoutCurvePieceKey)
        val usePositivePiece = hyperobla(usePositivePieceKey).bool
        val translateOutcome = Signed16PTLVNumber.fromBigDecimal(
          hyperobla(translateOutcomeKey).num.toInt)
        val translatePayout =
          Signed16PTLVNumber.fromBigDecimal(hyperobla(translatePayoutKey).num)
        val a = Signed16PTLVNumber.fromBigDecimal(hyperobla(aKey).num)
        val b = Signed16PTLVNumber.fromBigDecimal(hyperobla(bKey).num)
        val c = Signed16PTLVNumber.fromBigDecimal(hyperobla(cKey).num)
        val d = Signed16PTLVNumber.fromBigDecimal(hyperobla(dKey).num)

        HyperbolaPayoutCurvePieceTLV(usePositivePiece = usePositivePiece,
                                     translateOutcome = translateOutcome,
                                     translatePayout = translatePayout,
                                     a = a,
                                     b = b,
                                     c = c,
                                     d = d,
                                     DLCSerializationVersion.current)
      } else {
        sys.error(s"Unknown curve piece")
      }

    (endPoint, parsedCurvePiece)
  }

  private def parseTLVPoint(obj: ujson.Obj): TLVPoint = {
    val eventOutcome = obj(eventOutcomeKey).num.toLong
    val outcomePayout = obj(outcomePayoutKey).num.toLong
    val extraPrecision = obj(extraPrecisionKey).num.toInt
    TLVPoint(outcome = eventOutcome,
             value = Satoshis(outcomePayout),
             extraPrecision = extraPrecision,
             DLCSerializationVersion.current)
  }

  private def parseRoundingIntervals(obj: ujson.Obj): RoundingIntervals = {
    val intervalsArr = obj(intervalsKey).arr
    val roundingIntervals = intervalsArr.map {
      case o: ujson.Obj =>
        val beginInterval = o(beginIntervalKey).num.toInt
        val roundingMod = o(roundingModKey).num.toInt
        val interval =
          RoundingIntervals.IntervalStart(beginInterval, roundingMod)
        interval
      case x: ujson.Value =>
        sys.error(s"Expected json object for rounding intervals, got=$x")
    }

    RoundingIntervals(roundingIntervals.toVector)
  }

  private def parseOracleInfo(obj: ujson.Obj): OracleInfoTLV = {
    val isSingle = obj.value.get(singleKey)
    val isMulti = obj.value.get(multiKey)
    if (isSingle.isDefined) {
      val singleObj = isSingle.get
      val announcementObj = singleObj(oracleAnnouncementKey).obj
      val announcement = parseOracleAnnouncement(announcementObj)
      SingleOracleInfo(announcement).toSubType
    } else if (isMulti.isDefined) {
      val multiObj = isMulti.get
      val threshold = multiObj(thresholdKey).num.toInt
      val announcementsArr = multiObj(oracleAnnouncementsKey).arr
      val announcements =
        announcementsArr.toVector.map(v => parseOracleAnnouncement(v.obj))
      val orderedAnnouncements = OrderedAnnouncements(announcements)
      if (isAllEnumAnnouncement(announcements)) {
        EnumMultiOracleInfo(threshold,
                            OrderedAnnouncements(announcements)).toSubType
      } else if (isAllNumericAnnouncement(announcements)) {
        val oracleParams = parseOracleParams(multiObj(oracleParamsKey))
        val subType = NumericMultiOracleInfo(
          threshold = threshold,
          announcements = orderedAnnouncements,
          oracleParamsOpt = oracleParams).toSubType
        subType
      } else {
        sys.error(s"Mix announcements not supported yet")
      }

    } else {
      sys.error(s"Unknown oracle type, got=$obj")
    }
  }

  private def parseOracleParams(
      obj: ujson.Value): OptionTLV[OracleParamsTLV] = {
    if (obj == ujson.Null) {
      //means we have no oracle params
      NoneTLV
    } else {
      val maxErrorExp = obj(maxErrorExpKey).num.toInt
      val minFailExp = obj(minFailExpKey).num.toInt
      val maximizeCoverage = obj(maximizeCoverageKey).bool
      val params = OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage)
      SomeTLV(params)
    }
  }

  private def isAllEnumAnnouncement(
      announcements: Vector[OracleAnnouncementTLV]): Boolean = {
    announcements.forall(
      _.eventTLV.eventDescriptor.isInstanceOf[EnumEventDescriptorV0TLV])
  }

  private def isAllNumericAnnouncement(
      announcements: Vector[OracleAnnouncementTLV]): Boolean = {
    announcements.forall(
      _.eventTLV.eventDescriptor.isInstanceOf[NumericEventDescriptorTLV])
  }

  private def parseAcceptMsg(obj: ujson.Obj): AcceptTestMessage = {
    val acceptObj = obj(acceptMessageKey).obj
    val serialized = acceptObj(serializedKey).str
    val messageObj = acceptObj(messageKey).obj
    val protocolVersion = messageObj(PicklerKeys.protocolVersionKey).num.toInt
    val tempContractId =
      Sha256Digest.fromHex(messageObj(temporaryContractIdKey).str)
    val acceptCollateral = Satoshis(messageObj(acceptCollateralKey).num.toLong)
    val fundingPubKey = ECPublicKey.fromHex(messageObj(fundingPubKeyKey).str)
    val payoutSpk = ScriptPubKey.fromAsmHex(messageObj(payoutSpkKey).str)
    val payoutSerialId = parseU64(messageObj(payoutSerialIdKey).str)
    val fundingInputs = parseFundingInputs(messageObj(fundingInputsKey).arr)
    val changeSpk = ScriptPubKey.fromAsmHex(messageObj(changeSpkKey).str)
    val changeSerialId = parseU64(messageObj(changeSerialIdKey).str)
    val cetAdaptorSigs = parseCetAdaptorSignatures(
      messageObj(cetAdaptorSignaturesKey).obj)
    val refundSignature =
      ECDigitalSignature.fromHex(messageObj(refundSignatureKey).str)

    val acceptTLV = DLCAcceptTLV(
      protocolVersionOpt = Some(protocolVersion),
      tempContractId = tempContractId,
      acceptCollateralSatoshis = acceptCollateral,
      fundingPubKey = fundingPubKey,
      payoutSPK = payoutSpk,
      payoutSerialId = payoutSerialId,
      fundingInputs = fundingInputs,
      changeSPK = changeSpk,
      changeSerialId = changeSerialId,
      cetSignatures = cetAdaptorSigs,
      refundSignature = refundSignature,
      negotiationFields = NegotiationFieldsTLV.empty
    )

    AcceptTestMessage(acceptTLV, serialized)

  }

  private def parseSignMsg(obj: ujson.Obj): SignTestMessage = {
    val signObj = obj(signMessageKey).obj
    val serialized = signObj(serializedKey).str
    val messageObj = signObj(messageKey).obj
    val protocolVersion = messageObj(PicklerKeys.protocolVersionKey).num.toInt
    val contractId = ByteVector.fromValidHex(messageObj(contractIdKey).str)
    val adaptorSigs = parseCetAdaptorSignatures(
      messageObj(cetAdaptorSignaturesKey).obj)
    val refundSignature =
      ECDigitalSignature.fromHex(messageObj(refundSignatureKey).str)
    val fundingSignatures = parseFundingSignatures(
      messageObj(fundingSignaturesKey).obj)

    val signTLV =
      DLCSignTLV(Some(protocolVersion),
                 contractId,
                 adaptorSigs,
                 refundSignature,
                 fundingSignatures)

    SignTestMessage(signTLV, serialized)
  }

  private def parseFundingInputs(arr: ujson.Arr): Vector[FundingInputTLV] = {
    arr.value.toVector.map {
      case inputObj: ujson.Obj =>
        parseFundingInput(inputObj)
      case x: ujson.Value =>
        sys.error(s"Expected obj, got=$x")
    }
  }

  private def parseFundingInput(obj: ujson.Obj): FundingInputTLV = {
    val inputSerialId = parseU64(obj(inputSerialIdKey).str)
    val prevTx = Transaction.fromHex(obj(prevTxKey).str)
    val prevTxVout = obj(prevTxVoutKey).num.toLong
    val sequence = UInt32(obj(sequenceKey).num.toLong)
    val maxWitnessLen = UInt16(obj(maxWitnessLenKey).num.toLong)
    val redeemScriptStr = obj(redeemScriptKey).str
    val redeemScriptOpt = if (redeemScriptStr.nonEmpty) {
      val spk = WitnessScriptPubKey.fromAsmHex(obj(redeemScriptKey).str)
      Some(spk)
    } else {
      None
    }

    FundingInputV0TLV(
      inputSerialId,
      prevTx,
      UInt32(prevTxVout),
      sequence,
      maxWitnessLen,
      redeemScriptOpt,
      DLCSerializationVersion.current
    )
  }

  private def parseOracleAnnouncement(obj: ujson.Obj): OracleAnnouncementTLV = {
    val signature = SchnorrDigitalSignature
      .fromHex(obj(announcementSignatureKey).str)
    val oraclePubKey = SchnorrPublicKey.fromHex(obj(oraclePublicKeyKey).str)
    val oracleEventObj = obj(oracleEventKey).obj
    val oracleEventTLV = parseOracleEvent(oracleEventObj)

    OracleAnnouncementV0TLV.apply(signature, oraclePubKey, oracleEventTLV)
  }

  private def parseOracleEvent(obj: ujson.Obj): OracleEventV0TLV = {
    val eventMaturityEpoch = obj(eventMaturityEpochKey).num.toLong
    val eventDescriptorObj = obj(eventDescriptorKey).obj
    val eventDescriptor = parseEventDescriptor(eventDescriptorObj)
    val eventId = NormalizedString(obj(eventIdKey).str)
    val nonces = obj(oracleNoncesKey).arr.toVector.map {
      case str: ujson.Str =>
        SchnorrNonce.fromHex(str.str)
      case x: ujson.Value =>
        sys.error(s"Expected json string for nonces, got=$x")
    }

    val orderedNonces = OrderedNonces(nonces)

    OracleEventV0TLV(nonces = orderedNonces,
                     eventMaturityEpoch = UInt32(eventMaturityEpoch),
                     eventDescriptor = eventDescriptor,
                     eventId = eventId)
  }

  private def parseEventDescriptor(obj: ujson.Obj): EventDescriptorTLV = {
    val isEnumDescriptor = obj.value.get(enumEventKey)
    val isNumericContractDescriptor =
      obj.value.get(PicklerKeys.digitDecompositionEventKey)
    if (isEnumDescriptor.isDefined) {
      val enumDescriptorObj = isEnumDescriptor.get.obj
      parseEnumEventDescriptor(enumDescriptorObj)
    } else if (isNumericContractDescriptor.isDefined) {
      val digitDecompEventObj = isNumericContractDescriptor.get.obj
      val digitDecompEvent = parseDigitDecompEventDescriptor(
        digitDecompEventObj)
      digitDecompEvent
    } else {
      sys.error(s"Can only parse enum event descriptor so far")
    }
  }

  private def parseEnumEventDescriptor(
      obj: ujson.Obj): EnumEventDescriptorV0TLV = {
    val outcomes = obj(outcomesKey).arr.toVector.map {
      case str: ujson.Str =>
        NormalizedString(str.str)
      case x =>
        sys.error(s"Expected string for enum outcome, got=$x")
    }
    EnumEventDescriptorV0TLV(outcomes)
  }

  private def parseDigitDecompEventDescriptor(
      obj: ujson.Obj): DigitDecompositionEventDescriptorV0TLV = {
    val base = UInt16(obj(PicklerKeys.baseKey).num.toInt)
    val isSigned = obj(PicklerKeys.isSignedKey).bool
    val unit = NormalizedString(obj(PicklerKeys.unitKey).str)
    val precision = Int32(obj(PicklerKeys.precisionKey).num.toInt)
    val nbDigits = UInt16(obj(PicklerKeys.nbDigitsKey).num.toInt)

    if (isSigned) {
      SignedDigitDecompositionEventDescriptor(base, nbDigits, unit, precision)
    } else {
      UnsignedDigitDecompositionEventDescriptor(base,
                                                numDigits = nbDigits,
                                                unit = unit,
                                                precision = precision)
    }

  }

  private def parseCetAdaptorSignatures(obj: ujson.Obj): CETSignaturesTLV = {
    val ecAdaptorSignaturesArr = obj(ecdsaAdaptorSignaturesKey).arr
    val adaptorSigs = parseAdaptorSignatures(ecAdaptorSignaturesArr)
    CETSignaturesV0TLV(adaptorSigs, DLCSerializationVersion.current)
  }

  private def parseAdaptorSignatures(
      arr: ujson.Arr): Vector[ECAdaptorSignature] = {
    arr.value.toVector.map {
      case obj: ujson.Obj =>
        ECAdaptorSignature.fromHex(obj(signatureKey).str)
      case x: ujson.Value =>
        sys.error(s"Excpected string for ecdsa adaptor siganture, got obj=$x")
    }
  }

  private def parseFundingSignatures(obj: ujson.Obj): FundingSignaturesTLV = {
    val fundingSignatures: Vector[ujson.Value] = obj(
      fundingSignaturesKey).arr.toVector
    val witV0 = paresFundingSignaturesArr(fundingSignatures)
    FundingSignaturesV0TLV(witV0, DLCSerializationVersion.current)
  }

  private def paresFundingSignaturesArr(
      arr: Vector[ujson.Value]): Vector[ScriptWitnessV0] = {
    arr.map {
      case obj: ujson.Obj =>
        val witnessElementsArr = obj(witnessElementsKey).arr
        val witnesses: Vector[ByteVector] = {
          parseWitnessElements(witnessElementsArr)
        }

        val scriptWitnessV0 = ScriptWitness
          .apply(witnesses.reverse)
          .asInstanceOf[ScriptWitnessV0]
        scriptWitnessV0
      case x =>
        sys.error(s"Expected array of objects for funding signatures, got=$x")
    }
  }

  private def parseWitnessElements(arr: ujson.Arr): Vector[ByteVector] = {
    arr.value.toVector.map {
      case obj: ujson.Obj =>
        val witnessStr = obj(witnessKey).str
        ByteVector.fromValidHex(witnessStr)
      case x: ujson.Value =>
        sys.error(s"Expected witness json object, got=$x")
    }
  }

  private def parseU64(str: ujson.Str): UInt64 = {
    UInt64(BigInt(str.str))
  }
}

//must conform to the enum single test vector *** FAILED *** (683 milliseconds)
//[info]   "[fda71afd02440006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910ffdd82eda000000000bebc200fda71029040161000000000bebc200016200000000000000000163000000000bebc2000164000000000000000
//0fda712a1fdd8249dc5a85547be455db7aed0a03c4f8d9b45b0d5baac3c7534329a05827d0afbd4057d7951b20d1f86adcde402e6d4d2d8b0d80f9dd4d4227b9d9a9e909f22fdb2ae79fece7a58f025b865a8f562b128d20f6d5643d66c7e86c9830ce4b24
//6970055fdd822390001971fef1ec7bd1502cab3be96c27a3835ea6aae0129ae6dd520866674d91d996a60bf0bb0fdd8060a000401610162016301640454657374020d28500c90a24abf0c966b252b4368a48bd4f4355aac9cf20b1dc1c69cc45f190016001
//45a96e3dcabaa4e2a5da0f9ba2b74c6751c96c7936815a9b3b1e804000000000005f5e1000001fda714be6b4b18659574f6bc00a8020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101fff
//fffff0200f2052a01000000160014b57463cf52d05c5ad3918292a32cb81d9d7616c90000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf9012000000000000000000000000000000000000
//000000000000000000000000000000000000000000000ffffffff006b0000001600149cb01e57728a1abab75b53a84c3f3aae7509fbd8145e27b099f8a2002fd1c41a5f853e00000000000000000260bf0bb060c8463]0" did not equal
//
// "[a71a000622
//6e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f00000000000bebc20000040161000000000bebc200016200000000000000000163000000000bebc2000164000000000000000000c5a85547be455db7aed0a03c4f8d9b45b0d5ba
//ac3c7534329a05827d0afbd4057d7951b20d1f86adcde402e6d4d2d8b0d80f9dd4d4227b9d9a9e909f22fdb2ae79fece7a58f025b865a8f562b128d20f6d5643d66c7e86c9830ce4b24697005501971fef1ec7bd1502cab3be96c27a3835ea6aae0129ae6d
//d520866674d91d996a60bf0bb0000401610162016301640454657374020d28500c90a24abf0c966b252b4368a48bd4f4355aac9cf20b1dc1c69cc45f19001600145a96e3dcabaa4e2a5da0f9ba2b74c6751c96c7936815a9b3b1e8044b0000000005f5e100
//016b4b18659574f6bca8020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014b57463cf52d05c5ad3918292a32cb81d9d7616c90000000000000000
//266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf9012000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff006b0000001600149cb01e57728a1aba
//b75b53a84c3f3aae7509fbd8145e27b099f8a1ca2fd1c41a5f853dd0000000000000000260bf0bb060c84630000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000]0"
