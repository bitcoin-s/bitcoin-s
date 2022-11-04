package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.RescanComplete
import org.bitcoins.commons.serializers.JsonReaders.jsToSatoshis
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.config.DLC
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.hd.{AddressType, HDPath}
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  ScriptWitnessV0,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.util.TimeUtil._
import org.bitcoins.core.util.sorted.{
  OrderedAnnouncements,
  OrderedDLCPayoutCurvePieces,
  OrderedNonces,
  OrderedSchnorrSignatures
}
import org.bitcoins.core.util.{NetworkUtil, TimeUtil}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{AddressLabelTag, TxoState}
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._
import upickle.default._

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.Path
import java.time.Instant
import java.util.Date
import scala.util.Try

object Picklers {

  implicit val pathPickler: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, str => new File(str).toPath)

  implicit val inetSocketAddress: ReadWriter[InetSocketAddress] =
    readwriter[String].bimap(
      addr => s"${addr.getHostName}:${addr.getPort}",
      str => {
        val uri = new URI("tcp://" + str)
        InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
      })

  implicit val byteVectorPickler: ReadWriter[ByteVector] =
    readwriter[String].bimap(_.toHex, str => ByteVector.fromValidHex(str))

  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromString)

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val satoshisPickler: ReadWriter[Satoshis] =
    readwriter[Long].bimap(_.toLong, Satoshis.apply)

  implicit val schnorrNoncePickler: ReadWriter[SchnorrNonce] =
    readwriter[String].bimap(_.hex, SchnorrNonce.fromHex)

  implicit val enumEventDescriptorPickler: ReadWriter[
    EnumEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex, EnumEventDescriptorV0TLV.fromHex)

  implicit val digitDecompEventDescriptorPickler: ReadWriter[
    DigitDecompositionEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex,
                             DigitDecompositionEventDescriptorV0TLV.fromHex)

  implicit val eventDescriptorPickler: ReadWriter[EventDescriptorTLV] =
    readwriter[String].bimap(_.hex, EventDescriptorTLV.fromHex)

  implicit val eventDescriptorDLCTypePickler: ReadWriter[
    EventDescriptorDLCType] = {
    readwriter[ujson.Obj].bimap(writeEventDescriptor(_), readEventDescriptor(_))
  }

  implicit val oracleEventVoPickler: ReadWriter[OracleEventV0TLV] =
    readwriter[ujson.Obj]
      .bimap(writeOracleEventV0TLV(_), readOracleEventV0TLV(_))

  implicit val oracleEventV1Pickler: ReadWriter[OracleEventV1TLV] = {
    readwriter[ujson.Obj]
      .bimap(writeOracleEventV1TLV(_), readOracleEventV1TLV(_))
  }

  implicit val timestampPickler: ReadWriter[OracleEventTimestamp] = {
    readwriter[ujson.Obj].bimap(writeTimestamps(_), readTimestamps(_))
  }

  implicit val instantPickler: ReadWriter[Instant] =
    readwriter[Long].bimap(_.getEpochSecond, Instant.ofEpochSecond)

  implicit val datePickler: ReadWriter[Date] =
    readwriter[String].bimap(TimeUtil.iso8601ToString, TimeUtil.iso8601ToDate)

  implicit val aesPasswordPickler: ReadWriter[AesPassword] =
    readwriter[String].bimap(_.toStringSensitive, AesPassword.fromString)

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, Sha256DigestBE.fromHex)

  implicit val sha256DigestPickler: ReadWriter[Sha256Digest] =
    readwriter[String].bimap(_.hex, Sha256Digest.fromHex)

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE.fromHex)

  implicit val uInt32Pickler: ReadWriter[UInt32] =
    readwriter[Long].bimap(_.toLong, long => UInt32(long))

  implicit val satoshisPerVirtualBytePickler: ReadWriter[
    SatoshisPerVirtualByte] =
    readwriter[Long]
      .bimap(_.toLong, long => SatoshisPerVirtualByte(Satoshis(long)))

  implicit val oracleInfoPickler: ReadWriter[OracleInfo] =
    readwriter[String].bimap(_.hex, OracleInfo.fromHex)

  implicit val oracleAnnouncementPickler: ReadWriter[OracleAnnouncementTLV] =
    readwriter[String].bimap(_.hex, OracleAnnouncementTLV.fromHex)

  implicit val contractInfoTLVPickler: ReadWriter[ContractInfoTLV] =
    readwriter[Value].bimap(writeContractInfoJson(_), parseContractInfo(_))

  implicit val contractInfoV0TLVPickler: ReadWriter[ContractInfoV0TLV] =
    readwriter[String].bimap(_.hex, ContractInfoV0TLV.fromHex)

  implicit val schnorrDigitalSignaturePickler: ReadWriter[
    SchnorrDigitalSignature] =
    readwriter[String].bimap(_.hex, SchnorrDigitalSignature.fromHex)

  implicit val partialSignaturePickler: ReadWriter[PartialSignature] =
    readwriter[String].bimap(_.hex, PartialSignature.fromHex)

  implicit val lnMessageDLCOfferTLVPickler: ReadWriter[LnMessage[DLCOfferTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCOfferTLV).fromHex)

  implicit val txoStatePickler: ReadWriter[TxoState] = {
    readwriter[String].bimap(_.toString.toLowerCase, TxoState.fromString)
  }

  implicit val privKeyPathPickler: ReadWriter[HDPath] = {
    readwriter[String].bimap(_.toString, HDPath.fromString)
  }

  implicit val scriptPubKeyPickler: ReadWriter[ScriptPubKey] = {
    readwriter[String].bimap(_.asmHex, ScriptPubKey.fromAsmHex(_))
  }

  private def parseWitnessElements(arr: ujson.Arr): ScriptWitness = {
    val stackElements = arr.value.toVector.map {
      case obj: ujson.Obj =>
        val witnessStr = obj(PicklerKeys.witnessKey).str
        ByteVector.fromValidHex(witnessStr)
      case x: ujson.Value =>
        sys.error(s"Expected witness json object, got=$x")
    }

    ScriptWitness.apply(stackElements.reverse)
  }

  private def writeWitnessElements(witness: ScriptWitness): ujson.Arr = {
    val vec: Vector[ujson.Obj] = witness.stack.reverse.map { w =>
      ujson.Obj(PicklerKeys.witnessKey -> Str(w.toHex))
    }.toVector

    ujson.Arr.from(vec)
  }

  implicit val scriptWitnessPickler: ReadWriter[ScriptWitness] = {
    readwriter[Arr].bimap(writeWitnessElements, parseWitnessElements)
  }

  private def writeOutput(o: TransactionOutput): Obj = {
    Obj(
      PicklerKeys.satoshisKey -> writeJs(o.value.satoshis),
      PicklerKeys.scriptPubKeyKey -> writeJs(o.scriptPubKey)
    )
  }

  private def readOutput(obj: Obj): TransactionOutput = {
    val sats = Satoshis(obj(PicklerKeys.satoshisKey).num.toLong)
    val scriptPubKey =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.scriptPubKeyKey).str)
    TransactionOutput(sats, scriptPubKey)
  }

  implicit val txOutputPickler: ReadWriter[TransactionOutput] =
    readwriter[Obj].bimap(writeOutput, readOutput)

  private def writeSpendingInfoDb(si: SpendingInfoDb): Obj = {
    Obj(
      PicklerKeys.idKey -> {
        si.id match {
          case None     => ujson.Null
          case Some(id) => Num(id.toDouble)
        }
      },
      PicklerKeys.outPointKey -> writeJs(si.outPoint),
      PicklerKeys.outputKey -> writeJs(si.output),
      PicklerKeys.hdPathKey -> writeJs(si.privKeyPath),
      PicklerKeys.redeemScriptKey -> writeJs(si.redeemScriptOpt),
      PicklerKeys.witnessKey -> writeJs(si.scriptWitnessOpt),
      PicklerKeys.stateKey -> writeJs(si.state),
      PicklerKeys.txIdKey -> writeJs(si.txid),
      PicklerKeys.spendingTxIdKey -> writeJs(si.spendingTxIdOpt)
    )
  }

  private def readSpendingInfoDb(obj: Obj): SpendingInfoDb = {
    val id = obj(PicklerKeys.idKey).numOpt.map(_.toLong)
    val outpoint =
      upickle.default.read[TransactionOutPoint](obj(PicklerKeys.outPointKey))
    val output =
      upickle.default.read[TransactionOutput](obj(PicklerKeys.outputKey))
    val hdPath = upickle.default.read[HDPath](obj(PicklerKeys.hdPathKey))
    val redeemScript = upickle.default.read[Option[ScriptPubKey]](
      obj(PicklerKeys.redeemScriptKey))
    val scriptWitness =
      upickle.default.read[Option[ScriptWitness]](obj(PicklerKeys.witnessKey))
    val state = upickle.default.read[TxoState](obj(PicklerKeys.stateKey))
    val txId =
      upickle.default.read[DoubleSha256DigestBE](obj(PicklerKeys.txIdKey))
    val spendingTxId = upickle.default.read[Option[DoubleSha256DigestBE]](
      obj(PicklerKeys.spendingTxIdKey))
    SpendingInfoDb(
      id = id,
      outpoint = outpoint,
      output = output,
      hdPath = hdPath,
      redeemScriptOpt = redeemScript,
      scriptWitnessOpt = scriptWitness,
      state = state,
      txId = txId,
      spendingTxIdOpt = spendingTxId
    )
  }

  implicit val spendingInfoDbPickler: ReadWriter[SpendingInfoDb] = {
    readwriter[Obj].bimap(writeSpendingInfoDb, readSpendingInfoDb)
  }

  private def parseU64(str: ujson.Str): UInt64 = {
    UInt64(BigInt(str.str))
  }

  private def parseFundingInput(obj: ujson.Obj): FundingInputTLV = {
    val inputSerialId = parseU64(obj(PicklerKeys.inputSerialIdKey).str)
    val prevTx = Transaction.fromHex(obj(PicklerKeys.prevTxKey).str)
    val prevTxVout = obj(PicklerKeys.prevTxVoutKey).num.toLong
    val sequence = UInt32(obj(PicklerKeys.sequenceKey).num.toLong)
    val maxWitnessLen = UInt16(obj(PicklerKeys.maxWitnessLenKey).num.toLong)
    val redeemScriptStr = obj(PicklerKeys.redeemScriptKey).str
    val redeemScriptOpt = if (redeemScriptStr.nonEmpty) {
      val spk =
        WitnessScriptPubKey.fromAsmHex(obj(PicklerKeys.redeemScriptKey).str)
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

  private def parseFundingInputs(arr: ujson.Arr): Vector[FundingInputTLV] = {
    arr.value.toVector.map {
      case inputObj: ujson.Obj =>
        parseFundingInput(inputObj)
      case x: ujson.Value =>
        sys.error(s"Expected obj, got=$x")
    }
  }

  private def parseCetAdaptorSignatures(obj: ujson.Obj): CETSignaturesTLV = {
    val ecAdaptorSignaturesArr = obj(PicklerKeys.ecdsaAdaptorSignaturesKey).arr
    val adaptorSigs = parseAdaptorSignatures(ecAdaptorSignaturesArr)
    CETSignaturesV0TLV(adaptorSigs, DLCSerializationVersion.current)
  }

  private def parseAdaptorSignatures(
      arr: ujson.Arr): Vector[ECAdaptorSignature] = {
    arr.value.toVector.map {
      case obj: ujson.Obj =>
        ECAdaptorSignature.fromHex(obj(PicklerKeys.signatureKey).str)
      case x: ujson.Value =>
        sys.error(s"Excpected string for ecdsa adaptor siganture, got obj=$x")
    }
  }

  private def writeAdaptorSignatures(
      sigs: Vector[ECAdaptorSignature]): Vector[ujson.Obj] = {
    sigs.map { sig =>
      ujson.Obj(PicklerKeys.signatureKey -> Str(sig.hex))
    }
  }

  private def writeCetAdaptorSigs(
      cetSignaturesTLV: CETSignaturesTLV): ujson.Obj = {
    cetSignaturesTLV match {
      case v0: CETSignaturesV0TLV =>
        val sigsVec = writeAdaptorSignatures(v0.sigs)
        ujson.Obj(
          PicklerKeys.ecdsaAdaptorSignaturesKey -> ujson.Arr.from(sigsVec))
    }
  }

  private def writeOfferTLV(offer: DLCOfferTLV): ujson.Obj = {
    val protocolVersionJson =
      offer.protocolVersionOpt.map(ujson.Num(_)).getOrElse(ujson.Null)
    val nested = Obj(
      PicklerKeys.protocolVersionKey -> protocolVersionJson,
      PicklerKeys.contractFlagsKey -> ujson.Num(offer.contractFlags),
      PicklerKeys.chainHashKey -> ujson.Str(offer.chainHash.hex),
      PicklerKeys.temporaryContractIdKey -> ujson.Str(offer.tempContractId.hex),
      PicklerKeys.contractInfoKey -> upickle.default.writeJs(
        offer.contractInfo)(Picklers.contractInfoTLVPickler),
      PicklerKeys.fundingPubKeyKey -> ujson.Str(offer.fundingPubKey.hex),
      PicklerKeys.payoutSpkKey -> ujson.Str(offer.payoutSPK.asmHex),
      PicklerKeys.payoutSerialIdKey -> ujson.Str(
        offer.payoutSerialId.toBigInt.toString()),
      PicklerKeys.offerCollateralKey -> ujson.Num(
        offer.offererCollateralSatoshis.toLong.toDouble),
      PicklerKeys.fundingInputsKey -> upickle.default.writeJs(
        offer.fundingInputs),
      PicklerKeys.changeSpkKey -> ujson.Str(offer.changeSPK.asmHex),
      PicklerKeys.changeSerialIdKey -> ujson.Str(
        offer.changeSerialId.toBigInt.toString()),
      PicklerKeys.fundOutputSerialIdKey -> ujson.Str(
        offer.fundOutputSerialId.toBigInt.toString()),
      PicklerKeys.feeRatePerKbKey -> ujson.Num(offer.feeRate.toLong.toDouble),
      PicklerKeys.cetLocktimeKey -> ujson.Num(
        offer.contractMaturityBound.toUInt32.toLong.toDouble),
      PicklerKeys.refundLocktimeKey -> ujson.Num(
        offer.contractTimeout.toUInt32.toLong.toDouble)
    )
    val obj = ujson.Obj(
      PicklerKeys.messageKey -> nested,
      PicklerKeys.serializedKey -> ujson.Str(LnMessage(offer).hex)
    )

    obj
  }

  private def writeAcceptTLV(accept: DLCAcceptTLV): ujson.Obj = {
    val protocolVersionJson =
      accept.protocolVersionOpt.map(ujson.Num(_)).getOrElse(ujson.Null)
    val nested = Obj(
      PicklerKeys.protocolVersionKey -> protocolVersionJson,
      PicklerKeys.tempContractIdKey -> Str(accept.tempContractId.hex),
      PicklerKeys.acceptCollateralKey -> Num(
        accept.acceptCollateralSatoshis.toLong.toDouble),
      PicklerKeys.fundingPubKeyKey -> Str(accept.fundingPubKey.hex),
      PicklerKeys.payoutSpkKey -> Str(accept.payoutSPK.asmHex),
      PicklerKeys.payoutSerialIdKey -> Str(
        accept.payoutSerialId.toBigInt.toString()),
      PicklerKeys.fundingInputsKey -> writeJs(accept.fundingInputs),
      PicklerKeys.changeSpkKey -> Str(accept.changeSPK.asmHex),
      PicklerKeys.changeSerialIdKey -> Str(
        accept.changeSerialId.toBigInt.toString()),
      PicklerKeys.cetAdaptorSignaturesKey -> writeCetAdaptorSigs(
        accept.cetSignatures),
      PicklerKeys.refundSignatureKey -> Str(accept.refundSignature.hex),
      PicklerKeys.negotiationFieldsKey -> ujson.Null
    )

    val obj = ujson.Obj(
      PicklerKeys.messageKey -> nested,
      PicklerKeys.serializedKey -> ujson.Str(LnMessage(accept).hex)
    )

    obj
  }

  private def parseFundingSignatures(obj: ujson.Obj): FundingSignaturesTLV = {
    val fundingSignatures: Vector[ujson.Value] = obj(
      PicklerKeys.fundingSignaturesKey).arr.toVector
    val witV0 = paresFundingSignaturesArr(fundingSignatures)
    FundingSignaturesV0TLV(witV0, DLCSerializationVersion.current)
  }

  private def paresFundingSignaturesArr(
      arr: Vector[ujson.Value]): Vector[ScriptWitnessV0] = {
    arr.map {
      case obj: ujson.Obj =>
        val witnessElementsArr = obj(PicklerKeys.witnessElementsKey).arr
        val witness: ScriptWitness = {
          parseWitnessElements(witnessElementsArr)
        }

        val scriptWitnessV0 = witness.asInstanceOf[ScriptWitnessV0]
        scriptWitnessV0
      case x =>
        sys.error(s"Expected array of objects for funding signatures, got=$x")
    }
  }

  private def writeFundingSignatures(
      fundingSigs: FundingSignaturesTLV): ujson.Obj = {
    val sigs: Vector[ujson.Obj] = fundingSigs match {
      case v0: FundingSignaturesV0TLV =>
        val witnessJson: Vector[Obj] = {
          v0.witnesses.map { wit =>
            val witJson = writeWitnessElements(wit)
            ujson.Obj(PicklerKeys.witnessElementsKey -> witJson)
          }
        }
        witnessJson
    }
    ujson.Obj(
      PicklerKeys.fundingSignaturesKey -> ujson.Arr.from(sigs)
    )
  }

  private def writeSignTLV(sign: DLCSignTLV): ujson.Obj = {
    val protocolVersionJson =
      sign.protocolVersionOpt.map(ujson.Num(_)).getOrElse(ujson.Null)
    val nested = ujson.Obj(
      PicklerKeys.protocolVersionKey -> protocolVersionJson,
      PicklerKeys.contractIdKey -> ujson.Str(sign.contractId.toHex),
      PicklerKeys.cetAdaptorSignaturesKey -> writeCetAdaptorSigs(
        sign.cetSignatures),
      PicklerKeys.refundSignatureKey -> ujson.Str(sign.refundSignature.hex),
      PicklerKeys.fundingSignaturesKey ->
        writeFundingSignatures(sign.fundingSignatures)
    )

    ujson.Obj(
      PicklerKeys.messageKey -> nested,
      PicklerKeys.serializedKey -> ujson.Str(LnMessage(sign).hex)
    )
  }

  implicit val dlcOfferTLVPickler: ReadWriter[DLCOfferTLV] = {
    readwriter[Obj].bimap(writeOfferTLV(_), parseOfferTLV(_))
  }

  implicit val dlcAcceptTLVPickler: ReadWriter[DLCAcceptTLV] = {
    readwriter[Obj].bimap(writeAcceptTLV(_), parseAcceptTLV(_))
  }

  implicit val dlcSignTLVPickler: ReadWriter[DLCSignTLV] = {
    readwriter[Obj].bimap(writeSignTLV(_), parseSignTLV(_))
  }

  implicit val lnMessageDLCAcceptTLVPickler: ReadWriter[
    LnMessage[DLCAcceptTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCAcceptTLV).fromHex)

  implicit val lnMessageDLCSignTLVPickler: ReadWriter[LnMessage[DLCSignTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCSignTLV).fromHex)

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)

  implicit val blockPickler: ReadWriter[Block] = {
    readwriter[String].bimap(_.hex, Block.fromHex)
  }

  implicit val extPubKeyPickler: ReadWriter[ExtPublicKey] =
    readwriter[String].bimap(_.toString, ExtPublicKey.fromString)

  implicit val transactionOutPointPickler: ReadWriter[TransactionOutPoint] =
    readwriter[String].bimap(_.hex, TransactionOutPoint.fromHex)

  implicit val coinSelectionAlgoPickler: ReadWriter[CoinSelectionAlgo] =
    readwriter[String].bimap(_.toString, CoinSelectionAlgo.fromString)

  implicit val addressLabelTagPickler: ReadWriter[AddressLabelTag] =
    readwriter[String].bimap(_.name, AddressLabelTag)

  implicit val lockUnspentOutputParameterPickler: ReadWriter[
    LockUnspentOutputParameter] =
    readwriter[Value].bimap(_.toJson, LockUnspentOutputParameter.fromJson)

  // can't make implicit because it will overlap with ones needed for cli
  val announcementV0JsonWriter: Writer[OracleAnnouncementV0TLV] = {
    writer[Obj].comap { announcement =>
      val noncesJson = announcement.eventTLV.nonces.map { nonce =>
        Str(nonce.hex)
      }

      val descriptorJson = announcement.eventTLV.eventDescriptor match {
        case EnumEventDescriptorV0TLV(outcomes) =>
          Obj("outcomes" -> outcomes.map(Str(_)),
              "hex" -> announcement.eventTLV.eventDescriptor.hex)
        case numeric: NumericEventDescriptorTLV =>
          Obj(
            "base" -> Num(numeric.base.toLong.toDouble),
            "isSigned" -> Bool(numeric.isSigned),
            "unit" -> Str(numeric.unit),
            "precision" -> Num(numeric.precision.toLong.toDouble),
            "hex" -> announcement.eventTLV.eventDescriptor.hex
          )
      }

      val maturityStr =
        TimeUtil.iso8601ToString(Date.from(announcement.eventTLV.maturation))

      val eventJson = Obj("nonces" -> noncesJson,
                          "maturity" -> Str(maturityStr),
                          "descriptor" -> descriptorJson,
                          "eventId" -> Str(announcement.eventTLV.eventId))

      Obj(
        "announcementSignature" -> Str(announcement.announcementSignature.hex),
        "publicKey" -> Str(announcement.announcementPublicKey.hex),
        "event" -> eventJson,
        "hex" -> announcement.hex
      )
    }
  }

  val announcementV1JsonWriter: Writer[OracleAnnouncementV1TLV] = {
    writer[Obj].comap { announcement =>
      val oracleEventTLV: ujson.Value = announcement.eventTLV match {
        case v1: OracleEventV1TLV => writeJs(v1)(oracleEventV1Pickler)
      }

      val signature = Str(announcement.announcementSignature.hex)
      val metadata = writeJs(announcement.metadata)(oracleMetadataPickler)

      ujson.Obj(
        PicklerKeys.announcementSignatureKey -> signature,
        PicklerKeys.oracleEventKey -> oracleEventTLV,
        PicklerKeys.oracleMetadataKey -> metadata
      )
    }
  }

  val announcementV1JsonReader: Reader[OracleAnnouncementV1TLV] = {
    reader[Obj].map { case obj =>
      val announcementSignatureJson =
        obj(PicklerKeys.announcementSignatureKey).str
      val announcementSignature =
        SchnorrDigitalSignature.fromHex(announcementSignatureJson)

      val oracleMetadata = upickle.default.read(
        obj(PicklerKeys.oracleMetadataKey))(Picklers.oracleMetadataPickler)

      val oracleEvent = upickle.default.read(obj(PicklerKeys.oracleEventKey))(
        Picklers.oracleEventV1Pickler)

      OracleAnnouncementV1TLV(announcementSignature,
                              oracleEvent,
                              oracleMetadata)
    }
  }

  implicit val oracleAnnouncementV1TLVPickler: ReadWriter[
    OracleAnnouncementV1TLV] = {
    ReadWriter.join(announcementV1JsonReader, announcementV1JsonWriter)
  }

  // can't make implicit because it will overlap with ones needed for cli
  val oracleAnnouncementTLVJsonWriter: Writer[BaseOracleAnnouncement] =
    writer[Value].comap {
      case v0: OracleAnnouncementV0TLV =>
        writeJs(v0)(announcementV0JsonWriter)
      case v1: OracleAnnouncementV1TLV =>
        writeJs(v1)(announcementV1JsonWriter)
    }

  implicit val oracleMetadataPickler: ReadWriter[OracleMetadata] = {
    readwriter[Obj].bimap(writeOracleMetadata(_), readOracleMetadata(_))
  }

  implicit val schnorrSchemePickler: ReadWriter[SchnorrAttestation] = {
    readwriter[Obj].bimap(writeSchnorrScheme(_), readSchnorrScheme(_))
  }

  implicit val proofOfKnowledgePickler: ReadWriter[SchnorrProofOfKnowledge] = {
    readwriter[Obj].bimap(writeSchnorrProofOfKnowledge(_),
                          readSchnorrProofOfKnowledge(_))
  }

  // can't make implicit because it will overlap with ones needed for cli
  val oracleAttestmentV0Writer: Writer[OracleAttestmentV0TLV] =
    writer[Obj].comap { attestments =>
      val sigsJson = attestments.sigs.map(sig => Str(sig.hex))
      val valuesJson = attestments.outcomes.map(Str(_))

      Obj("eventId" -> Str(attestments.eventId),
          "signatures" -> sigsJson,
          "values" -> valuesJson,
          "hex" -> attestments.hex)
    }

  implicit val fundingInputV0Writer: Writer[FundingInputTLV] =
    writer[Value].comap { case v0: FundingInputV0TLV =>
      writeJs(v0)(fundingInputWriter)
    }

  implicit val fundingInputWriter: Writer[FundingInputV0TLV] =
    writer[Obj].comap { input =>
      import input._

      val redeemScriptJson = redeemScriptOpt match {
        case Some(rs) => Str(rs.hex)
        case None     => Str("")
      }

      Obj(
        "inputSerialId" -> Str(inputSerialId.toBigInt.toString()),
        "prevTx" -> Str(prevTx.hex),
        "prevTxVout" -> Num(prevTxVout.toLong.toDouble),
        "sequence" -> Num(sequence.toLong.toDouble),
        "maxWitnessLen" -> Num(maxWitnessLen.toLong.toDouble),
        "redeemScript" -> redeemScriptJson
      )
    }

  implicit val tlvPointReader: Reader[TLVPoint] = {
    reader[Obj].map { obj: Obj =>
      val map = obj.value
      val outcome = map(PicklerKeys.outcomeKey).num.toLong
      val payout = jsToSatoshis(map(PicklerKeys.payoutKey))
      val extraPrecision = map(PicklerKeys.extraPrecisionKey).num.toInt
      TLVPoint(outcome, payout, extraPrecision, DLCSerializationVersion.current)
    }
  }

  implicit val tlvPointWriter: Writer[TLVPoint] = {
    writer[Obj].comap { point =>
      Obj(
        PicklerKeys.eventOutcomeKey -> Num(point.outcome.toDouble),
        PicklerKeys.outcomePayoutKey -> Num(point.value.toLong.toDouble),
        PicklerKeys.extraPrecisionKey -> Num(point.extraPrecision.toDouble)
      )
    }
  }

  implicit val polynomialPayoutCurvePieceTLVWriter: Writer[
    PolynomialPayoutCurvePieceTLV] = {
    writer[Obj].comap { piece =>
      val pointsJs = piece.midpoints.map { point =>
        upickle.default.writeJs(point)(Picklers.tlvPointWriter)
      }

      val nested = ujson.Obj(
        PicklerKeys.payoutPointsKey -> ujson.Arr.from(pointsJs)
      )

      ujson.Obj(
        PicklerKeys.polynomialPayoutCurvePieceKey -> nested
      )
    }
  }

  implicit val hyperbolaPayoutCurvePieceTLVWriter: Writer[
    HyperbolaPayoutCurvePieceTLV] = {
    writer[Obj].comap { piece =>
      val nested = Obj(
        PicklerKeys.usePositivePiece -> Bool(piece.usePositivePiece),
        PicklerKeys.translateOutcome -> Num(
          piece.translateOutcome.toBigDecimal.toDouble),
        PicklerKeys.translatePayout -> Num(
          piece.translatePayout.toBigDecimal.toDouble),
        PicklerKeys.a -> Num(piece.a.toBigDecimal.toDouble),
        PicklerKeys.b -> Num(piece.b.toBigDecimal.toDouble),
        PicklerKeys.c -> Num(piece.c.toBigDecimal.toDouble),
        PicklerKeys.d -> Num(piece.d.toBigDecimal.toDouble)
      )

      ujson.Obj(PicklerKeys.hyperbolaPayoutCurvePieceKey -> nested)
    }
  }

  implicit val payoutCurvePieceTLVWriter: Writer[PayoutCurvePieceTLV] = {
    writer[ujson.Value].comap { curvePiece =>
      val nested = curvePiece match {
        case polynomial: PolynomialPayoutCurvePieceTLV =>
          writeJs(polynomial)(polynomialPayoutCurvePieceTLVWriter)
        case hyperbola: HyperbolaPayoutCurvePieceTLV =>
          writeJs(hyperbola)(hyperbolaPayoutCurvePieceTLVWriter)
      }
      nested
    }
  }

  implicit val payoutFunctionV0TLVWriter: Writer[PayoutFunctionV0TLV] = {

    writer[Obj].comap { payoutFunc =>
      val pieces =
        payoutFunc.piecesWithLeftEndpoints.map { case (leftEndpoint, piece) =>
          ujson.Obj(
            PicklerKeys.endPointKey -> upickle.default.writeJs(leftEndpoint),
            PicklerKeys.payoutCurvePieceKey -> upickle.default.writeJs(piece)
          )
        }
      ujson.Obj(
        PicklerKeys.payoutFunctionPiecesKey -> ujson.Arr.from(pieces),
        PicklerKeys.lastEndpointKey -> upickle.default.writeJs(
          payoutFunc.lastEndpoint)
      )
    }
  }

  implicit val payoutFunctionV0TLVReader: Reader[PayoutFunctionV0TLV] = {
    reader[Obj].map { obj: Obj =>
      val pointsArr = obj(PicklerKeys.pointsKey).arr
      val points: Vector[TLVPoint] = pointsArr.map {
        case x @ (_: Arr | _: Num | Null | _: Bool | _: Str) =>
          sys.error(
            s"Cannot have $x when parsing payout curve points, expected json object")
        case obj: Obj =>
          upickle.default.read[TLVPoint](obj)
      }.toVector

      DLCPayoutCurve
        .fromPoints(points, serializationVersion = DLCSerializationVersion.Beta)
        .toSubType
    }
  }

  implicit val roundingIntervalsV0TLVWriter: Writer[RoundingIntervalsV0TLV] =
    writer[Obj].comap { roundingIntervals =>
      import roundingIntervals._

      val intervalsJs = intervalStarts.map { i =>
        Obj("beginInterval" -> Num(i._1.toDouble),
            "roundingMod" -> Num(i._2.toLong.toDouble))
      }

      Obj("intervals" -> intervalsJs)
    }

  implicit val contractDescriptorV0: ReadWriter[ContractDescriptorV0TLV] = {
    readwriter[Value].bimap(enumeratedContractDescriptorWriter,
                            enumeratedContractDescriptorReader)
  }

  private def enumeratedContractDescriptorReader(
      value: Value): ContractDescriptorV0TLV = {
    parseContractDescriptor(value)
  }

  private def enumeratedContractDescriptorWriter(
      v0: ContractDescriptorV0TLV): Value = {
    val outcomesJs: Vector[ujson.Obj] = v0.outcomes.map {
      case (outcome, payout) =>
        ujson.Obj(
          PicklerKeys.outcomeKey -> ujson.Str(outcome),
          PicklerKeys.offerPayoutKey -> ujson.Num(payout.toLong.toDouble)
        )
    }
    val arr = ujson.Arr.from(outcomesJs)
    val payouts = ujson.Obj(
      PicklerKeys.payoutsKey -> arr
    )

    ujson.Obj(
      PicklerKeys.enumeratedContractDescriptorKey -> payouts
    )
  }

  implicit val contractDescriptorV1Writer: Writer[ContractDescriptorV1TLV] =
    writer[Obj].comap { v1 =>
      import v1._
      val payoutFunctionJson = writeJs(payoutFunction)
      val nested = Obj(
        PicklerKeys.nbDigitsKey -> Num(numDigits.toDouble),
        PicklerKeys.payoutFunctionKey -> payoutFunctionJson,
        PicklerKeys.roundingIntervalsKey -> writeJs(roundingIntervals)
      )

      ujson.Obj(PicklerKeys.numericOutcomeContractDescriptorKey -> nested)
    }

  implicit val contractDescriptorWriter: Writer[ContractDescriptorTLV] =
    writer[Value].comap {
      case v0: ContractDescriptorV0TLV =>
        writeJs(v0)(contractDescriptorV0)
      case v1: ContractDescriptorV1TLV =>
        writeJs(v1)(contractDescriptorV1Writer)
    }

  implicit val oracleInfoV0TLVWriter: Writer[OracleInfoV0TLV] =
    writer[Obj].comap { oracleInfo =>
      val nested = Obj(
        PicklerKeys.oracleAnnouncementKey ->
          writeJs(oracleInfo.announcement)(oracleAnnouncementTLVJsonWriter)
      )
      ujson.Obj(PicklerKeys.singleKey -> nested)
    }

  implicit val oracleInfoV1TLVWriter: Writer[OracleInfoV1TLV] =
    writer[Obj].comap { oracleInfo =>
      import oracleInfo._

      val oracleParamsJson = oracleInfo.oracleParamsOpt.toOption match {
        case Some(oracleParams) => writeJs(oracleParams)
        case None               => ujson.Null
      }
      val nested = Obj(
        PicklerKeys.thresholdKey -> Num(threshold.toDouble),
        PicklerKeys.oracleAnnouncementsKey -> oracles.map(o =>
          writeJs(o)(oracleAnnouncementTLVJsonWriter)),
        PicklerKeys.oracleParamsKey -> oracleParamsJson
      )

      ujson.Obj(
        PicklerKeys.multiKey -> nested
      )
    }

  implicit val oracleParamsV0TLVWriter: Writer[OracleParamsV0TLV] =
    writer[Obj].comap { params =>
      import params._
      Obj("maxErrorExp" -> Num(maxErrorExp.toDouble),
          "minFailExp" -> Num(minFailExp.toDouble),
          "maximizeCoverage" -> Bool(maximizeCoverage))
    }

  implicit val oracleParamsTLVWriter: Writer[OracleParamsTLV] =
    writer[Value].comap { case v0: OracleParamsV0TLV =>
      writeJs(v0)
    }

  implicit val oracleInfoTLVWriter: Writer[OracleInfoTLV] =
    writer[Value].comap {
      case v0: OracleInfoV0TLV => writeJs(v0)
      case v1: OracleInfoV1TLV => writeJs(v1)
    }

  // can't make implicit because it will overlap with ones needed for cli
  val contractInfoV0TLVJsonWriter: Writer[ContractInfoV0TLV] =
    writer[Obj].comap { contractInfo =>
      import contractInfo._

      val descriptorAndInfoObj = ujson.Obj(
        PicklerKeys.contractDescriptorKey -> writeJs(contractDescriptor),
        PicklerKeys.oracleInfoKey -> writeJs(oracleInfo)
      )

      val nested = Obj(
        PicklerKeys.totalCollateralKey -> writeJs(totalCollateral),
        PicklerKeys.contractInfoKey -> descriptorAndInfoObj
      )

      ujson.Obj(PicklerKeys.singleContractInfoKey -> nested)
    }

  val contractInfoV1TLVJsonWriter: Writer[ContractInfoV1TLV] = {
    writer[Obj].comap { contractInfo =>
      val arrayVec: Vector[ujson.Obj] = contractInfo.contractOraclePairs.map {
        case (c, o) =>
          val contractDescriptorJson = writeJs(c)
          val oracleInfoJson = writeJs(o)
          ujson.Obj(PicklerKeys.contractDescriptorKey -> contractDescriptorJson,
                    PicklerKeys.oracleInfoKey -> oracleInfoJson)
      }

      val arrayJson = ujson.Arr.from(arrayVec)

      val nested = Obj(
        PicklerKeys.totalCollateralKey -> Num(
          contractInfo.totalCollateral.toLong.toDouble),
        PicklerKeys.contractInfosKey -> arrayJson
      )

      ujson.Obj(
        PicklerKeys.disjointContractInfoKey -> nested
      )
    }
  }

  private def writeContractInfoJson(
      contractInfoTLV: ContractInfoTLV): ujson.Value = {

    contractInfoTLV match {
      case v0: ContractInfoV0TLV =>
        upickle.default.writeJs(v0)(contractInfoV0TLVJsonWriter)
      case v1: ContractInfoV1TLV =>
        upickle.default.writeJs(v1)(contractInfoV1TLVJsonWriter)
    }
  }

  implicit val offerTLVWriter: Writer[DLCOfferTLV] =
    writer[Obj].comap { offer =>
      import offer._
      Obj(
        "contractFlags" -> Str(contractFlags.toHexString),
        "chainHash" -> Str(chainHash.hex),
        "contractInfo" -> writeJs(contractInfo)(
          Picklers.contractInfoTLVPickler),
        "fundingPubKey" -> Str(fundingPubKey.hex),
        "payoutSPK" -> Str(payoutSPK.hex),
        "payoutSerialId" -> Num(payoutSerialId.toBigInt.toDouble),
        "offerCollateral" -> Num(offererCollateralSatoshis.toLong.toDouble),
        "fundingInputs" -> fundingInputs.map(i => writeJs(i)),
        "changeSPK" -> Str(changeSPK.hex),
        "changeSerialId" -> Str(changeSerialId.toBigInt.toString()),
        "fundOutputSerialId" -> Num(fundOutputSerialId.toBigInt.toDouble),
        "feeRatePerVb" -> Num(feeRate.toLong.toDouble),
        "cetLocktime" -> Num(contractMaturityBound.toUInt32.toLong.toDouble),
        "refundLocktime" -> Num(contractTimeout.toUInt32.toLong.toDouble),
        PicklerKeys.tempContractIdKey -> Str(offer.tempContractId.hex)
      )
    }

  implicit val payoutAddressW: Writer[PayoutAddress] = writer[Obj].comap {
    payoutAddress =>
      import payoutAddress._
      Obj(
        "address" -> Str(address.toString),
        "isExternal" -> Bool(isExternal)
      )
  }

  implicit val optionPayoutAddressW: Writer[Option[PayoutAddress]] =
    writer[Value].comap { payoutAddressOpt =>
      payoutAddressOpt
        .map(pa => writeJs(pa))
        .getOrElse(ujson.Null)
    }

  implicit val optionContactDbW: Writer[Option[DLCContactDb]] =
    writer[Value].comap {
      case Some(contact) => writeContactDb(contact)
      case None          => ujson.Null
    }

  implicit val offeredW: Writer[Offered] =
    writer[Obj].comap { offered =>
      import offered._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val acceptedComputingAdaptorSigsW: Writer[
    AcceptedComputingAdaptorSigs] = writer[Obj].comap { accepted =>
    import accepted._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val acceptedW: Writer[Accepted] = writer[Obj].comap { accepted =>
    import accepted._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val signedComputingAdaptorSigsW: Writer[SignedComputingAdaptorSigs] =
    writer[Obj].comap { signed =>
      import signed._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val signedW: Writer[Signed] = writer[Obj].comap { signed =>
    import signed._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val broadcastedW: Writer[Broadcasted] =
    writer[Obj].comap { broadcasted =>
      import broadcasted._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val confirmedW: Writer[Confirmed] =
    writer[Obj].comap { confirmed =>
      import confirmed._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  private val counterPartyPayoutKey: String = "counterPartyPayout"

  implicit val claimedW: Writer[Claimed] = writer[Obj].comap { claimed =>
    import claimed._
    val (oraclesJs, outcomesJs) = oracleOutcome match {
      case EnumOracleOutcome(oracles, outcome) =>
        (Arr.from(oracles.map(o => Str(o.announcement.hex))),
         Str(outcome.outcome))
      case numeric: NumericOracleOutcome =>
        (Arr.from(numeric.oracles.map(_.announcement.hex)),
         Arr.from(numeric.outcomes.map(o => Arr.from(o.digits))))
    }

    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "closingTxId" -> Str(closingTxId.hex),
      "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
      "outcomes" -> outcomesJs,
      "oracles" -> oraclesJs,
      PicklerKeys.myPayout -> Num(claimed.myPayout.satoshis.toLong.toDouble),
      counterPartyPayoutKey -> Num(
        claimed.counterPartyPayout.satoshis.toLong.toDouble),
      PicklerKeys.pnl -> Num(claimed.pnl.satoshis.toLong.toDouble),
      PicklerKeys.rateOfReturn -> Num(claimed.rateOfReturn.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val remoteClaimedW: Writer[RemoteClaimed] =
    writer[Obj].comap { remoteClaimed =>
      import remoteClaimed._
      val (oraclesJs, outcomesJs) = oracleOutcome match {
        case EnumOracleOutcome(oracles, outcome) =>
          (Arr.from(oracles.map(o => Str(o.announcement.hex))),
           Str(outcome.outcome))
        case numeric: NumericOracleOutcome =>
          (Arr.from(numeric.oracles.map(_.announcement.hex)),
           Arr.from(numeric.outcomes.map(o => Arr.from(o.digits))))
      }

      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "closingTxId" -> Str(closingTxId.hex),
        "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
        "outcomes" -> outcomesJs,
        "oracles" -> oraclesJs,
        PicklerKeys.myPayout -> Num(
          remoteClaimed.myPayout.satoshis.toLong.toDouble),
        counterPartyPayoutKey -> Num(
          remoteClaimed.counterPartyPayout.satoshis.toLong.toDouble),
        PicklerKeys.pnl -> Num(remoteClaimed.pnl.satoshis.toLong.toDouble),
        PicklerKeys.rateOfReturn -> Num(remoteClaimed.rateOfReturn.toDouble),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val refundedW: Writer[Refunded] = writer[Obj].comap { refunded =>
    import refunded._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "closingTxId" -> Str(closingTxId.hex),
      PicklerKeys.myPayout -> Num(refunded.myPayout.satoshis.toLong.toDouble),
      counterPartyPayoutKey -> Num(
        refunded.counterPartyPayout.satoshis.toLong.toDouble),
      PicklerKeys.pnl -> Num(refunded.pnl.satoshis.toLong.toDouble),
      PicklerKeys.rateOfReturn -> Num(refunded.rateOfReturn.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val dlcOfferAddW: Writer[IncomingDLCOfferDb] = writer[Obj].comap {
    offerDb =>
      Obj(
        "hash" -> offerDb.hash.hex,
        "receivedAt" -> Num(offerDb.receivedAt.getEpochSecond.toDouble),
        "peer" -> offerDb.peer.map(Str).getOrElse(Null),
        "message" -> offerDb.message.map(Str).getOrElse(Null),
        "offerTLV" -> offerDb.offerTLV.hex
      )
  }

  implicit val dlcOfferRemoveW: Writer[Sha256Digest] = writer[Value].comap {
    offerHash => writeJs(offerHash.hex)
  }

  implicit val dlcStatusW: Writer[DLCStatus] = writer[Value].comap {
    case o: Offered =>
      writeJs(o)(offeredW)
    case a: AcceptedComputingAdaptorSigs =>
      writeJs(a)(acceptedComputingAdaptorSigsW)
    case a: Accepted =>
      writeJs(a)(acceptedW)
    case s: SignedComputingAdaptorSigs =>
      writeJs(s)(signedComputingAdaptorSigsW)
    case s: Signed =>
      writeJs(s)(signedW)
    case b: Broadcasted =>
      writeJs(b)(broadcastedW)
    case c: Confirmed =>
      writeJs(c)(confirmedW)
    case c: Claimed =>
      writeJs(c)(claimedW)
    case r: RemoteClaimed =>
      writeJs(r)(remoteClaimedW)
    case r: Refunded =>
      writeJs(r)(refundedW)
  }

  implicit val dlcOfferAddR: Reader[IncomingDLCOfferDb] = reader[Obj].map {
    obj =>
      val hash = Sha256Digest(obj("hash").str)
      val peer = Try(obj("peer").str).toOption
      val message = Try(obj("message").str).toOption
      val receivedAt = Instant.ofEpochSecond(obj("receivedAt").num.toLong)
      val offerTLV = DLCOfferTLV.fromHex(obj("offerTLV").str)
      IncomingDLCOfferDb(hash = hash,
                         peer = peer,
                         message = message,
                         receivedAt = receivedAt,
                         offerTLV = offerTLV)
  }

  implicit val dlcOfferRemoveR: Reader[Sha256Digest] =
    reader[Value].map { obj => Sha256Digest(obj.str) }

  implicit val dlcStatusR: Reader[DLCStatus] = reader[Obj].map { obj =>
    val dlcId = Sha256Digest(obj("dlcId").str)
    val state = DLCState.fromString(obj("state").str)
    val lastUpdated = iso8601ToInstant(obj("lastUpdated").str)
    val isInitiator = obj("isInitiator").bool
    val tempContractId = Sha256Digest(obj(PicklerKeys.tempContractIdKey).str)
    val contractInfoTLV = ContractInfoV0TLV(obj("contractInfo").str)
    val contractMaturity =
      BlockStamp(UInt32(obj("contractMaturity").num.toLong))
    val contractTimeout = BlockStamp(UInt32(obj("contractTimeout").num.toLong))
    val feeRate = SatoshisPerVirtualByte.fromLong(obj("feeRate").num.toLong)
    val totalCollateral = Satoshis(obj("totalCollateral").num.toLong)
    val localCollateral = Satoshis(obj("localCollateral").num.toLong)
    val peerOpt =
      if (obj("peer").isNull || !obj.value.contains("peer")) None
      else Some(obj("peer").str)

    lazy val contractId = ByteVector.fromValidHex(obj("contractId").str)
    lazy val fundingTxId = DoubleSha256DigestBE(obj("fundingTxId").str)
    lazy val closingTxId = DoubleSha256DigestBE(obj("closingTxId").str)
    lazy val oracleSigs = {
      val unsorted = obj("oracleSigs").arr
        .map(value => SchnorrDigitalSignature(value.str))
        .toVector
      OrderedSchnorrSignatures.fromUnsorted(unsorted)
    }

    val payoutAddressJs = obj("payoutAddress")
    lazy val payoutAddress: Option[PayoutAddress] = payoutAddressJs match {
      case json: Obj =>
        json("address").strOpt.map(a =>
          PayoutAddress(BitcoinAddress.fromString(a),
                        json("isExternal").boolOpt.getOrElse(false)))
      case Null => None
      case v: Value =>
        throw new IllegalArgumentException(s"Unexpected payout address $v")

    }

    lazy val outcomesJs = obj("outcomes")
    lazy val outcomes = outcomesJs.strOpt match {
      case Some(value) => Vector(EnumOutcome(value))
      case None =>
        outcomesJs.arr.map { outcomeJs =>
          val digits = outcomeJs.arr.map(value => value.num.toInt)
          UnsignedNumericOutcome(digits.toVector)
        }.toVector
    }

    lazy val oraclesJs = obj("oracles")
    lazy val oracles = oraclesJs.arr.map { value =>
      val announcementTLV = OracleAnnouncementV1TLV(value.str)
      SingleOracleInfo(announcementTLV)
    }.toVector

    lazy val oracleOutcome = outcomes.head match {
      case outcome: EnumOutcome =>
        EnumOracleOutcome(oracles.asInstanceOf[Vector[EnumSingleOracleInfo]],
                          outcome)
      case UnsignedNumericOutcome(_) =>
        val numericOutcomes =
          outcomes.map(_.asInstanceOf[UnsignedNumericOutcome])
        val numericOracles =
          oracles.map(_.asInstanceOf[NumericSingleOracleInfo])
        NumericOracleOutcome(numericOracles.zip(numericOutcomes))
      case signed: SignedNumericOutcome =>
        throw new IllegalArgumentException(s"Unexpected outcome $signed")
    }

    lazy val myPayoutJs = obj(PicklerKeys.myPayout)
    lazy val myPayoutOpt = myPayoutJs.numOpt.map(sats => Satoshis(sats.toLong))
    lazy val theirPayoutJs = obj(counterPartyPayoutKey)
    lazy val theirPayoutOpt =
      theirPayoutJs.numOpt.map(sats => Satoshis(sats.toLong))

    state match {
      case DLCState.Offered =>
        Offered(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.AcceptComputingAdaptorSigs =>
        AcceptedComputingAdaptorSigs(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.Accepted =>
        Accepted(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.SignComputingAdaptorSigs =>
        SignedComputingAdaptorSigs(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Signed =>
        Signed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Broadcasted =>
        Broadcasted(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Confirmed =>
        Confirmed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Claimed =>
        Claimed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs,
          oracleOutcome,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
      case DLCState.RemoteClaimed =>
        require(oracleSigs.size == 1,
                "Remote claimed should only have one oracle sig")
        RemoteClaimed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs.head,
          oracleOutcome,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
      case DLCState.Refunded =>
        Refunded(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromSubType(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
    }
  }

  implicit val dlcWalletAccountingWriter: Writer[DLCWalletAccounting] = {
    writer[Obj].comap { walletAccounting: DLCWalletAccounting =>
      Obj(
        PicklerKeys.myCollateral -> Num(
          walletAccounting.myCollateral.satoshis.toLong.toDouble),
        PicklerKeys.theirCollateral -> Num(
          walletAccounting.theirCollateral.satoshis.toLong.toDouble),
        PicklerKeys.myPayout -> Num(
          walletAccounting.myPayout.satoshis.toLong.toDouble),
        PicklerKeys.theirPayout -> Num(
          walletAccounting.theirPayout.satoshis.toLong.toDouble),
        PicklerKeys.pnl -> Num(walletAccounting.pnl.satoshis.toLong.toDouble),
        PicklerKeys.rateOfReturn -> Num(walletAccounting.rateOfReturn.toDouble)
      )
    }
  }

  implicit val mnemonicCodePickler: ReadWriter[MnemonicCode] =
    readwriter[String].bimap(
      _.words.mkString(" "),
      str => MnemonicCode.fromWords(str.split(' ').toVector))

  implicit val extPrivateKeyPickler: ReadWriter[ExtPrivateKey] =
    readwriter[String].bimap(ExtKey.toString, ExtPrivateKey.fromString)

  implicit val oracleAnnouncementTLV: ReadWriter[OracleAnnouncementV0TLV] =
    readwriter[String].bimap(_.hex, OracleAnnouncementV0TLV.fromHex)

  implicit val oracleAttestmentTLV: ReadWriter[OracleAttestmentTLV] =
    readwriter[String]
      .bimap(_.hex, OracleAttestmentTLV.fromHex(_))

  implicit val schnorrAttestationTLV: ReadWriter[SchnorrAttestationTLV] =
    readwriter[ujson.Obj]
      .bimap(writeSchnorrAttestation(_), readSchnorrAttestation(_))

  implicit val oracleAttestmentV0TLV: ReadWriter[OracleAttestmentV0TLV] =
    readwriter[String].bimap(_.hex, OracleAttestmentV0TLV.fromHex)

  implicit val ecPublicKeyPickler: ReadWriter[ECPublicKey] =
    readwriter[String].bimap(_.hex, ECPublicKey.fromHex)

  implicit val addressTypePickler: ReadWriter[AddressType] =
    readwriter[String].bimap(_.toString, AddressType.fromString)

  def parseContractDescriptor(payoutsVal: Value): ContractDescriptorV0TLV = {
    val outcomes = payoutsVal(PicklerKeys.outcomesKey)
    val payouts: Vector[(String, Satoshis)] = outcomes.obj.toVector.map {
      case (outcome, payoutJs) =>
        val payout = jsToSatoshis(payoutJs.num)
        (outcome, payout)
    }
    ContractDescriptorV0TLV(outcomes = payouts, DLCSerializationVersion.current)
  }

  private def readBlockHeaderResult(obj: Obj): GetBlockHeaderResult = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val confirmations = obj(PicklerKeys.confirmationsKey).num.toInt
    val height = obj(PicklerKeys.heightKey).num.toInt
    val version = obj(PicklerKeys.versionKey).num.toInt
    val versionHex = Int32.fromHex(obj(PicklerKeys.versionHexKey).str)
    val merkleroot =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.merklerootKey).str)
    val time = UInt32(obj(PicklerKeys.timeKey).num.toLong)
    val mediantime = UInt32(obj(PicklerKeys.mediantimeKey).num.toLong)
    val nonce = UInt32(obj(PicklerKeys.nonceKey).num.toLong)
    val bits = UInt32.fromHex(obj(PicklerKeys.bitsKey).str)
    val difficulty = obj(PicklerKeys.difficultyKey).num
    val chainWork = obj(PicklerKeys.chainworkKey).str
    val previousBlockHash = obj(PicklerKeys.previousblockhashKey).strOpt.map {
      str =>
        DoubleSha256DigestBE.fromHex(str)
    }

    val nextblockhash = obj(PicklerKeys.nextblockhashKey).strOpt.map { str =>
      DoubleSha256DigestBE.fromHex(str)
    }

    GetBlockHeaderResult(
      hash = hash,
      confirmations = confirmations,
      height = height,
      version = version,
      versionHex = versionHex,
      merkleroot = merkleroot,
      time = time,
      mediantime = mediantime,
      nonce = nonce,
      bits = bits,
      difficulty = difficulty,
      chainwork = chainWork,
      previousblockhash = previousBlockHash,
      nextblockhash = nextblockhash
    )
  }

  private def writeBlockHeaderResult(header: GetBlockHeaderResult): Obj = {
    val json = Obj(
      PicklerKeys.rawKey -> Str(header.blockHeader.hex),
      PicklerKeys.hashKey -> Str(header.hash.hex),
      PicklerKeys.confirmationsKey -> Num(header.confirmations),
      PicklerKeys.heightKey -> Num(header.height),
      PicklerKeys.versionKey -> Num(header.version.toLong.toDouble),
      PicklerKeys.versionHexKey -> Str(Int32(header.version).hex),
      PicklerKeys.merklerootKey -> Str(header.merkleroot.hex),
      PicklerKeys.timeKey -> Num(header.time.toBigInt.toDouble),
      PicklerKeys.mediantimeKey -> Num(header.mediantime.toLong.toDouble),
      PicklerKeys.nonceKey -> Num(header.nonce.toBigInt.toDouble),
      PicklerKeys.bitsKey -> Str(header.bits.hex),
      PicklerKeys.difficultyKey -> Num(header.difficulty.toDouble),
      PicklerKeys.chainworkKey -> Str(header.chainwork),
      PicklerKeys.previousblockhashKey -> {
        header.previousblockhash.map(_.hex) match {
          case Some(str) => Str(str)
          case None      => ujson.Null
        }
      },
      PicklerKeys.nextblockhashKey -> {
        header.nextblockhash.map(_.hex) match {
          case Some(str) => Str(str)
          case None      => ujson.Null
        }
      }
    )
    json
  }

  implicit val getBlockHeaderResultPickler: ReadWriter[GetBlockHeaderResult] = {
    readwriter[ujson.Obj]
      .bimap(writeBlockHeaderResult(_), readBlockHeaderResult(_))
  }

  implicit val compactFilterHeaderPickler: ReadWriter[CompactFilterHeaderDb] = {
    readwriter[ujson.Obj]
      .bimap(writeCompactFilterHeaderDb(_), readCompactFilterHeaderDb(_))
  }

  implicit val compactFilterDbPickler: ReadWriter[CompactFilterDb] = {
    readwriter[ujson.Obj]
      .bimap(writeCompactFilterDb(_), readCompactFilterDb(_))
  }

  private def writeCompactFilterDb(
      compactFilterDb: CompactFilterDb): ujson.Obj = {
    ujson.Obj(
      PicklerKeys.hashKey -> ujson.Str(compactFilterDb.hashBE.hex),
      PicklerKeys.filterTypeKey -> ujson.Str(
        compactFilterDb.filterType.toString),
      PicklerKeys.compactFilterBytesKey -> ujson.Str(
        compactFilterDb.bytes.toHex),
      PicklerKeys.heightKey -> ujson.Num(compactFilterDb.height),
      PicklerKeys.blockHashKey -> ujson.Str(compactFilterDb.blockHashBE.hex)
    )
  }

  private def readCompactFilterDb(obj: ujson.Obj): CompactFilterDb = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val filterType = FilterType.fromString(obj(PicklerKeys.filterTypeKey).str)
    val bytes =
      ByteVector.fromValidHex(obj(PicklerKeys.compactFilterBytesKey).str)
    val height = obj(PicklerKeys.heightKey).num.toInt
    val blockHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.blockHashKey).str)

    CompactFilterDb(
      hashBE = hash,
      filterType = filterType,
      bytes = bytes,
      height = height,
      blockHashBE = blockHash
    )
  }

  private def writeCompactFilterHeaderDb(
      filterHeaderDb: CompactFilterHeaderDb): ujson.Obj = {
    ujson.Obj(
      PicklerKeys.hashKey -> ujson.Str(filterHeaderDb.hashBE.hex),
      PicklerKeys.filterHashKey -> ujson.Str(filterHeaderDb.filterHashBE.hex),
      PicklerKeys.previousFilterHeaderKey ->
        ujson.Str(filterHeaderDb.previousFilterHeaderBE.hex),
      PicklerKeys.blockHashKey -> ujson.Str(filterHeaderDb.blockHashBE.hex),
      PicklerKeys.heightKey -> ujson.Num(filterHeaderDb.height)
    )
  }

  private def readCompactFilterHeaderDb(
      obj: ujson.Obj): CompactFilterHeaderDb = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val filterHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.filterHashKey).str)
    val previousFilterHeader =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.previousFilterHeaderKey).str)
    val blockHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.blockHashKey).str)
    val height = obj(PicklerKeys.heightKey).num
    CompactFilterHeaderDb(hashBE = hash,
                          filterHashBE = filterHash,
                          previousFilterHeaderBE = previousFilterHeader,
                          blockHashBE = blockHash,
                          height = height.toInt)
  }

  private def writeContactDb(contact: DLCContactDb): ujson.Obj = {
    Obj(
      PicklerKeys.aliasKey -> contact.alias,
      PicklerKeys.addressKey -> s"${contact.address.getHostName}:${contact.address.getPort}",
      PicklerKeys.memoKey -> contact.memo
    )
  }

  private def readContactDb(obj: ujson.Obj): DLCContactDb = {
    val addressStr = obj(PicklerKeys.addressKey).str
    val address: InetSocketAddress =
      NetworkUtil.parseInetSocketAddress(addressStr, DLC.DefaultPort)
    DLCContactDb(
      alias = obj(PicklerKeys.aliasKey).str,
      address = address,
      memo = obj(PicklerKeys.memoKey).str
    )
  }

  implicit val contactDbPickler: ReadWriter[DLCContactDb] = {
    readwriter[ujson.Obj]
      .bimap(writeContactDb(_), readContactDb(_))
  }

  implicit val rescanComplete: ReadWriter[RescanComplete] = {
    readwriter[ujson.Value].bimap(writeRescanComplete(_), readRescanComplete(_))
  }

  private def writeRescanComplete(rescanComplete: RescanComplete): ujson.Str = {
    ujson.Str(rescanComplete.payload)
  }

  private def readRescanComplete(value: ujson.Value): RescanComplete = {
    RescanComplete(value.str)
  }

  implicit val feeUnit: ReadWriter[FeeUnit] = {
    readwriter[ujson.Value].bimap(writeFeeUnit(_), readFeeUnit(_))
  }

  private def writeFeeUnit(unit: FeeUnit): Value = unit match {
    case SatoshisPerVirtualByte(currencyUnit) =>
      ujson.Num(currencyUnit.satoshis.toDouble)
    case err: FeeUnit =>
      throw new RuntimeException(s"Unsupported fee unit type: `$err`")
  }

  private def readFeeUnit(value: Value): FeeUnit = {
    SatoshisPerVirtualByte.fromLong(value.num.toLong)
  }

  private def readOracleMetadata(obj: ujson.Obj): OracleMetadata = {
    val announcementPubKeyJson = obj(PicklerKeys.announcementPublicKeyKey).str
    val announcementPublicKey = SchnorrPublicKey.fromHex(announcementPubKeyJson)
    val oracleName = NormalizedString(obj(PicklerKeys.oracleNameKey).str)
    val oracleDescription = NormalizedString(
      obj(PicklerKeys.oracleDescriptionKey).str)
    val timestamp = UInt32(obj(PicklerKeys.timestampKey).num.toLong)

    val attestationSchemeJson = obj(PicklerKeys.attestationSchemeKey).obj
    val oracleSchemes =
      upickle.default.read(attestationSchemeJson)(schnorrSchemePickler)
    val metadataSigJson = obj(PicklerKeys.oracleMetaDataSignatureKey).str
    val metadataSignature = OracleMetadataSignature(metadataSigJson)

    OracleMetadata(
      announcementPublicKey = announcementPublicKey,
      oracleName = oracleName,
      oracleDescription = oracleDescription,
      creationTime = timestamp,
      attestations = oracleSchemes,
      metadataSignature = metadataSignature
    )
  }

  private def writeOracleMetadata(metadata: OracleMetadata): ujson.Obj = {
    val announcementPublicKey = ujson.Str(metadata.announcementPublicKey.hex)
    val oracleName = ujson.Str(metadata.oracleName)
    val oracleDescription = ujson.Str(metadata.oracleDescription)
    val timestamp = ujson.Num(metadata.creationTime.toLong.toDouble)
    val attestationScheme =
      writeJs(metadata.attestations)(schnorrSchemePickler)
    val oracleMetadataSignature = ujson.Str(metadata.metadataSignature.hex)
    ujson.Obj(
      PicklerKeys.announcementPublicKeyKey -> announcementPublicKey,
      PicklerKeys.oracleNameKey -> oracleName,
      PicklerKeys.oracleDescriptionKey -> oracleDescription,
      PicklerKeys.timestampKey -> timestamp,
      PicklerKeys.attestationSchemeKey -> attestationScheme,
      PicklerKeys.oracleMetaDataSignatureKey -> oracleMetadataSignature,
      PicklerKeys.oracleMetaDataSignatureKey -> oracleMetadataSignature
    )
  }

  private def writeSchnorrScheme(
      schnorrScheme: SchnorrAttestation): ujson.Obj = {

    val noncesJson = upickle.default.writeJs(schnorrScheme.nonces.toVector)
    val pokJson = upickle.default.writeJs(schnorrScheme.proofOfKnowledge)
    ujson.Obj(
      PicklerKeys.attestationPublicKeyKey -> ujson.Str(
        schnorrScheme.attestationPublicKey.hex),
      PicklerKeys.oracleNoncesKey -> noncesJson,
      PicklerKeys.proofOfKnowledgeKey -> pokJson
    )
  }

  private def readSchnorrScheme(obj: ujson.Obj): SchnorrAttestation = {
    val attestationPublicKeyJson = obj(PicklerKeys.attestationPublicKeyKey).str
    val attestationPublicKey =
      SchnorrPublicKey.fromHex(attestationPublicKeyJson)
    val noncesJson = obj(PicklerKeys.oracleNoncesKey).arr
    val nonces = noncesJson.toVector.map { nonceHex =>
      SchnorrNonce.fromHex(nonceHex.str)
    }

    val pokJson =
      obj(PicklerKeys.proofOfKnowledgeKey).obj
    val proofOfKnowledge =
      upickle.default.read[SchnorrProofOfKnowledge](pokJson)

    SchnorrAttestation(attestationPublicKey,
                       OrderedNonces(nonces),
                       proofOfKnowledge)
  }

  private def writeSchnorrProofOfKnowledge(
      pok: SchnorrProofOfKnowledge): ujson.Obj = {
    val signatures = pok.nonceSignature.map { sig =>
      upickle.default.writeJs(sig)
    }
    val array = ujson.Arr.from(signatures)
    val attestationPublicKeyProof =
      upickle.default.writeJs(pok.attestationPubKeySignature)
    ujson.Obj(
      PicklerKeys.attestationPublicKeyProofKey -> attestationPublicKeyProof,
      PicklerKeys.nonceProofsKey -> array
    )
  }

  private def readSchnorrProofOfKnowledge(
      obj: ujson.Obj): SchnorrProofOfKnowledge = {
    val attestationPublicKeySigJson = obj(
      PicklerKeys.attestationPublicKeyProofKey).str
    val attestationPublicKeySig =
      SchnorrDigitalSignature.fromHex(attestationPublicKeySigJson)
    val nonceSignaturesJson = obj(PicklerKeys.nonceProofsKey).arr
    val nonceSignatures = nonceSignaturesJson.value.toVector.map { json =>
      SchnorrDigitalSignature.fromHex(json.str)
    }

    SchnorrProofOfKnowledge(attestationPubKeySignature =
                              attestationPublicKeySig,
                            nonceSignature = nonceSignatures)
  }

  def writeOracleEventV0TLV(oracleEvent: OracleEventV0TLV): ujson.Obj = {
    val noncesJson: Vector[Value] = oracleEvent.nonces.toVector.map { n =>
      ujson.Str(n.hex)
    }
    val nonceArr = ujson.Arr.from(noncesJson)

    ujson.Obj(
      PicklerKeys.eventMaturityEpochKey -> ujson.Num(
        oracleEvent.eventMaturityEpoch.toLong.toDouble),
      PicklerKeys.enumEventKey -> upickle.default.writeJs(
        oracleEvent.eventDescriptor),
      PicklerKeys.eventIdKey -> ujson.Str(oracleEvent.eventId.normStr),
      PicklerKeys.oracleNoncesKey -> nonceArr
    )
  }

  def readOracleEventV0TLV(obj: ujson.Obj): OracleEventV0TLV = {
    val eventMaturityEpoch = obj(PicklerKeys.eventMaturityEpochKey).num.toInt
    val eventDescriptor =
      upickle.default.read(obj)(Picklers.eventDescriptorPickler)
    val eventId = NormalizedString(obj(PicklerKeys.eventIdKey).str)
    val nonces = obj(PicklerKeys.oracleNoncesKey).arr.toVector.map {
      case str: ujson.Str =>
        SchnorrNonce.fromHex(str.str)
      case x: ujson.Value =>
        sys.error(s"Expected json string for nonces, got=$x")
    }

    val orderedNonces = OrderedNonces(nonces)

    OracleEventV0TLV(
      nonces = orderedNonces.toVector,
      eventMaturityEpoch = UInt32(eventMaturityEpoch),
      eventDescriptor = eventDescriptor,
      eventId = eventId
    )
  }

  private def readOracleEventV1TLV(obj: ujson.Obj): OracleEventV1TLV = {
    val eventDescriptorObj = obj(PicklerKeys.eventDescriptorKey).obj
    val eventDescriptor = readEventDescriptor(eventDescriptorObj)
    val eventId = NormalizedString(obj(PicklerKeys.eventIdKey).str)

    val timestamps = upickle.default.read(obj)(Picklers.timestampPickler)
    OracleEventV1TLV(eventDescriptor, eventId, timestamps)
  }

  private def writeOracleEventV1TLV(event: OracleEventV1TLV): ujson.Obj = {
    val timestampsObj: ujson.Obj = writeTimestamps(event.timestamps)
    val descriptorObj = upickle.default.writeJs(event.eventDescriptor)(
      Picklers.eventDescriptorDLCTypePickler)
    ujson.Obj(
      PicklerKeys.timestampKey -> timestampsObj,
      PicklerKeys.eventDescriptorKey -> descriptorObj,
      PicklerKeys.eventIdKey -> ujson.Str(event.eventId.normStr)
    )
  }

  private def readEventDescriptor(obj: ujson.Obj): EventDescriptorDLCType = {
    val isEnumDescriptor = obj.value.get(PicklerKeys.enumEventKey)
    val isNumericContractDescriptor =
      obj.value.get(PicklerKeys.digitDecompositionEventKey)
    if (isEnumDescriptor.isDefined) {
      val enumDescriptorObj = isEnumDescriptor.get.obj
      readEnumEventDescriptor(enumDescriptorObj)
    } else if (isNumericContractDescriptor.isDefined) {
      val digitDecompEventObj = isNumericContractDescriptor.get.obj
      val digitDecompEvent = readDigitDecompEventDescriptor(digitDecompEventObj)
      digitDecompEvent
    } else {
      sys.error(s"Can only parse enum event descriptor so far")
    }
  }

  private def readEnumEventDescriptor(
      obj: ujson.Obj): EnumEventDescriptorDLCSubType = {
    val outcomes = obj(PicklerKeys.outcomesKey).arr.toVector.map {
      case str: ujson.Str =>
        NormalizedString(str.str)
      case x =>
        sys.error(s"Expected string for enum outcome, got=$x")
    }
    EnumEventDescriptorDLCSubType(outcomes)
  }

  private def readDigitDecompEventDescriptor(
      obj: ujson.Obj): DigitDecompositionEventDescriptorDLCType = {
    val base = UInt8(obj(PicklerKeys.baseKey).num.toInt)
    val isSigned = obj(PicklerKeys.isSignedKey).bool
    val unit = NormalizedString(obj(PicklerKeys.unitKey).str)
    val precision = Int32(obj(PicklerKeys.precisionKey).num.toInt)
    val nbDigits = UInt16(obj(PicklerKeys.nbDigitsKey).num.toInt)

    if (isSigned) {
      SignedDigitDecompositionEventDescriptorDLCType(base,
                                                     nbDigits,
                                                     unit,
                                                     precision)
    } else {
      UnsignedDigitDecompositionEventDescriptorDLCType(base,
                                                       numDigits = nbDigits,
                                                       unit = unit,
                                                       precision = precision)
    }
  }

  private def writeEventDescriptor(event: EventDescriptorDLCType): ujson.Obj = {
    event match {
      case enum: EnumEventDescriptorDLCSubType =>
        writeEnumEventDescriptor(enum)
      case numeric: NumericEventDescriptorDLCType =>
        writeNumericEventDescriptor(numeric)
    }
  }

  private def writeEnumEventDescriptor(
      event: EnumEventDescriptorDLCSubType): ujson.Obj = {
    val outcomes: Vector[ujson.Str] = event.outcomes.map { outcome =>
      ujson.Str(outcome.normStr)
    }

    val nested = ujson.Obj(
      PicklerKeys.outcomesKey -> ujson.Arr.from(outcomes)
    )

    ujson.Obj(PicklerKeys.enumEventKey -> nested)
  }

  private def writeNumericEventDescriptor(
      numeric: NumericEventDescriptorDLCType): ujson.Obj = {
    val nested = ujson.Obj(
      PicklerKeys.baseKey -> ujson.Num(numeric.base.toInt),
      PicklerKeys.isSignedKey -> ujson.Bool(numeric.isSigned),
      PicklerKeys.unitKey -> ujson.Str(numeric.unit.normStr),
      PicklerKeys.precisionKey -> ujson.Num(numeric.precision.toInt),
      PicklerKeys.nbDigitsKey -> ujson.Num(numeric.numDigits.toInt)
    )

    ujson.Obj(PicklerKeys.digitDecompositionEventKey -> nested)
  }

  private def parseContractInfo(value: ujson.Value): ContractInfoTLV = {
    val obj = value.obj
    val singleContractObjOpt = obj.value.get(PicklerKeys.singleContractInfoKey)
    val disjointContractObjOpt =
      obj.value.get(PicklerKeys.disjointContractInfoKey)
    if (singleContractObjOpt.isDefined) {
      val singleContractObj = singleContractObjOpt.get
      val contractInfoObj = singleContractObj(PicklerKeys.contractInfoKey).obj
      val totalCollateral: Satoshis = Satoshis(
        singleContractObj(PicklerKeys.totalCollateralKey).num.toLong)
      val contractDescriptorObj = contractInfoObj(
        PicklerKeys.contractDescriptorKey).obj
      val contractDescriptorTLV = parseContractDescriptor(contractDescriptorObj)
      val oracleInfoObj = contractInfoObj(PicklerKeys.oracleInfoKey).obj
      val oracleInfo = parseOracleInfo(oracleInfoObj)

      ContractInfoV0TLV(totalCollateral,
                        contractDescriptorTLV,
                        oracleInfo,
                        DLCSerializationVersion.current)
    } else if (disjointContractObjOpt.isDefined) {
      val disjointContractObj = disjointContractObjOpt.get
      val totalCollateral: Satoshis = Satoshis(
        disjointContractObj(PicklerKeys.totalCollateralKey).num.toLong)
      val contractInfosArr = disjointContractObj(
        PicklerKeys.contractInfosKey).arr
      val contractInfos = contractInfosArr.map { c =>
        val contractDescriptorObj = c(PicklerKeys.contractDescriptorKey).obj
        val descriptorTLV =
          parseContractDescriptor(contractDescriptorObj)
        val descriptor = ContractDescriptor.fromSubType(descriptorTLV)
        val oracleInfoObj = c(PicklerKeys.oracleInfoKey).obj
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
    val isEnum = obj.value.get(PicklerKeys.enumeratedContractDescriptorKey)
    val isNumeric =
      obj.value.get(PicklerKeys.numericOutcomeContractDescriptorKey)
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
      obj(PicklerKeys.payoutsKey).arr.toVector.map {
        case payoutsObj: ujson.Obj =>
          val outcome = EnumOutcome(payoutsObj(PicklerKeys.outcomeKey).str)
          val payout =
            Satoshis(payoutsObj(PicklerKeys.offerPayoutKey).num.toLong)
          (outcome, payout)
        case x: ujson.Value =>
          sys.error(s"Cannot parse payout from $x expected ujson.Obj")
      }

    EnumContractDescriptor(outcomes)
  }

  private def parseNumericContractDescriptor(
      obj: ujson.Obj): NumericContractDescriptor = {
    val numDigits = obj(PicklerKeys.nbDigitsKey).num.toInt
    val payoutFunctionObj = obj(PicklerKeys.payoutFunctionKey).obj
    val payoutFunctionPieces = payoutFunctionObj(
      PicklerKeys.payoutFunctionPiecesKey).arr
    val lastEndPoint = parseTLVPoint(
      payoutFunctionObj(PicklerKeys.lastEndpointKey).obj)
    val payoutCurve = parseDLCPayoutCurve(
      arr = payoutFunctionPieces,
      lastEndPoint = lastEndPoint
    )

    val roundingIntervals: RoundingIntervals = {
      parseRoundingIntervals(obj(PicklerKeys.roundingIntervalsKey).obj)
    }

    val descriptor =
      NumericContractDescriptor(payoutCurve,
                                numDigits = numDigits,
                                roundingIntervals)

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

    val orderedCurvePieces = OrderedDLCPayoutCurvePieces(dlcCurvePieces)

    DLCPayoutCurve(pieces = orderedCurvePieces,
                   serializationVersion = DLCSerializationVersion.current)
  }

  private def parsePayoutFunctionPiece(
      obj: ujson.Obj): (TLVPoint, PayoutCurvePieceTLV) = {
    val endPointObj =
      obj(PicklerKeys.endPointKey).obj
    val endPoint = parseTLVPoint(endPointObj)
    val curvePiece = obj(PicklerKeys.payoutCurvePieceKey).obj
    val parsedCurvePiece =
      if (curvePiece.get(PicklerKeys.polynomialPayoutCurvePieceKey).isDefined) {
        val polynomialCurvePiece = curvePiece(
          PicklerKeys.polynomialPayoutCurvePieceKey).obj
        val payoutPoints = polynomialCurvePiece(PicklerKeys.payoutPointsKey).arr
        val midPoints =
          payoutPoints.map(value => parseTLVPoint(value.obj)).toVector
        val polynomialPayoutCurvePieceTLV = PolynomialPayoutCurvePieceTLV(
          midPoints,
          DLCSerializationVersion.current)
        polynomialPayoutCurvePieceTLV
      } else if (
        curvePiece.get(PicklerKeys.hyperbolaPayoutCurvePieceKey).isDefined
      ) {
        val hyperobla = curvePiece(PicklerKeys.hyperbolaPayoutCurvePieceKey)
        val usePositivePiece = hyperobla(PicklerKeys.usePositivePieceKey).bool
        val translateOutcome = Signed16PTLVNumber.fromBigDecimal(
          hyperobla(PicklerKeys.translateOutcomeKey).num.toInt)
        val translatePayout =
          Signed16PTLVNumber.fromBigDecimal(
            hyperobla(PicklerKeys.translatePayoutKey).num)
        val a =
          Signed16PTLVNumber.fromBigDecimal(hyperobla(PicklerKeys.aKey).num)
        val b =
          Signed16PTLVNumber.fromBigDecimal(hyperobla(PicklerKeys.bKey).num)
        val c =
          Signed16PTLVNumber.fromBigDecimal(hyperobla(PicklerKeys.cKey).num)
        val d =
          Signed16PTLVNumber.fromBigDecimal(hyperobla(PicklerKeys.dKey).num)

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
    val eventOutcome = obj(PicklerKeys.eventOutcomeKey).num.toLong
    val outcomePayout = obj(PicklerKeys.outcomePayoutKey).num.toLong
    val extraPrecision = obj(PicklerKeys.extraPrecisionKey).num.toInt
    TLVPoint(outcome = eventOutcome,
             value = Satoshis(outcomePayout),
             extraPrecision = extraPrecision,
             DLCSerializationVersion.current)
  }

  private def parseRoundingIntervals(obj: ujson.Obj): RoundingIntervals = {
    val intervalsArr = obj(PicklerKeys.intervalsKey).arr
    val roundingIntervals = intervalsArr.map {
      case o: ujson.Obj =>
        val beginInterval = o(PicklerKeys.beginIntervalKey).num.toInt
        val roundingMod = o(PicklerKeys.roundingModKey).num.toInt
        val interval =
          RoundingIntervals.IntervalStart(beginInterval, roundingMod)
        interval
      case x: ujson.Value =>
        sys.error(s"Expected json object for rounding intervals, got=$x")
    }

    RoundingIntervals(roundingIntervals.toVector)
  }

  private def parseOracleInfo(obj: ujson.Obj): OracleInfoTLV = {
    val isSingle = obj.value.get(PicklerKeys.singleKey)
    val isMulti = obj.value.get(PicklerKeys.multiKey)
    if (isSingle.isDefined) {
      val singleObj = isSingle.get
      val announcementObj = singleObj(PicklerKeys.oracleAnnouncementKey).obj
      val announcement = parseOracleAnnouncement(announcementObj)
      SingleOracleInfo(announcement).toSubType
    } else if (isMulti.isDefined) {
      val multiObj = isMulti.get
      val threshold = multiObj(PicklerKeys.thresholdKey).num.toInt
      val announcementsArr = multiObj(PicklerKeys.oracleAnnouncementsKey).arr
      val announcements =
        announcementsArr.toVector.map(v => parseOracleAnnouncement(v.obj))
      val orderedAnnouncements = OrderedAnnouncements(announcements)
      if (isAllEnumAnnouncement(announcements)) {
        EnumMultiOracleInfo(threshold,
                            OrderedAnnouncements(announcements)).toSubType
      } else if (isAllNumericAnnouncement(announcements)) {
        val oracleParams = readOracleParams(
          multiObj(PicklerKeys.oracleParamsKey))
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

  private def readOracleParams(
      obj: ujson.Value): OptionDLCType[OracleParamsTLV] = {
    if (obj == ujson.Null) {
      //means we have no oracle params
      NoneDLCType
    } else {
      val maxErrorExp = obj(PicklerKeys.maxErrorExpKey).num.toInt
      val minFailExp = obj(PicklerKeys.minFailExpKey).num.toInt
      val maximizeCoverage = obj(PicklerKeys.maximizeCoverageKey).bool
      val params = OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage)
      SomeDLCType(params)
    }
  }

  private def isAllEnumAnnouncement(
      announcements: Vector[BaseOracleAnnouncement]): Boolean = {

    announcements.forall { a =>
      val eventDescriptor = a.eventTLV.eventDescriptor
      eventDescriptor.isInstanceOf[EnumEventDescriptorV0TLV] ||
      eventDescriptor.isInstanceOf[EnumEventDescriptorDLCSubType]
    }
  }

  private def isAllNumericAnnouncement(
      announcements: Vector[BaseOracleAnnouncement]): Boolean = {
    announcements.forall { a =>
      val eventDescriptor = a.eventTLV.eventDescriptor
      eventDescriptor.isInstanceOf[NumericEventDescriptorTLV] ||
      eventDescriptor.isInstanceOf[NumericEventDescriptorDLCType]
    }
  }

  private def parseOracleAnnouncement(
      obj: ujson.Obj): OracleAnnouncementV1TLV = {
    upickle.default.read[OracleAnnouncementV1TLV](obj)(
      Picklers.oracleAnnouncementV1TLVPickler)
  }

  private def parseOfferTLV(obj: ujson.Obj): DLCOfferTLV = {
    val protocolVersion = obj(PicklerKeys.protocolVersionKey).num.toInt
    val contractFlags = obj(PicklerKeys.contractFlagsKey)
    val chainHash =
      DoubleSha256Digest.fromHex(obj(PicklerKeys.chainHashKey).str)
    val tempContractId =
      Sha256Digest.fromHex(obj(PicklerKeys.temporaryContractIdKey).str)
    val contractInfo = parseContractInfo(obj(PicklerKeys.contractInfoKey).obj)

    val fundingPubKey =
      ECPublicKey.fromHex(obj(PicklerKeys.fundingPubKeyKey).str)
    val payoutSpk =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.payoutSpkKey).str)
    val payoutSerialId = parseU64(obj(PicklerKeys.payoutSerialIdKey).str)

    val offerCollateral = Satoshis(
      obj(PicklerKeys.offerCollateralKey).num.toLong)

    val fundingInputs: Vector[FundingInputTLV] = {
      parseFundingInputs(obj(PicklerKeys.fundingInputsKey).arr)

    }

    val changeSpk =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.changeSpkKey).str)
    val changeSerialId = parseU64(obj(PicklerKeys.changeSerialIdKey).str)
    val fundingOutputSerialId = parseU64(
      obj(PicklerKeys.fundOutputSerialIdKey).str)

    val feeRatePerVb = SatoshisPerVirtualByte
      .fromLong(obj(PicklerKeys.feeRatePerKbKey).num.toLong)
    val cetLocktime = obj(PicklerKeys.cetLocktimeKey).num.toLong
    val refundLocktime = obj(PicklerKeys.refundLocktimeKey).num.toLong

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

    offerTLV
  }

  private def parseAcceptTLV(obj: ujson.Obj): DLCAcceptTLV = {
    val protocolVersion = obj(PicklerKeys.protocolVersionKey).num.toInt
    val tempContractId =
      Sha256Digest.fromHex(obj(PicklerKeys.temporaryContractIdKey).str)
    val acceptCollateral = Satoshis(
      obj(PicklerKeys.acceptCollateralKey).num.toLong)
    val fundingPubKey =
      ECPublicKey.fromHex(obj(PicklerKeys.fundingPubKeyKey).str)
    val payoutSpk =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.payoutSpkKey).str)
    val payoutSerialId = parseU64(obj(PicklerKeys.payoutSerialIdKey).str)
    val fundingInputs = parseFundingInputs(
      obj(PicklerKeys.fundingInputsKey).arr)
    val changeSpk =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.changeSpkKey).str)
    val changeSerialId = parseU64(obj(PicklerKeys.changeSerialIdKey).str)
    val cetAdaptorSigs = parseCetAdaptorSignatures(
      obj(PicklerKeys.cetAdaptorSignaturesKey).obj)
    val refundSignature =
      ECDigitalSignature.fromHex(obj(PicklerKeys.refundSignatureKey).str)

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

    acceptTLV

  }

  private def parseSignTLV(obj: ujson.Obj): DLCSignTLV = {
    val protocolVersion = obj(PicklerKeys.protocolVersionKey).num.toInt
    val contractId =
      ByteVector.fromValidHex(obj(PicklerKeys.contractIdKey).str)
    val adaptorSigs = parseCetAdaptorSignatures(
      obj(PicklerKeys.cetAdaptorSignaturesKey).obj)
    val refundSignature =
      ECDigitalSignature.fromHex(obj(PicklerKeys.refundSignatureKey).str)
    val fundingSignatures = parseFundingSignatures(
      obj(PicklerKeys.fundingSignaturesKey).obj)

    val signTLV =
      DLCSignTLV(Some(protocolVersion),
                 contractId,
                 adaptorSigs,
                 refundSignature,
                 fundingSignatures)

    signTLV
  }

  private def readTimestamps(obj: ujson.Obj): OracleEventTimestamp = {
    val timestampObj = obj(PicklerKeys.timestampKey)

    if (
      timestampObj.obj.get(PicklerKeys.fixedOracleEventTimestampKey).isDefined
    ) {
      val epoch = timestampObj(PicklerKeys.fixedOracleEventTimestampKey)
        .obj(PicklerKeys.expectedTimeEpochKey)
      FixedOracleEventTimestamp(UInt32(epoch.num.toInt))
    } else {
      sys.error(s"Unknown timestamp, obj=$obj")
    }
  }

  private def writeTimestamps(timestamps: OracleEventTimestamp): ujson.Obj = {
    val nestedJson: ujson.Obj = timestamps match {
      case fixed: FixedOracleEventTimestamp =>
        ujson.Obj(
          PicklerKeys.fixedOracleEventTimestampKey ->
            ujson.Obj(
              PicklerKeys.expectedTimeEpochKey -> ujson.Num(
                fixed.maturationTime.toLong.toDouble)
            )
        )
      case _: RangeOracleEventTimestamp =>
        ???
    }

    nestedJson
  }

  private def readSchnorrAttestation(obj: ujson.Obj): SchnorrAttestationTLV = {
    val msgObj = obj(PicklerKeys.messageKey).obj
    val eventId = NormalizedString(msgObj(PicklerKeys.eventIdKey).str)
    val oraclePublicKey =
      SchnorrPublicKey.fromHex(msgObj(PicklerKeys.oraclePublicKeyKey).str)
    val signatures =
      msgObj(PicklerKeys.signaturesKey).arr.value.toVector.map {
        case str: ujson.Str =>
          SchnorrDigitalSignature.fromHex(str.str)
        case x @ (_: ujson.Obj | _: ujson.Num | ujson.Null | _: ujson.Arr |
            _: ujson.Bool) =>
          sys.error(s"signatures must be strings, got=$x")
      }
    val outcomes = msgObj(PicklerKeys.outcomesKey).arr.toVector.map {
      case str: ujson.Str =>
        NormalizedString(str.str)
      case x =>
        sys.error(s"Expected string for enum outcome, got=$x")
    }

    SchnorrAttestationTLV(eventId,
                          oraclePublicKey,
                          sigs = OrderedSchnorrSignatures(signatures),
                          outcomes = outcomes)
  }

  private def writeSchnorrAttestation(
      attestment: SchnorrAttestationTLV): ujson.Obj = {
    val nestedObj = ujson.Obj(
      PicklerKeys.eventIdKey -> ujson.Str(attestment.eventId.normStr),
      PicklerKeys.oraclePublicKeyKey -> ujson.Str(attestment.publicKey.hex),
      PicklerKeys.signaturesKey -> ujson.Arr.from(attestment.sigs.map(s =>
        ujson.Str(s.hex))),
      PicklerKeys.outcomesKey -> ujson.Arr.from(attestment.outcomes.map(o =>
        ujson.Str(o.normStr)))
    )

    val msgObj = ujson.Obj(
      PicklerKeys.messageKey -> nestedObj,
      PicklerKeys.serializedKey -> ujson.Str(LnMessage(attestment).hex)
    )

    msgObj
  }

}
