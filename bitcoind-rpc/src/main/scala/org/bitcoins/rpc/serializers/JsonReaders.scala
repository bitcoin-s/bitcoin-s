package org.bitcoins.rpc.serializers

import java.io.File
import java.net.{InetAddress, URI}
import java.time.{LocalDateTime, ZoneId, ZoneOffset}

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.{Int32, Int64, UInt32, UInt64}
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptSignature,
  WitnessVersion,
  WitnessVersion0
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{
  Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.{BitcoinFeeUnit, SatoshisPerByte}
import org.bitcoins.rpc.client.common.RpcOpts.LabelPurpose
import org.bitcoins.rpc.jsonmodels._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json._

import scala.util.{Failure, Success}
import org.bitcoins.core.config._

object JsonReaders {

  /**
    * Tries to prase the provided JSON into a map with keys of
    * type `K` and values of type `V`
    */
  def mapReads[K, V](js: JsValue)(
      implicit readsK: Reads[K],
      readsV: Reads[V]): JsResult[Map[K, V]] = {
    js.validate[JsObject].flatMap { jsObj =>
      val jsResults: Seq[(JsResult[K], JsResult[V])] = jsObj.fields.map {
        case (key, value) => JsString(key).validate[K] -> value.validate[V]
      }

      val allErrors: Seq[(JsPath, Seq[JsonValidationError])] =
        jsResults.collect {
          case (JsError(keyErrors), _)   => keyErrors
          case (_, JsError(valueErrors)) => valueErrors
        }.flatten

      if (allErrors.nonEmpty) {
        JsError(allErrors)
      } else {
        JsSuccess(jsResults.collect {
          case (JsSuccess(k, _), JsSuccess(v, _)) =>
            k -> v
        }.toMap)
      }
    }
  }
  implicit object LocalDateTimeReads extends Reads[LocalDateTime] {
    override def reads(json: JsValue): JsResult[LocalDateTime] =
      SerializerUtil.processJsNumberBigInt[LocalDateTime](bigInt =>
        LocalDateTime.ofEpochSecond(bigInt.toLong, 0, ZoneOffset.UTC))(json)
  }

  implicit object BigIntReads extends Reads[BigInt] {
    override def reads(json: JsValue): JsResult[BigInt] =
      SerializerUtil.processJsNumber[BigInt](_.toBigInt())(json)
  }

  implicit object Sha256DigestReads extends Reads[Sha256Digest] {
    override def reads(json: JsValue): JsResult[Sha256Digest] =
      SerializerUtil.processJsString[Sha256Digest](Sha256Digest.fromHex)(json)
  }

  implicit object RipeMd160DigestReads extends Reads[RipeMd160Digest] {
    override def reads(json: JsValue): JsResult[RipeMd160Digest] =
      SerializerUtil.processJsString[RipeMd160Digest](RipeMd160Digest.fromHex)(
        json)
  }

  implicit object RipeMd160DigestBEReads extends Reads[RipeMd160DigestBE] {
    override def reads(json: JsValue): JsResult[RipeMd160DigestBE] =
      SerializerUtil.processJsString[RipeMd160DigestBE](
        RipeMd160DigestBE.fromHex)(json)
  }

  implicit object DoubleSha256DigestReads extends Reads[DoubleSha256Digest] {
    override def reads(json: JsValue): JsResult[DoubleSha256Digest] =
      SerializerUtil.processJsString[DoubleSha256Digest](
        DoubleSha256Digest.fromHex)(json)
  }

  implicit object DoubleSha256DigestBEReads
      extends Reads[DoubleSha256DigestBE] {
    override def reads(json: JsValue): JsResult[DoubleSha256DigestBE] =
      SerializerUtil.processJsString[DoubleSha256DigestBE](
        DoubleSha256DigestBE.fromHex)(json)
  }

  implicit object BitcoinsReads extends Reads[Bitcoins] {
    override def reads(json: JsValue): JsResult[Bitcoins] =
      SerializerUtil.processJsNumber[Bitcoins](Bitcoins(_))(json)
  }

  implicit object SatoshisReads extends Reads[Satoshis] {
    override def reads(json: JsValue): JsResult[Satoshis] =
      SerializerUtil.processJsNumber[Satoshis](num =>
        Satoshis(Int64(num.toBigInt)))(json)
  }

  implicit object BlockHeaderReads extends Reads[BlockHeader] {
    override def reads(json: JsValue): JsResult[BlockHeader] =
      SerializerUtil.processJsString[BlockHeader](BlockHeader.fromHex)(json)
  }

  implicit object Int32Reads extends Reads[Int32] {
    override def reads(json: JsValue): JsResult[Int32] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) => JsSuccess(Int32(num))
          case None      => SerializerUtil.buildErrorMsg("Int32", n)
        }
      case JsString(s) => JsSuccess(Int32.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsnumber", err)
    }
  }

  implicit object UInt32Reads extends Reads[UInt32] {
    override def reads(json: JsValue): JsResult[UInt32] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) =>
            if (num >= 0) {
              JsSuccess(UInt32(num))
            } else {
              SerializerUtil.buildErrorMsg("positive_value", num)
            }
          case None => SerializerUtil.buildErrorMsg("UInt32", n)
        }
      case JsString(s) => JsSuccess(UInt32.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsnumber", err)
    }
  }

  implicit object UInt64Reads extends Reads[UInt64] {
    override def reads(json: JsValue): JsResult[UInt64] = json match {
      case JsNumber(n) =>
        n.toBigIntExact() match {
          case Some(num) =>
            if (num >= 0) {
              JsSuccess(UInt64(num))
            } else {
              SerializerUtil.buildErrorMsg("positive_value", num)
            }
          case None => SerializerUtil.buildErrorMsg("UInt32", n)
        }
      case JsString(s) => JsSuccess(UInt64.fromHex(s))
      case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsnumber", err)
    }
  }

  implicit object LabelPurposeReads extends Reads[LabelPurpose] {
    override def reads(json: JsValue): JsResult[LabelPurpose] =
      json match {
        case JsString("send")    => JsSuccess(LabelPurpose.Send)
        case JsString("receive") => JsSuccess(LabelPurpose.Receive)
        // TODO better error message?
        case err =>
          SerializerUtil.buildErrorMsg(expected = "send or receive", err)
      }
  }

  implicit object WitnessVersionReads extends Reads[WitnessVersion] {
    override def reads(json: JsValue): JsResult[WitnessVersion] =
      json match {
        case JsNumber(num) if num == 0 => JsSuccess(WitnessVersion0)
        case JsNumber(num) if num != 0 =>
          SerializerUtil.buildErrorMsg("Expected witness_version 0", num)
        case err =>
          SerializerUtil.buildErrorMsg("Expected numerical witness_version",
                                       err)
      }
  }

  implicit object AddressReads extends Reads[Address] {
    override def reads(json: JsValue): JsResult[Address] = json match {
      case JsString(s) =>
        Address.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            SerializerUtil.buildErrorMsg("address", err)
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsstring", err)
    }
  }

  // Errors for Unit return types are caught in RpcClient::checkUnit
  implicit object UnitReads extends Reads[Unit] {
    override def reads(json: JsValue): JsResult[Unit] = JsSuccess(())
  }

  implicit object InetAddressReads extends Reads[InetAddress] {
    override def reads(json: JsValue): JsResult[InetAddress] =
      SerializerUtil.processJsString[InetAddress](InetAddress.getByName)(json)
  }

  implicit object ECDigitalSignatureReads extends Reads[ECDigitalSignature] {
    override def reads(json: JsValue): JsResult[ECDigitalSignature] = {
      SerializerUtil.processJsString(ECDigitalSignature.fromHex)(json)
    }
  }

  implicit object ScriptPubKeyReads extends Reads[ScriptPubKey] {
    override def reads(json: JsValue): JsResult[ScriptPubKey] =
      SerializerUtil.processJsString[ScriptPubKey](ScriptPubKey.fromAsmHex)(
        json)
  }

  implicit object BlockReads extends Reads[Block] {
    override def reads(json: JsValue): JsResult[Block] =
      SerializerUtil.processJsString[Block](Block.fromHex)(json)
  }

  implicit object Sha256Hash160DigestReads extends Reads[Sha256Hash160Digest] {
    override def reads(json: JsValue): JsResult[Sha256Hash160Digest] =
      SerializerUtil.processJsString[Sha256Hash160Digest](
        Sha256Hash160Digest.fromHex)(json)
  }

  implicit object ECPublicKeyReads extends Reads[ECPublicKey] {
    override def reads(json: JsValue): JsResult[ECPublicKey] =
      SerializerUtil.processJsString[ECPublicKey](ECPublicKey.fromHex)(json)
  }

  implicit object P2PKHAddressReads extends Reads[P2PKHAddress] {
    override def reads(json: JsValue): JsResult[P2PKHAddress] = json match {
      case JsString(s) =>
        P2PKHAddress.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            SerializerUtil.buildErrorMsg("p2pkhaddress", err)
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsstring", err)
    }
  }

  implicit object P2SHAddressReads extends Reads[P2SHAddress] {
    override def reads(json: JsValue): JsResult[P2SHAddress] = json match {
      case JsString(s) =>
        P2SHAddress.fromString(s) match {
          case Success(address) => JsSuccess(address)
          case Failure(err) =>
            SerializerUtil.buildErrorMsg("p2shaddress", err)
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsstring", err)
    }
  }

  implicit object ScriptSignatureReads extends Reads[ScriptSignature] {
    override def reads(json: JsValue): JsResult[ScriptSignature] =
      SerializerUtil.processJsString[ScriptSignature](
        ScriptSignature.fromAsmHex)(json)
  }

  implicit object TransactionInputReads extends Reads[TransactionInput] {
    override def reads(json: JsValue): JsResult[TransactionInput] = {
      (json \ "sequence").validate[UInt32].flatMap { sequence =>
        (json \ "coinbase").validate[String] match {
          case s: JsSuccess[String] =>
            JsSuccess(
              CoinbaseInput(ScriptSignature.fromAsmHex(s.value), sequence))
          case _ =>
            (json \ "txid").validate[DoubleSha256DigestBE].flatMap { txid =>
              (json \ "vout").validate[UInt32].flatMap { vout =>
                (json \ "scriptSig" \ "hex")
                  .validate[ScriptSignature]
                  .flatMap { scriptSig =>
                    JsSuccess(
                      TransactionInput(TransactionOutPoint(txid.flip, vout),
                                       scriptSig,
                                       sequence))
                  }
              }
            }
        }
      }
    }
  }

  implicit object BitcoinAddressReads extends Reads[BitcoinAddress] {
    override def reads(json: JsValue): JsResult[BitcoinAddress] = json match {
      case JsString(s) =>
        BitcoinAddress.fromString(s) match {
          case Success(address) =>
            JsSuccess(address)
          case Failure(err) =>
            SerializerUtil.buildErrorMsg("address", err)
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsstring", err)
    }
  }

  implicit object MerkleBlockReads extends Reads[MerkleBlock] {
    override def reads(json: JsValue): JsResult[MerkleBlock] =
      SerializerUtil.processJsString[MerkleBlock](MerkleBlock.fromHex)(json)
  }

  implicit object TransactionReads extends Reads[Transaction] {
    override def reads(json: JsValue): JsResult[Transaction] =
      SerializerUtil.processJsString[Transaction](Transaction.fromHex)(json)
  }

  implicit object TransactionOutPointReads extends Reads[TransactionOutPoint] {
    private case class OutPoint(txid: DoubleSha256DigestBE, vout: UInt32)
    override def reads(json: JsValue): JsResult[TransactionOutPoint] = {
      implicit val outPointReads: Reads[OutPoint] = Json.reads[OutPoint]
      json.validate[OutPoint] match {
        case JsSuccess(op, _) =>
          JsSuccess(TransactionOutPoint(op.txid.flip, op.vout))
        case JsError(err) =>
          JsError(s"Could not parse TransactionOutPoint, got ${err.toString()}")
      }
    }
  }

  implicit object RpcAddressReads extends Reads[RpcAddress] {

    def reads(json: JsValue): JsResult[RpcAddress] = json match {
      case array: JsArray =>
        val bitcoinResult = array.value.find(_.isInstanceOf[JsNumber]) match {
          case Some(JsNumber(n)) => JsSuccess(Bitcoins(n))
          case Some(
              err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
              _: JsObject)) =>
            SerializerUtil.buildJsErrorMsg("jsnumber", err)
          case None => JsError("error.expected.balance")
        }
        bitcoinResult.flatMap { bitcoins =>
          val jsStrings: IndexedSeq[String] = array.value
            .filter(_.isInstanceOf[JsString])
            .map(_.asInstanceOf[JsString].value)
          val addressResult = jsStrings.find(BitcoinAddress.isValid) match {
            case Some(s) =>
              BitcoinAddress.fromString(s) match {
                case Success(a) => JsSuccess(a)
                case Failure(err) =>
                  SerializerUtil.buildErrorMsg("address", err)
              }
            case None => JsError("error.expected.address")
          }
          addressResult.flatMap { address =>
            val account = jsStrings.find(s => !BitcoinAddress.isValid(s))
            JsSuccess(RpcAddress(address, bitcoins, account))
          }
        }
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsString |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsarray", err)
    }
  }

  implicit object HashTypeReads extends Reads[HashType] {
    override def reads(json: JsValue): JsResult[HashType] =
      SerializerUtil.processJsString {
        case "ALL"                 => HashType.sigHashAll
        case "NONE"                => HashType.sigHashNone
        case "SINGLE"              => HashType.sigHashSingle
        case "ALL|ANYONECANPAY"    => HashType.sigHashAllAnyoneCanPay
        case "NONE|ANYONECANPAY"   => HashType.sigHashNoneAnyoneCanPay
        case "SINGLE|ANYONECANPAY" => HashType.sigHashSingleAnyoneCanPay
      }(json)
  }

  implicit object FinalizedPsbtReads extends Reads[FinalizedPsbt] {
    override def reads(json: JsValue): JsResult[FinalizedPsbt] =
      (json \ "complete").validate[Boolean].flatMap { completed =>
        if (completed) {
          (json \ "hex").validate[Transaction].flatMap { tx =>
            JsSuccess(FinalizedPsbt(tx))
          }
        } else {
          JsError("PSBT was not completed!")
        }
      }
  }

  implicit object NonFinalizedPsbtReads extends Reads[NonFinalizedPsbt] {
    override def reads(json: JsValue): JsResult[NonFinalizedPsbt] =
      if ((json \ "hex").isDefined) {
        JsError("PSBT was submitted as a serialized hex transaction!")
      } else {
        (json \ "psbt").validate[String].map(NonFinalizedPsbt)
      }
  }

  implicit object FinalizePsbtResultReads extends Reads[FinalizePsbtResult] {
    override def reads(json: JsValue): JsResult[FinalizePsbtResult] =
      if ((json \ "hex").isDefined) {
        json.validate[FinalizedPsbt]
      } else {
        json.validate[NonFinalizedPsbt]
      }
  }

  implicit object RpcPsbtOutputReads extends Reads[RpcPsbtOutput] {
    override def reads(json: JsValue): JsResult[RpcPsbtOutput] =
      for {
        redeemScript <- (json \ "redeem_script").validateOpt[RpcPsbtScript]
        witnessScript <- (json \ "witness_script").validateOpt[RpcPsbtScript]
        unknown <- (json \ "unknown").validateOpt[Map[String, String]]
        bip32Derivs <- (json \ "bip32_derivs")
          .validateOpt[Vector[PsbtBIP32Deriv]]
      } yield RpcPsbtOutput(redeemScript, witnessScript, bip32Derivs, unknown)
  }

  implicit object PsbtBIP32DerivsReads extends Reads[PsbtBIP32Deriv] {
    override def reads(json: JsValue): JsResult[PsbtBIP32Deriv] =
      for {
        pubkey <- (json \ "pubkey").validate[ECPublicKey]
        masterFingerprint <- (json \ "master_fingerprint").validate[String]
        path <- (json \ "path").validate[String]
      } yield
        PsbtBIP32Deriv(pubkey = pubkey,
                       masterFingerprint = masterFingerprint,
                       path = path)
  }

  implicit object RpcPsbtScriptReads extends Reads[RpcPsbtScript] {
    override def reads(json: JsValue): JsResult[RpcPsbtScript] =
      for {
        asm <- (json \ "asm").validate[String]
        hex <- (json \ "hex").validate[ScriptPubKey]
        scriptType <- (json \ "type").validateOpt[ScriptType]
        address <- (json \ "address").validateOpt[BitcoinAddress]
      } yield
        RpcPsbtScript(asm = asm,
                      hex = hex,
                      scriptType = scriptType,
                      address = address)
  }

  implicit object MapPubKeySignatureReads
      extends Reads[Map[ECPublicKey, ECDigitalSignature]] {
    override def reads(
        json: JsValue): JsResult[Map[ECPublicKey, ECDigitalSignature]] =
      JsonReaders.mapReads(json)(implicitly[Reads[ECPublicKey]],
                                 implicitly[Reads[ECDigitalSignature]])
  }

  implicit object RpcPsbtInputReads extends Reads[RpcPsbtInput] {
    override def reads(json: JsValue): JsResult[RpcPsbtInput] =
      for {
        nonWitnessUtxo <- (json \ "non_witness_utxo")
          .validateOpt[RpcTransaction]
        witnessUtxo <- (json \ "witness_utxo").validateOpt[PsbtWitnessUtxoInput]
        finalScriptSig <- (json \ "final_scriptSig").validateOpt[RpcPsbtScript]
        redeemScript <- (json \ "redeem_script").validateOpt[RpcPsbtScript]
        sighash <- (json \ "sighash").validateOpt[HashType]
        partialSignatures <- (json \ "partial_signatures")
          .validateOpt[Map[ECPublicKey, ECDigitalSignature]]
        witnessScript <- (json \ "witness_script").validateOpt[RpcPsbtScript]
        bip32Derivs <- (json \ "bi32_derivs")
          .validateOpt[Vector[PsbtBIP32Deriv]]
        finalScriptWitness <- JsSuccess(None) // todo(torkelrogstad) find an example of this
        unknown <- (json \ "unknown").validateOpt[Map[String, String]]
      } yield {
        RpcPsbtInput(
          nonWitnessUtxo = nonWitnessUtxo,
          witnessUtxo = witnessUtxo,
          partialSignatures = partialSignatures,
          sighash = sighash,
          redeemScript = redeemScript,
          witnessScript = witnessScript,
          bip32Derivs = bip32Derivs,
          finalScriptSig = finalScriptSig,
          finalScriptwitness = finalScriptWitness,
          unknown = unknown
        )
      }

  }

  implicit object ScriptTypeReads extends Reads[ScriptType] {
    override def reads(json: JsValue): JsResult[ScriptType] =
      json
        .validate[String]
        .map(ScriptType.fromStringExn)
  }

  implicit object TestMempoolAcceptResultReads
      extends Reads[TestMempoolAcceptResult] {
    override def reads(json: JsValue): JsResult[TestMempoolAcceptResult] =
      for {
        txid <- (json \ "txid").validate[DoubleSha256DigestBE]
        allowed <- (json \ "allowed").validate[Boolean]
        rejectReason <- (json \ "reject-reason").validateOpt[String]
      } yield TestMempoolAcceptResult(txid, allowed, rejectReason)
  }

  // Currently takes in BTC/kB
  implicit object BitcoinFeeUnitReads extends Reads[BitcoinFeeUnit] {
    override def reads(json: JsValue): JsResult[BitcoinFeeUnit] =
      SerializerUtil.processJsNumber[BitcoinFeeUnit](num =>
        SatoshisPerByte(Satoshis(Int64((num * 100000).toBigInt()))))(json)
  }

  implicit object FileReads extends Reads[File] {
    override def reads(json: JsValue): JsResult[File] =
      SerializerUtil.processJsString[File](new File(_))(json)
  }

  implicit object URIReads extends Reads[URI] {
    override def reads(json: JsValue): JsResult[URI] =
      SerializerUtil.processJsString[URI](str => new URI("http://" + str))(json)
  }

  implicit object NetworkParamsReads extends Reads[NetworkParameters] {

    def reads(json: JsValue): JsResult[NetworkParameters] =
      json.validate[String].map(_.toLowerCase()).map {
        case "regtest" => RegTest
        case "main"    => MainNet
        case "test"    => TestNet3
      }
  }
}
