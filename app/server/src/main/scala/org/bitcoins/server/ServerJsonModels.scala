package org.bitcoins.server

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.serializers.JsonReaders
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.config.DLC
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import ujson._

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.Path
import scala.util._


case class DecodeAccept(accept: DLCAcceptTLV)

object DecodeAccept extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeAccept] = {
    jsArr.arr.toList match {
      case acceptJs :: Nil =>
        Try {
          val accept: LnMessage[DLCAcceptTLV] =
            LnMessageFactory(DLCAcceptTLV).fromHex(acceptJs.str)
          DecodeAccept(accept.tlv)
        } match {
          case Success(value) =>
            Success(value)
          case Failure(_) =>
            Try {
              val accept = DLCAcceptTLV.fromHex(acceptJs.str)
              DecodeAccept(accept)
            }
        }
      case Nil =>
        Failure(new IllegalArgumentException(s"Missing accept announcement"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length} Expected: 1"))
    }
  }
}

case class DecodeSign(sign: DLCSignTLV)

object DecodeSign extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeSign] = {
    jsArr.arr.toList match {
      case signJs :: Nil =>
        Try {
          val accept: LnMessage[DLCSignTLV] =
            LnMessageFactory(DLCSignTLV).fromHex(signJs.str)
          DecodeSign(accept.tlv)
        } match {
          case Success(value) =>
            Success(value)
          case Failure(_) =>
            Try {
              val sign = DLCSignTLV.fromHex(signJs.str)
              DecodeSign(sign)
            }
        }
      case Nil =>
        Failure(new IllegalArgumentException(s"Missing accept announcement"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length} Expected: 1"))
    }
  }
}

case class DecodeAttestations(announcement: OracleAttestmentV0TLV)

object DecodeAttestations extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DecodeAttestations] = {
    jsArr.arr.toList match {
      case attestmentJs :: Nil =>
        Try {
          val attestments = OracleAttestmentV0TLV(attestmentJs.str)
          DecodeAttestations(attestments)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing attestment argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class DLCDataFromFile(
    path: Path,
    destinationOpt: Option[Path],
    externalPayoutAddressOpt: Option[BitcoinAddress],
    externalChangeAddressOpt: Option[BitcoinAddress])

object DLCDataFromFile extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[DLCDataFromFile] = {
    def parseParameters(
        pathJs: Value,
        destJs: Value,
        payoutAddressJs: Value,
        changeAddressJs: Value) = Try {
      val path = new File(pathJs.str).toPath
      val destJsOpt = nullToOpt(destJs)
      val destOpt = destJsOpt.map(js => new File(js.str).toPath)
      val payoutAddressJsOpt = nullToOpt(payoutAddressJs)
      val payoutAddressOpt =
        payoutAddressJsOpt.map(js => jsToBitcoinAddress(js))
      val changeAddressJsOpt = nullToOpt(changeAddressJs)
      val changeAddressOpt =
        changeAddressJsOpt.map(js => jsToBitcoinAddress(js))

      DLCDataFromFile(path, destOpt, payoutAddressOpt, changeAddressOpt)
    }

    jsArr.arr.toList match {
      case pathJs :: Nil =>
        parseParameters(pathJs, Null, Null, Null)
      case pathJs :: destJs :: Nil =>
        parseParameters(pathJs, destJs, Null, Null)
      case pathJs :: destJs :: payoutAddressJs :: Nil =>
        parseParameters(pathJs, destJs, payoutAddressJs, Null)
      case pathJs :: destJs :: payoutAddressJs :: changeAddressJs :: Nil =>
        parseParameters(pathJs, destJs, payoutAddressJs, changeAddressJs)
      case Nil =>
        Failure(new IllegalArgumentException("Missing path argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class OfferAdd(
    offerTLV: DLCOfferTLV,
    peer: Option[String],
    message: Option[String])

object OfferAdd extends ServerJsonModels {

  def fromJsArr(arr: ujson.Arr): Try[OfferAdd] = {
    arr.arr.toList match {
      case offerJs :: peerJs :: messageJs :: Nil =>
        Try {
          val offer: LnMessage[DLCOfferTLV] =
            LnMessageFactory(DLCOfferTLV).fromHex(offerJs.str)
          val peer = nullToOpt(peerJs).map(_.str)
          val message = nullToOpt(messageJs).map(_.str)
          OfferAdd(offer.tlv, peer, message)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to offer-add, got=${other.length} expected=3")
        Failure(exn)
    }
  }
}

case class OfferRemove(hash: Sha256Digest)

object OfferRemove {

  def fromJsArr(arr: ujson.Arr): Try[OfferRemove] = {
    arr.arr.toList match {
      case hashJs :: Nil =>
        Try {
          val hash = Sha256Digest.fromHex(hashJs.str)
          OfferRemove(hash)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to offer-remove, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class OfferSend(
    remoteAddress: InetSocketAddress,
    message: String,
    offerE: Either[DLCOfferTLV, Sha256Digest])

object OfferSend extends ServerJsonModels {

  def fromJsArr(arr: ujson.Arr): Try[OfferSend] = {
    arr.arr.toList match {
      case offerJs :: peerAddressJs :: messageJs :: Nil =>
        Try {
          val peerAddress =
            jsToInetSocketAddress(peerAddressJs, DLC.DefaultPort)
          val message = messageJs.str
          val offerE =
            Try(LnMessageFactory(DLCOfferTLV).fromHex(offerJs.str).tlv)
              .orElse(Try(DLCOfferTLV.fromHex(offerJs.str))) match {
              case Success(o) => Left(o)
              case Failure(_) => Right(Sha256Digest.fromHex(offerJs.str))
            }
          OfferSend(peerAddress, message, offerE)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to offer-send, got=${other.length} expected=3")
        Failure(exn)
    }
  }
}

case class GetDLCOffer(tempContractId: Sha256Digest)

object GetDLCOffer {

  def fromJsArr(arr: ujson.Arr): Try[GetDLCOffer] = {
    arr.arr.toList match {
      case tempContractIdJs :: Nil =>
        Try {
          val tempContractId = Sha256Digest.fromHex(tempContractIdJs.str)

          GetDLCOffer(tempContractId)
        }
      case other =>
        val exn = new IllegalArgumentException(
          s"Bad number or arguments to offer-send, got=${other.length} expected=1")
        Failure(exn)
    }
  }
}

case class LoadWallet(
    walletName: Option[String],
    password: Option[AesPassword],
    bip39Password: Option[String])

object LoadWallet extends ServerJsonModels {

  def fromJsArr(arr: ujson.Arr): Try[LoadWallet] = Try {
    arr.arr.toList match {
      case _ :: _ :: bip39PasswordJs :: Nil =>
        val (walletNameOpt, passwordOpt) =
          jsToWalletNameAndPassword(arr.arr.slice(0, 1))
        LoadWallet(walletNameOpt,
                   passwordOpt,
                   nullToOpt(bip39PasswordJs).map(_.str))
      case _ :: _ :: Nil =>
        val (walletNameOpt, passwordOpt) =
          jsToWalletNameAndPassword(arr.arr.slice(0, 1))
        LoadWallet(walletNameOpt, passwordOpt, None)
      case walletNameJs :: Nil =>
        LoadWallet(jsToStringOpt(walletNameJs), None, None)
      case Nil =>
        LoadWallet(None, None, None)
      case other =>
        throw new IllegalArgumentException(
          s"Bad number of arguments: ${other.length}. Expected: 3")
    }
  }
}

trait ServerJsonModels {

  def jsToOracleAnnouncementTLV(js: Value): OracleAnnouncementTLV =
    js match {
      case str: Str =>
        OracleAnnouncementTLV(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected an OracleAnnouncementTLV as a hex string")
    }

  def jsToContractInfoTLV(js: Value): ContractInfoV0TLV =
    js match {
      case str: Str =>
        ContractInfoV0TLV(str.value)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a ContractInfo as a hex string")
    }

  def jsToSatoshisPerVirtualByteOpt(js: Value): Option[SatoshisPerVirtualByte] =
    nullToOpt(js).map {
      case str: Str =>
        SatoshisPerVirtualByte(Satoshis(str.value))
      case num: Num =>
        SatoshisPerVirtualByte(Satoshis(num.value.toLong))
      case _: Value =>
        throw Value.InvalidData(js, "Expected a fee rate in sats/vbyte")
    }

  def jsToUInt32(js: Value): UInt32 =
    js match {
      case str: Str =>
        UInt32(BigInt(str.value))
      case num: Num =>
        UInt32(num.value.toLong)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a UInt32")
    }

  def jsToSatoshis(js: Value): Satoshis = JsonReaders.jsToSatoshis(js)

  def jsToBitcoinAddress(js: Value): BitcoinAddress = {
    try {
      BitcoinAddress.fromString(js.str)
    } catch {
      case _: IllegalArgumentException =>
        throw Value.InvalidData(js, "Expected a valid address")
    }
  }

  def jsToPSBTSeq(js: Value): Seq[PSBT] = {
    js.arr.foldLeft(Seq.empty[PSBT])((seq, psbt) => seq :+ jsToPSBT(psbt))
  }

  def jsToPSBT(js: Value): PSBT = PSBT.fromString(js.str)

  def jsToTransactionOutPointSeq(js: Value): Seq[TransactionOutPoint] = {
    js.arr.foldLeft(Seq.empty[TransactionOutPoint])((seq, outPoint) =>
      seq :+ jsToTransactionOutPoint(outPoint))
  }

  def jsToTransactionOutPoint(js: Value): TransactionOutPoint =
    TransactionOutPoint(js.str)

  def jsToLockUnspentOutputParameter(js: Value): LockUnspentOutputParameter =
    LockUnspentOutputParameter.fromJson(js)

  def jsToLockUnspentOutputParameters(
      js: Value): Seq[LockUnspentOutputParameter] = {
    js.arr.foldLeft(Seq.empty[LockUnspentOutputParameter])((seq, outPoint) =>
      seq :+ jsToLockUnspentOutputParameter(outPoint))
  }

  def jsToCoinSelectionAlgo(js: Value): CoinSelectionAlgo =
    CoinSelectionAlgo
      .fromString(js.str)

  def jsToTx(js: Value): Transaction = Transaction.fromHex(js.str)

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }

  def jsToSchnorrDigitalSignature(js: Value): SchnorrDigitalSignature =
    js match {
      case str: Str =>
        SchnorrDigitalSignature(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a SchnorrDigitalSignature as a hex string")
    }

  def jsToSchnorrDigitalSignatureVec(
      js: Value): Vector[SchnorrDigitalSignature] = {
    js.arr.foldLeft(Vector.empty[SchnorrDigitalSignature])((vec, sig) =>
      vec :+ jsToSchnorrDigitalSignature(sig))
  }

  def jsToOracleAttestmentTLV(js: Value): OracleAttestmentTLV =
    js match {
      case str: Str =>
        OracleAttestmentTLV(str.value)
      case _: Value =>
        throw Value.InvalidData(
          js,
          "Expected a OracleAttestmentTLV as a hex string")
    }

  def jsToOracleAttestmentTLVVec(js: Value): Vector[OracleAttestmentTLV] = {
    js.arr.foldLeft(Vector.empty[OracleAttestmentTLV])((vec, tlv) =>
      vec :+ jsToOracleAttestmentTLV(tlv))
  }

  def jsToAESPassword(js: Value): Option[AesPassword] = {
    js match {
      case Str(str) =>
        Some(AesPassword.fromString(str))
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }

  def jsToStringOpt(js: Value): Option[String] = {
    js match {
      case Str(str) =>
        Some(str)
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }

  def jsToWalletNameAndPassword(
      js: Value): (Option[String], Option[AesPassword]) = {
    js match {
      case Arr(arr) =>
        if (arr.size >= 2) {
          (jsToStringOpt(arr(0)), jsToAESPassword(arr(1)))
        } else if (arr.size == 1) {
          (jsToStringOpt(arr(0)), None)
        } else { (None, None) }
      case _: Value =>
        throw new IllegalArgumentException(s"Expected json.Arr")
    }
  }

  def jsToMnemonics(js: Value): MnemonicCode = {
    val mnemonicWords = js match {
      case Str(str) => str.split(' ').toVector
      case Arr(arr) => arr.map(_.str).toVector
      case Null | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException(
          "mnemonic must be a string or array of strings")
    }
    MnemonicCode.fromWords(mnemonicWords)
  }

  def jsToInetSocketAddress(
      js: Value,
      defaultPort: Int = -1): InetSocketAddress = {
    js match {
      case str: Str =>
        val uri = new URI("tcp://" + str.str)
        val port = if (uri.getPort >= 0) uri.getPort else defaultPort
        InetSocketAddress.createUnresolved(uri.getHost, port)
      case _: Value =>
        throw Value.InvalidData(js, "Expected a host address")
    }
  }
}
