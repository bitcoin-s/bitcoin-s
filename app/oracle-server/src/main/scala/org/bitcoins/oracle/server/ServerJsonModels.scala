package org.bitcoins.oracle.server

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto.AesPassword
import ujson._

import java.time.Instant
import scala.util.{Failure, Try}

case class CreateEvent(
    label: String,
    maturationTime: Instant,
    outcomes: Vector[String])

object CreateEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateEvent] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: outcomesJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime: Instant =
            Instant.ofEpochSecond(maturationTimeJs.num.toLong)
          val outcomes = outcomesJs.arr.map(_.str).toVector

          CreateEvent(label, maturationTime, outcomes)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException("Missing label and outcome arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class CreateDigitDecompEvent(
    eventName: String,
    maturationTime: Instant,
    base: Int,
    isSigned: Boolean,
    numDigits: Int,
    unit: String,
    precision: Int)

object CreateDigitDecompEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDigitDecompEvent] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: baseJs :: isSignedJs :: numDigitsJs :: unitJs :: precisionJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime: Instant =
            Instant.ofEpochSecond(maturationTimeJs.num.toLong)
          val base = baseJs.num.toInt
          val isSigned = isSignedJs.bool
          val numDigits = numDigitsJs.num.toInt
          val unit = unitJs.str
          val precision = precisionJs.num.toInt

          CreateDigitDecompEvent(label,
                                 maturationTime,
                                 base,
                                 isSigned,
                                 numDigits,
                                 unit,
                                 precision)
        }
      case Nil =>
        Failure(new IllegalArgumentException(
          "Missing label, maturationTime, base, isSigned, and numDigits arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 5"))
    }
  }
}

case class SignEvent(
    oracleAnnouncementTLV: OracleAnnouncementTLV,
    outcome: String)

object SignEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignEvent] = {
    jsArr.arr.toList match {
      case tlvJs :: outcomeJs :: Nil =>
        Try {
          val oracleAnnouncementTLV = OracleAnnouncementTLV(tlvJs.str)
          val outcome = outcomeJs.str

          SignEvent(oracleAnnouncementTLV, outcome)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing oracle event tlv and outcome arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class SignDigits(oracleAnnouncementTLV: OracleAnnouncementTLV, num: Long)

object SignDigits extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDigits] = {
    jsArr.arr.toList match {
      case tlvJs :: numJs :: Nil =>
        Try {
          val oracleAnnouncementTLV = OracleAnnouncementTLV(tlvJs.str)
          val num = numJs match {
            case num: Num => num.value
            case str: Str => str.value.toDouble
            case _: Value =>
              throw new IllegalArgumentException(
                s"Unable to parse $numJs as a number")
          }

          SignDigits(oracleAnnouncementTLV, num.toLong)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing oracle event tlv and num arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class GetEvent(announcementTLV: OracleAnnouncementTLV)

object GetEvent extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetEvent] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")
    Try {
      val oracleAnnouncementTLV = OracleAnnouncementTLV(jsArr.arr.head.str)

      GetEvent(oracleAnnouncementTLV)
    }
  }
}

case class KeyManagerPassphraseChange(
    oldPassword: AesPassword,
    newPassword: AesPassword)

object KeyManagerPassphraseChange extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseChange] = {
    jsArr.arr.toList match {
      case oldPassJs :: newPassJs :: Nil =>
        Try {
          val oldPass = AesPassword.fromString(oldPassJs.str)
          val newPass = AesPassword.fromString(newPassJs.str)

          KeyManagerPassphraseChange(oldPass, newPass)
        }
      case Nil =>
        Failure(
          new IllegalArgumentException(
            "Missing old password and new password arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 2"))
    }
  }
}

case class KeyManagerPassphraseSet(password: AesPassword)

object KeyManagerPassphraseSet extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[KeyManagerPassphraseSet] = {
    jsArr.arr.toList match {
      case passJs :: Nil =>
        Try {
          val pass = AesPassword.fromString(passJs.str)

          KeyManagerPassphraseSet(pass)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing password argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

trait ServerJsonModels {

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
}
