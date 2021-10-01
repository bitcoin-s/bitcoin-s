package org.bitcoins.oracle.server

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.{AesPassword, StringFactory}
import ujson._

import java.time.Instant
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

case class SignMessage(message: String)

object SignMessage extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignMessage] = {
    jsArr.arr.toList match {
      case strJs :: Nil =>
        Try {
          val message = strJs.str

          SignMessage(message)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing message argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}

case class CreateAnnouncement(
    label: String,
    maturationTime: Instant,
    outcomes: Vector[String])

object CreateAnnouncement extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateAnnouncement] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: outcomesJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime = jsISOtoInstant(maturationTimeJs)
          val outcomes = outcomesJs.arr.map(_.str).toVector

          CreateAnnouncement(label, maturationTime, outcomes)
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

case class CreateNumericAnnouncement(
    eventName: String,
    maturationTime: Instant,
    minValue: Long,
    maxValue: Long,
    unit: String,
    precision: Int)

object CreateNumericAnnouncement extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateNumericAnnouncement] = {
    jsArr.arr.toList match {
      case labelJs :: maturationTimeJs :: minJs :: maxJs :: unitJs :: precisionJs :: Nil =>
        Try {
          val label = labelJs.str
          val maturationTime = jsISOtoInstant(maturationTimeJs)
          val minValue = minJs.num.toLong
          val maxValue = maxJs.num.toLong
          val unit = unitJs.str
          val precision = precisionJs.num.toInt

          CreateNumericAnnouncement(label,
                                    maturationTime,
                                    minValue,
                                    maxValue,
                                    unit,
                                    precision)
        }
      case Nil =>
        Failure(new IllegalArgumentException(
          "Missing label, maturationTime, minValue, maxValue, units, and precision arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 6"))
    }
  }
}

case class CreateDigitDecompAnnouncement(
    eventName: String,
    maturationTime: Instant,
    base: Int,
    isSigned: Boolean,
    numDigits: Int,
    unit: String,
    precision: Int)

object CreateDigitDecompAnnouncement extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[CreateDigitDecompAnnouncement] = {
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

          CreateDigitDecompAnnouncement(label,
                                        maturationTime,
                                        base,
                                        isSigned,
                                        numDigits,
                                        unit,
                                        precision)
        }
      case Nil =>
        Failure(new IllegalArgumentException(
          "Missing label, maturationTime, base, isSigned, numDigits, units, and precision arguments"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 7"))
    }
  }
}

case class SignEnum(eventName: String, outcome: String)

object SignEnum extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignEnum] = {
    jsArr.arr.toList match {
      case nameJs :: outcomeJs :: Nil =>
        Try {
          val outcome = outcomeJs.str

          SignEnum(nameJs.str, outcome)
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

case class SignDigits(eventName: String, num: Long)

object SignDigits extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[SignDigits] = {
    jsArr.arr.toList match {
      case nameJs :: numJs :: Nil =>
        Try {
          val num = numJs match {
            case num: Num => num.value
            case str: Str => str.value.toDouble
            case _: Value =>
              throw new IllegalArgumentException(
                s"Unable to parse $numJs as a number")
          }

          SignDigits(nameJs.str, num.toLong)
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

case class GetAnnouncement(eventName: String)

object GetAnnouncement extends ServerJsonModels {

  def fromJsArr(jsArr: ujson.Arr): Try[GetAnnouncement] = {
    require(jsArr.arr.size == 1,
            s"Bad number of arguments: ${jsArr.arr.size}. Expected: 1")
    Try {
      GetAnnouncement(jsArr.arr.head.str)
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

case class DeleteAnnouncement(eventName: String)

object DeleteAnnouncement
    extends ServerJsonModels
    with StringFactory[DeleteAnnouncement] {

  def fromJsArray(jsArr: ujson.Arr): Try[DeleteAnnouncement] = {
    jsArr.arr.toVector match {
      case eventName +: Vector() =>
        Try {
          DeleteAnnouncement.fromString(eventName.str)
        }
      case Vector() =>
        Failure(new IllegalArgumentException(s"Missing event name argument"))
      case other =>
        Failure(new IllegalArgumentException(
          s"Bad number of arguments to deleteannouncement, got=${other.length} expected: 1"))
    }
  }

  override def fromString(string: String): DeleteAnnouncement = {
    DeleteAnnouncement(string)
  }
}

case class DeleteAttestation(eventName: String)

object DeleteAttestation
    extends ServerJsonModels
    with StringFactory[DeleteAttestation] {

  def fromJsArry(jsArr: ujson.Arr): Try[DeleteAttestation] = {
    jsArr.arr.toVector match {
      case eventName +: Vector() =>
        Try {
          DeleteAttestation.fromString(eventName.str)
        }
      case Vector() =>
        Failure(new IllegalArgumentException("Missing event name argument"))
      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }

  override def fromString(string: String): DeleteAttestation = {
    DeleteAttestation(string)
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

  def jsISOtoInstant(js: Value): Instant = {
    try {
      js match {
        case Str(str) =>
          val date = TimeUtil.iso8601ToDate(str)
          date.toInstant
        case Null | Obj(_) | Arr(_) | _: Bool | _: Num =>
          throw new Exception
      }
    } catch {
      case NonFatal(_) =>
        throw Value.InvalidData(js, "Expected a date given in ISO 8601 format")
    }
  }

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }
}
