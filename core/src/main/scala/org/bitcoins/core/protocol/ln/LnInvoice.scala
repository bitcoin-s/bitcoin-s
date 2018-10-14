package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.{ UInt5, UInt64 }
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.util._
import org.slf4j.LoggerFactory

import scala.util.{ Failure, Success, Try }

sealed abstract class LnInvoice {
  require(
    timestamp < UInt64(NumberUtil.pow2(35)),
    s"timestamp ${timestamp.toBigInt} < ${NumberUtil.pow2(35)}")

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  val bech32Separator: Char = Bech32.separator

  def hrp: LnHumanReadablePart

  private def data: Vector[UInt5] = {
    val u5s: Vector[UInt5] = bech32TimeStamp ++ lnTags.data ++ signature.data
    u5s
  }

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def timestamp: UInt64

  def lnTags: LnTaggedFields

  def signature: LnInvoiceSignature

  def bech32Checksum: String = {
    val bytes: Vector[UInt5] = LnInvoice.createChecksum(hrp, data)
    val bech32 = Bech32.encode5bitToString(bytes)
    bech32
  }

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Vector[UInt5] = {
    var numNoPadding = LnUtil.encodeNumber(input.toBigInt)

    while (numNoPadding.length < 7) {
      numNoPadding = UInt5.zero +: numNoPadding
    }

    require(numNoPadding.length == 7)
    numNoPadding
  }

  private def bech32TimeStamp: Vector[UInt5] = {
    val tsB32 = uInt64ToBase32(timestamp)
    tsB32
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(hrp.toString)
    b.append(bech32Separator)

    val dataToString = Bech32.encode5bitToString(data)
    b.append(dataToString)
    b.append(bech32Checksum)

    b.toString()
  }
}

object LnInvoice {
  private case class LnInvoiceImpl(
    hrp: LnHumanReadablePart,
    timestamp: UInt64,
    lnTags: LnTaggedFields,
    signature: LnInvoiceSignature) extends LnInvoice

  private val logger = LoggerFactory.getLogger(getClass.getSimpleName)

  def decodeTimestamp(u5s: Vector[UInt5]): UInt64 = {
    val decoded = LnUtil.decodeNumber(u5s)
    UInt64(decoded)
  }

  def hrpExpand(lnHumanReadablePart: LnHumanReadablePart): Vector[UInt5] = {
    val bytes = lnHumanReadablePart.bytes
    val u5s = Bech32.hrpExpand(bytes)
    u5s
  }

  def createChecksum(hrp: LnHumanReadablePart, data: Vector[UInt5]): Vector[UInt5] = {
    val hrpBytes = hrpExpand(hrp)
    val u5s = Bech32.createChecksum(hrpBytes ++ data)
    u5s
  }

  def verifyChecksum(hrp: LnHumanReadablePart, u5s: Seq[UInt5]): Boolean = {
    val data = hrpExpand(hrp) ++ u5s
    val checksum = Bech32.polyMod(data)
    checksum == 1
  }

  def apply(hrp: LnHumanReadablePart, data: Vector[UInt5]): LnInvoice = {

    //https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#data-part

    //first 35 bits is time stamp
    val timestampU5s = data.take(7)

    val timestamp = decodeTimestamp(timestampU5s)

    //last bits should be a 520 bit signature
    //should be 104 5 bit increments (104 * 5 = 520)
    val signatureU5s = data.takeRight(104)
    val signature = LnInvoiceSignature.fromU5s(signatureU5s)

    val tags = data.slice(7, data.length - 104)

    val taggedFields = LnTaggedFields.fromUInt5s(tags)

    LnInvoice(
      hrp = hrp,
      timestamp = timestamp,
      lnTags = taggedFields,
      signature = signature)
  }

  def fromString(bech32String: String): Try[LnInvoice] = {
    val sepIndexes = {
      bech32String.zipWithIndex.filter(_._1 == Bech32.separator)
    }
    if (sepIndexes.isEmpty) {
      Failure(new IllegalArgumentException("LnInvoice did not have the correct separator"))
    } else {
      val sepIndex = sepIndexes.last._2
      val (hrp, data) = (bech32String.take(sepIndex), bech32String.splitAt(sepIndex + 1)._2)
      if (hrp.size < 1 || data.size < 6) {
        Failure(new IllegalArgumentException("Hrp/data too short"))
      } else {

        val hrpValid = LnHumanReadablePart.fromString(hrp)

        //is checksum returned here?
        val dataValid = Bech32.checkDataValidity(data)

        val isChecksumValid: Try[Vector[UInt5]] = hrpValid.flatMap { h: LnHumanReadablePart =>
          dataValid.flatMap { d: Vector[UInt5] =>
            if (verifyChecksum(h, d)) {
              if (d.size < 6) Success(Vector.empty)
              else Success(d.take(d.size - 6))
            } else Failure(new IllegalArgumentException("Checksum was invalid on the LnInvoice"))
          }
        }

        isChecksumValid.flatMap { d: Vector[UInt5] =>
          hrpValid.map(h => LnInvoice(h, d))
        }
      }
    }
  }

  def apply(hrp: LnHumanReadablePart, timestamp: UInt64, lnTags: LnTaggedFields,
    signature: LnInvoiceSignature): LnInvoice = {
    LnInvoiceImpl(
      hrp = hrp,
      timestamp = timestamp,
      lnTags = lnTags,
      signature = signature)
  }
}

