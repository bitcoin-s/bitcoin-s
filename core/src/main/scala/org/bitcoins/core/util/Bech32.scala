package org.bitcoins.core.util

import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.protocol.BtcHumanReadablePart
import org.bitcoins.core.protocol.ln.LnHumanReadablePart
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * A abstract class representing basic utility functions of Bech32
  * For more information on Bech32 please seee BIP173
  * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
  */
sealed abstract class Bech32 {

  private val generators: Vector[Long] =
    Vector(0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3)

  /**
    * Creates a checksum for the given byte vector according to
    * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 BIP173]]
    */
  def createChecksum(u5s: Vector[UInt5]): Vector[UInt5] = {
    val z = UInt5.zero
    val polymod: Long = polyMod(u5s ++ Array(z, z, z, z, z, z)) ^ 1
    //[(polymod >> 5 * (5 - i)) & 31 for i in range(6)]

    val result: Vector[UInt5] = 0
      .until(6)
      .map { i =>
        //((polymod >> five * (five - u)) & UInt8(31.toShort))
        UInt5((polymod >> 5 * (5 - i)) & 31)
      }
      .toVector
    result
  }

  /**
    * Expands the human readable part of a bech32 address as per
    * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 BIP173]]
    */
  def hrpExpand(hrp: Bech32HumanReadablePart): Vector[UInt5] = {
    val lowerchars = hrp.chars.toLowerCase

    val x: Vector[UInt5] = lowerchars.map { c =>
      UInt5(c >> 5)
    }.toVector

    val y: Vector[UInt5] = lowerchars.map { c =>
      UInt5(c & 0x1f)
    }.toVector

    x ++ (UInt5.zero +: y)
  }

  def polyMod(bytes: Vector[UInt5]): Long = {
    var chk: Long = 1
    bytes.foreach { v =>
      val b = chk >> 25
      //chk = (chk & 0x1ffffff) << 5 ^ v
      chk = (chk & 0x1ffffff) << 5 ^ v.toLong
      0.until(5).foreach { i: Int =>
        //chk ^= GEN[i] if ((b >> i) & 1) else 0
        if (((b >> i) & 1) == 1) {
          chk = chk ^ generators(i)
        }
      }
    }
    chk
  }

  /** Checks if the possible human readable part follows
    * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 BIP173]]
    * rules */
  def checkHrpValidity(hrp: String): Try[Bech32HumanReadablePart] = {
    @tailrec
    def loop(
        remaining: List[Char],
        accum: List[Char],
        isLower: Boolean,
        isUpper: Boolean): Try[Seq[Char]] = remaining match {
      case h :: t =>
        if (!isInHrpRange(h)) {
          Failure(
            new IllegalArgumentException(
              "Invalid character range for hrp, got: " + hrp))
        } else if (isLower && isUpper) {
          Failure(
            new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          loop(t, h +: accum, h.isLower || isLower, h.isUpper || isUpper)
        }
      case Nil =>
        if (isLower && isUpper) {
          Failure(
            new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          Success(accum.reverse)
        }
    }

    val hrpT =
      loop(hrp.toCharArray.toList, Nil, isLower = false, isUpper = false)

    hrpT.flatMap { chars =>
      val str = chars.mkString
      val lnT = LnHumanReadablePart(str)
      val btcT = BtcHumanReadablePart(str)

      lnT
        .orElse(btcT)
        .orElse(Failure(new IllegalArgumentException(
          s"Could not construct valid LN or BTC HRP from $str ")))
    }
  }

  def isInHrpRange(char: Char): Boolean = char >= 33 && char <= 126

  /**
    * Takes in the data portion of a bech32 address and decodes it to a byte array
    * It also checks the validity of the data portion according to BIP173
    */
  def checkDataValidity(data: String): Try[Vector[UInt5]] = {
    @tailrec
    def loop(
        remaining: List[Char],
        accum: Vector[UInt5],
        hasUpper: Boolean,
        hasLower: Boolean): Try[Vector[UInt5]] = remaining match {
      case Nil => Success(accum.reverse)
      case h :: t =>
        if (!Bech32.charset.contains(h.toLower)) {
          Failure(
            new IllegalArgumentException(
              "Invalid character in data of bech32 address, got: " + h))
        } else {
          if ((h.isUpper && hasLower) || (h.isLower && hasUpper)) {
            Failure(
              new IllegalArgumentException(
                "Cannot have mixed case for bech32 address"))
          } else {
            val byte = Bech32.charset.indexOf(h.toLower).toByte

            loop(remaining = t,
                 accum = UInt5.fromByte(byte) +: accum,
                 hasUpper = h.isUpper || hasUpper,
                 hasLower = h.isLower || hasLower)
          }
        }
    }
    val payload: Try[Vector[UInt5]] = loop(data.toCharArray.toList,
                                           Vector.empty,
                                           hasUpper = false,
                                           hasLower = false)

    payload
  }

  /**
    * Converts a byte vector to 5bit vector
    * and then serializes to bech32
    */
  def encode8bitToString(bytes: ByteVector): String = {
    val vec = UInt8.toUInt8s(bytes)
    encode8bitToString(vec)
  }

  /**
    * Converts a byte vector to 5bit vector
    * and then serializes to bech32
    */
  def encode8bitToString(bytes: Vector[UInt8]): String = {
    val b = from8bitTo5bit(bytes)
    encode5bitToString(b)
  }

  /** Takes a bech32 5bit array and encodes it to a string */
  def encode5bitToString(b: Vector[UInt5]): String = {
    b.map(b => Bech32.charset(b.toInt)).mkString
  }

  /** Converts a byte vector from 8bits to 5bits */
  def from8bitTo5bit(bytes: ByteVector): Vector[UInt5] = {
    val u8s = UInt8.toUInt8s(bytes)
    val u5s = NumberUtil.convertUInt8sToUInt5s(u8s)
    u5s
  }

  /** Converts a byte array from 8bits to base 5 bits */
  def from8bitTo5bit(u8s: Vector[UInt8]): Vector[UInt5] = {
    val bytes = UInt8.toBytes(u8s)
    from8bitTo5bit(bytes)
  }

  /** Decodes a byte array from 5bits to base 8bits */
  def from5bitTo8bit(b: Vector[UInt5], pad: Boolean = false): Vector[UInt8] = {
    NumberUtil.convertUInt5sToUInt8(b, pad)
  }

  /**
    * Validate a Bech32 string, and determine HRP and data.
    * Fails if HRP is not LN or BTC compatible.
    *
    * @see Mimics
    *      [[https://github.com/sipa/bech32/blob/master/ref/python/segwit_addr.py#L62 this function]]
    *      by Sipa
    */
  def splitToHrpAndData(
      bech32: String): Try[(Bech32HumanReadablePart, Vector[UInt5])] = {
    val sepIndexes = bech32.zipWithIndex.filter {
      case (sep, _) => sep == Bech32.separator
    }

    val length = bech32.length
    val maxLength =
      // is this a LN invoice or not?
      if (bech32.startsWith("ln"))
        // BOLT 11 is not fully bech32 compatible
        // https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#requirements
        Integer.MAX_VALUE
      else
        90

    if (length > maxLength || length < 8) {
      Failure(
        new IllegalArgumentException(
          "Bech32 payloads must be between 8 and 90 chars, got: " + length))
    } else if (sepIndexes.isEmpty) {
      Failure(
        new IllegalArgumentException(
          "Bech32 payload did not have the correct separator"))
    } else {
      val (_, sepIndex) = sepIndexes.last
      val hrpStr = bech32.take(sepIndex)
      val (_, dataStr) = bech32.splitAt(sepIndex + 1)

      if (hrpStr.length < 1) {
        Failure(new IllegalArgumentException("HRP too short"))
      } else if (dataStr.length < 6) {
        Failure(new IllegalArgumentException("Hrp/data too short"))
      } else {
        for {
          hrp <- checkHrpValidity(hrpStr)
          dataWithCheck <- Bech32.checkDataValidity(dataStr)
          dataNoCheck <- {
            if (verifyChecksum(hrp, dataWithCheck)) {
              Success(dataWithCheck.take(dataWithCheck.size - 6))
            } else
              Failure(
                new IllegalArgumentException(
                  s"Checksum was invalid on bech32 string $bech32"))
          }
        } yield (hrp, dataNoCheck)
      }
    }
  }

  def verifyChecksum(hrp: Bech32HumanReadablePart, u5s: Seq[UInt5]): Boolean = {
    val expandedHrp = hrpExpand(hrp)
    val data = expandedHrp ++ u5s
    val checksum = Bech32.polyMod(data)
    checksum == 1
  }

  /** Assumes we are given a valid bech32 string */
  def decodeStringToU5s(str: String): Vector[UInt5] = {
    str
      .map(_.toLower)
      .map { char =>
        val index = Bech32.charset.indexOf(char)
        require(index >= 0,
                s"$char (${char.toInt}) is not part of the Bech32 charset!")
        UInt5(index)
      }
      .toVector
  }
}

object Bech32 extends Bech32 {

  /** Separator used to separate the hrp & data parts of a bech32 addr */
  val separator = '1'

  /*
   * See [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 BIP173]]
   * for more
   */
  val charset: Vector[Char] = Vector('q', 'p', 'z', 'r', 'y', '9', 'x', '8',
    'g', 'f', '2', 't', 'v', 'd', 'w', '0', 's', '3', 'j', 'n', '5', '4', 'k',
    'h', 'c', 'e', '6', 'm', 'u', 'a', '7', 'l')
}

abstract class Bech32HumanReadablePart {
  require(chars.forall(Bech32.isInHrpRange),
          s"Some characters in $chars were not in valid HRP range ([33-126])")

  def chars: String

  /** Expands this HRP into a vector of UInt5s, in accordance with the Bech32 spec */
  def expand: Vector[UInt5] = Bech32.hrpExpand(this)
}
