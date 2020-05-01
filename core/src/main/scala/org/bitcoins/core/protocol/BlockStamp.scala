package org.bitcoins.core.protocol

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, TemporalAccessor}

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.{BlockTime, _}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.util.{Failure, Try}

/** This trait represents a point on blockchain, and is used to specify block ranges */
sealed trait BlockStamp {
  def mkString: String
}

/** This trait represents a point on the blockchain, including future points */
sealed trait BlockStampWithFuture extends BlockStamp {
  def toUInt32: UInt32
  def toScriptNumber: ScriptNumber
}

object BlockStamp {
  val height0 = BlockHeight(0)
  val height0Opt = Some(height0)

  case class InvalidBlockStamp(blockStamp: String)
      extends RuntimeException(s"Invalid blockstamp: ${blockStamp}")

  case class BlockHash(hash: DoubleSha256DigestBE) extends BlockStamp {
    override def mkString: String = hash.hex
  }
  case class BlockHeight(height: Int) extends BlockStampWithFuture {
    require(height >= 0, "block height must be a positive number")
    override def mkString: String = height.toString
    override def toUInt32: UInt32 = UInt32(height)
    override def toScriptNumber: ScriptNumber = ScriptNumber(height)
  }
  case class BlockTime(time: UInt32) extends BlockStampWithFuture {
    override def mkString: String = {
      val instant = Instant.ofEpochSecond(time.toLong)
      DateTimeFormatter.ISO_INSTANT.format(instant)
    }
    override def toUInt32: UInt32 = time
    override def toScriptNumber: ScriptNumber = ScriptNumber(time.toLong)
  }

  /** This is Tue Nov  5 00:53:20 1985 UTC
    * @see [[https://github.com/bitcoin/bitcoin/blob/master/src/script/script.h#L39]]
    */
  final val LOCKTIME_THRESHOLD: Int = 500000000

  def apply(timeLockNumber: Int): BlockStampWithFuture =
    BlockStampWithFuture.fromUInt32(UInt32(timeLockNumber))

  object BlockTime {

    def apply(temporalAccessor: TemporalAccessor): BlockTime = {
      val seconds = temporalAccessor.getLong(ChronoField.INSTANT_SECONDS)
      val time = UInt32(seconds)
      new BlockTime(time)
    }
  }

  def fromString(s: String): Try[BlockStamp] = {

    lazy val blockStampWithFuture = BlockStampWithFuture.fromString(s)

    lazy val blockHash = Try(DoubleSha256DigestBE(s)).map(BlockHash)

    blockHash orElse blockStampWithFuture
  }
}

object BlockStampWithFuture {

  def fromString(s: String): Try[BlockStampWithFuture] = {
    lazy val blockHeight = Try(s.toInt).map(BlockHeight)

    lazy val blockTime =
      Try(DateTimeFormatter.ISO_INSTANT.parse(s)).map(BlockTime(_))

    lazy val error = Failure(InvalidBlockStamp(s))

    blockHeight orElse blockTime orElse error
  }

  def apply(timeLockNumber: UInt32): BlockStampWithFuture =
    fromUInt32(timeLockNumber)

  def apply(timeLockNumber: Int): BlockStampWithFuture =
    fromUInt32(UInt32(timeLockNumber))

  /** @see [[https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki#detailed-specification]] */
  def fromUInt32(uInt32: UInt32): BlockStampWithFuture = {
    if (uInt32 < UInt32(LOCKTIME_THRESHOLD)) {
      BlockHeight(uInt32.toInt)
    } else {
      BlockStamp.BlockTime(uInt32)
    }
  }
}
