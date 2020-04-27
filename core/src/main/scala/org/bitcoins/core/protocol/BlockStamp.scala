package org.bitcoins.core.protocol

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, TemporalAccessor}

import org.bitcoins.core.number.UInt32
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.util.{Failure, Try}

/** This trait represents a point on blockchain, and is used to specify block ranges */
sealed trait BlockStamp {
  def mkString: String
}

object BlockStamp {
  val height0 = BlockHeight(0)
  val height0Opt = Some(height0)

  case class InvalidBlockStamp(blockStamp: String)
      extends RuntimeException(s"Invalid blockstamp: ${blockStamp}")

  case class BlockHash(hash: DoubleSha256DigestBE) extends BlockStamp {
    override def mkString: String = hash.hex
  }
  case class BlockHeight(height: Int) extends BlockStamp {
    require(height >= 0, "block height must be a positive number")
    override def mkString: String = height.toString
  }
  case class BlockTime(time: UInt32) extends BlockStamp {
    override def mkString: String = {
      val instant = Instant.ofEpochSecond(time.toLong)
      DateTimeFormatter.ISO_INSTANT.format(instant)
    }
  }

  object BlockTime {

    def apply(temporalAccessor: TemporalAccessor): BlockTime = {
      val seconds = temporalAccessor.getLong(ChronoField.INSTANT_SECONDS)
      val time = UInt32(seconds)
      new BlockTime(time)
    }

  }

  def fromString(s: String): Try[BlockStamp] = {
    lazy val blockHash = Try(DoubleSha256DigestBE.fromHex(s)).map(BlockHash(_))

    lazy val blockHeight = Try(s.toInt).map(BlockHeight(_))

    lazy val blockTime =
      Try(DateTimeFormatter.ISO_INSTANT.parse(s)).map(BlockTime(_))

    lazy val error = Failure(InvalidBlockStamp(s))

    blockHash orElse blockHeight orElse blockTime orElse error
  }
}
