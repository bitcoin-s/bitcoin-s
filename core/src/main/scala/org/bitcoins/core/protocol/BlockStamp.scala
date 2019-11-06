package org.bitcoins.core.protocol

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.number.UInt32

import scala.util.{Failure, Try}

/** This trait represents a point on blockchain, and is used to specify block ranges */
sealed trait BlockStamp {
  def mkString: String
}

object BlockStamp {
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
    // TODO implement me
    override def mkString: String = ???
  }

  def fromString(s: String): Try[BlockStamp] = {
    val blockHeight = Try(s.toInt).map(BlockHeight(_))

    lazy val blockHash = Try(DoubleSha256DigestBE.fromHex(s)).map(BlockHash(_))

    lazy val error = Failure(InvalidBlockStamp(s))

    // TODO implement BlockTime parser

    blockHeight orElse blockHash orElse error
  }
}
