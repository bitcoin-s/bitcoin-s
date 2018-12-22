package org.bitcoins.node.models

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import slick.jdbc.PostgresProfile.api._

/**
  * Created by chris on 9/9/16.
  * These are a collection of functions to map our native bitcoin-s types to scala slick types
  * For instance, taking a [[org.bitcoins.core.crypto.DoubleSha256Digest]] and converting it
  * into a String, which s a type that Slick understands
  */
trait ColumnMappers {

  /** Responsible for mapping a [[DoubleSha256Digest]] to a String, and vice versa */
  implicit val doubleSha256DigestMapper: BaseColumnType[DoubleSha256Digest] =
    MappedColumnType.base[DoubleSha256Digest, String](
      _.hex,
      DoubleSha256Digest(_)
    )

  /** Responsible for mapping a [[UInt32]] to a long in Slick, and vice versa */
  implicit val uInt32Mapper: BaseColumnType[UInt32] =
    MappedColumnType.base[UInt32, Long](
      tmap = _.toLong,
      tcomap = UInt32(_)
    )

  implicit val int32Mapper: BaseColumnType[Int32] = {
    MappedColumnType.base[Int32, Long](tmap = _.toLong, tcomap = Int32(_))
  }

  /** Responsible for mapping a [[TransactionOutput]] to hex in Slick, and vice versa */
  implicit val transactionOutputMapper: BaseColumnType[TransactionOutput] = {
    MappedColumnType.base[TransactionOutput, String](
      _.hex,
      TransactionOutput(_)
    )
  }
}

object ColumnMappers extends ColumnMappers
