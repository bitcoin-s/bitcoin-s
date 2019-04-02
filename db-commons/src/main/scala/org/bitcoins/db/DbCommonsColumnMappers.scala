package org.bitcoins.db

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import slick.jdbc.SQLiteProfile.api._

abstract class DbCommonsColumnMappers {

  /** Responsible for mapping a [[DoubleSha256Digest]] to a String, and vice versa */
  implicit val doubleSha256DigestMapper: BaseColumnType[DoubleSha256Digest] =
    MappedColumnType.base[DoubleSha256Digest, String](
      _.hex,
      DoubleSha256Digest(_)
    )

  implicit val doubleSha256DigestBEMapper: BaseColumnType[
    DoubleSha256DigestBE] =
    MappedColumnType.base[DoubleSha256DigestBE, String](
      _.hex,
      DoubleSha256DigestBE.fromHex
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

  implicit val uint64Mapper: BaseColumnType[UInt64] = {
    MappedColumnType.base[UInt64, BigDecimal](
      { u64: UInt64 =>
        BigDecimal(u64.toBigInt.bigInteger)
      },
      //this has the potential to throw
      { bigDec: BigDecimal =>
        UInt64(bigDec.toBigIntExact().get)
      }
    )
  }
}

object DbCommonsColumnMappers extends DbCommonsColumnMappers
