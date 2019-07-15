package org.bitcoins.db

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import scodec.bits.ByteVector
import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.core.hd.HDCoinType
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.core.hd.SegWitHDPath
import slick.jdbc.GetResult
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.Int64

abstract class DbCommonsColumnMappers {

  /**
    * If executing something like this:
    *
    * {{{
    * sql"SELECT * FROM sqlite_master where type='table'"
    * }}}
    *
    * you end up with something like this:
    * {{{
    * /-------+---------------+---------------+----------+----------------------\
    * | 1     | 2             | 3             | 4        | 5                    |
    * | type  | name          | tbl_name      | rootpage | sql                  |
    * |-------+---------------+---------------+----------+----------------------|
    * | table | block_headers | block_headers | 2        | CREATE TABLE "blo... |
    * \-------+---------------+---------------+----------+----------------------/
    * }}}
    *
    * This is most likely an implementation that will break of you try and cast
    * the result of a different raw SQL query into a
    * [[org.bitcoins.db.SQLiteTableInfo SQLiteTableInfo]].
    */
  implicit val sqliteTableInfoReader: GetResult[SQLiteTableInfo] =
    GetResult[SQLiteTableInfo] { row =>
      row.nextString() // type
      row.nextString() // name
      val tableName = row.nextString()
      row.nextString() // rootpage
      val sql = row.nextString()
      SQLiteTableInfo(tableName, sql)

    }

  implicit val doubleSha256DigestBEMapper: BaseColumnType[
    DoubleSha256DigestBE] =
    MappedColumnType.base[DoubleSha256DigestBE, String](
      _.hex,
      DoubleSha256DigestBE.fromHex
    )

  implicit val ecPublicKeyMapper: BaseColumnType[ECPublicKey] =
    MappedColumnType.base[ECPublicKey, String](_.hex, ECPublicKey.fromHex)

  implicit val sha256Hash160DigestMapper: BaseColumnType[Sha256Hash160Digest] =
    MappedColumnType
      .base[Sha256Hash160Digest, String](_.hex, Sha256Hash160Digest.fromHex)

  /** Responsible for mapping a [[org.bitcoins.core.number.UInt32 UInt32]] to a long in Slick, and vice versa */
  implicit val uInt32Mapper: BaseColumnType[UInt32] =
    MappedColumnType.base[UInt32, Long](
      tmap = _.toLong,
      tcomap = UInt32(_)
    )

  implicit val int32Mapper: BaseColumnType[Int32] = {
    MappedColumnType.base[Int32, Long](tmap = _.toLong, tcomap = Int32(_))
  }

  /** Responsible for mapping a [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]] to hex in Slick, and vice versa */
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

  implicit val transactionOutPointMapper: BaseColumnType[TransactionOutPoint] = {
    MappedColumnType
      .base[TransactionOutPoint, String](_.hex, TransactionOutPoint(_))
  }

  implicit val scriptPubKeyMapper: BaseColumnType[ScriptPubKey] = {
    MappedColumnType.base[ScriptPubKey, String](_.hex, ScriptPubKey(_))
  }

  implicit val scriptWitnessMapper: BaseColumnType[ScriptWitness] = {
    MappedColumnType
      .base[ScriptWitness, String](
        _.hex,
        hex => RawScriptWitnessParser.read(ByteVector.fromValidHex(hex)))
  }

  implicit val byteVectorMapper: BaseColumnType[ByteVector] = {
    MappedColumnType
      .base[ByteVector, String](_.toHex, ByteVector.fromValidHex(_))
  }

  implicit val xpubMapper: BaseColumnType[ExtPublicKey] = {
    MappedColumnType
      .base[ExtPublicKey, String](_.toString, ExtPublicKey.fromString(_).get)
  }

  implicit val hdCoinTypeMapper: BaseColumnType[HDCoinType] = {
    MappedColumnType.base[HDCoinType, Int](_.toInt, HDCoinType.fromInt)
  }

  implicit val hdPathMappper: BaseColumnType[HDPath] =
    MappedColumnType
      .base[HDPath, String](_.toString, HDPath.fromString(_).get) // hm rethink .get?

  implicit val segwitPathMappper: BaseColumnType[SegWitHDPath] =
    MappedColumnType
      .base[SegWitHDPath, String](_.toString, SegWitHDPath.fromString(_)) // hm rethink .get?

  implicit val hdChainTypeMapper: BaseColumnType[HDChainType] =
    MappedColumnType.base[HDChainType, Int](_.index, HDChainType.fromInt)

  implicit val hdPurposeMapper: BaseColumnType[HDPurpose] =
    MappedColumnType
      .base[HDPurpose, Int](_.constant, HDPurposes.fromConstant(_).get) // hm rething .get

  implicit val bitcoinAddressMapper: BaseColumnType[BitcoinAddress] =
    MappedColumnType
      .base[BitcoinAddress, String](_.value, BitcoinAddress.fromStringExn)

  implicit val scriptTypeMapper: BaseColumnType[ScriptType] =
    MappedColumnType
      .base[ScriptType, String](_.toString, ScriptType.fromStringExn)

  implicit val txMapper: BaseColumnType[Transaction] =
    MappedColumnType.base[Transaction, String](_.hex, Transaction.fromHex)

  implicit val currencyUnitMapper: BaseColumnType[CurrencyUnit] =
    MappedColumnType
      .base[CurrencyUnit, Long](_.satoshis.toLong, l => Satoshis(Int64(l)))

}

object DbCommonsColumnMappers extends DbCommonsColumnMappers
