package org.bitcoins.db

import org.bitcoins.core.number.UInt64
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import slick.jdbc.{PostgresProfile, SQLiteProfile}

class UInt64MapperTest extends BitcoinSUnitTest {
  behavior of "UInt64Mapper"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDrivenConfigNewCode
  }
  private val sqlite = new DbCommonsColumnMappers(SQLiteProfile)

  private val postgres = new DbCommonsColumnMappers(PostgresProfile)

  it must "map a large uint64 to bytes and back" in {
    val u64 = UInt64.max
    val sqliteHex = sqlite.uInt64ToHex(u64)
    val sqliteu64 = UInt64.fromHex(sqliteHex)
    assert(sqliteu64 == u64)

    val pgHex = postgres.uInt64ToHex(u64)
    val pgu64 = UInt64.fromHex(pgHex)
    assert(pgu64 == u64)
  }

  it must "map UInt64.zero to bytes and back" in {
    val u64 = UInt64.zero
    val sqliteHex = sqlite.uInt64ToHex(u64)
    val sqliteu64 = UInt64.fromHex(sqliteHex)
    assert(sqliteu64 == u64)

    val pgHex = postgres.uInt64ToHex(u64)
    val pgu64 = UInt64.fromHex(pgHex)
    assert(pgu64 == u64)
  }

  it must "map uint64 to bytes and back" in {
    forAll(NumberGenerator.uInt64) { u64 =>
      val sqliteHex = sqlite.uInt64ToHex(u64)
      val sqliteu64 = UInt64.fromHex(sqliteHex)
      assert(sqliteu64 == u64)
      val pgHex = postgres.uInt64ToHex(u64)
      val pgu64 = UInt64.fromHex(pgHex)
      assert(pgu64 == u64)
    }
  }
}
