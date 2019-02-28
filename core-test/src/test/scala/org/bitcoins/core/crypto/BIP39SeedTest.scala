package org.bitcoins.core.crypto
import org.bitcoins.core.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.CryptoGenerators
import scodec.bits.HexStringSyntax

import scala.util.{Failure, Try}

class BIP39SeedTest extends BitcoinSUnitTest {
  behavior of "BIP39Seed"

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "have serialization symmetry - with password" in {
    forAll(CryptoGenerators.bip39SeedWithPassword) { seed =>
      assert(seed == BIP39Seed.fromBytes(seed.bytes))
    }
  }

  it must "have serialization symmetry - no password" in {
    forAll(CryptoGenerators.bip39SeedNoPassword) { seed =>
      assert(seed == BIP39Seed.fromBytes(seed.bytes))
    }
  }

  it must "reject seeds with too few bits of entropy" in {
    forAll(CryptoGenerators.entropy.bits128) { entropy =>
      val attempt = Try { BIP39Seed.fromBytes(entropy.toByteVector.drop(1)) }
      assert(attempt.isFailure)
      val exc = attempt.asInstanceOf[Failure[_]].exception

      assert(exc.getMessage contains "Seed must be between")
    }
  }

  it must "reject seeds with too many bits of entropy" in {
    forAll(CryptoGenerators.entropy.bits256) { entropy =>
      val attempt = Try {
        BIP39Seed.fromBytes((entropy ++ entropy).toByteVector ++ hex"c") // one byte too much
      }
      assert(attempt.isFailure)

      val exc = attempt
        .asInstanceOf[Failure[_]]
        .exception

      assert(exc.getMessage contains "Seed must be between")
    }
  }
}
