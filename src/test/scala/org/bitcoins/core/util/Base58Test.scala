package org.bitcoins.core.util

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.crypto.{ ECPrivateKey, Sha256Hash160Digest }
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.util.testprotocol._
import org.scalatest.{ FlatSpec, MustMatchers }
import spray.json._

import scala.io.Source

/**
 * Created by tom on 5/17/16.
 */
class Base58Test extends FlatSpec with MustMatchers {
  "Base58" must "encode byte value of 0 to character of 1" in {
    Base58.encode(0.toByte) must be("1")
  }

  it must "encode byte value of 22 to character P" in {
    Base58.encode(22.toByte) must be("P")
  }

  it must "decode base58 character '1' to byte value of 0 then encode back to base58 char '1'" in {
    val char = "1"
    val decoded = Base58.decode(char)
    Base58.encode(decoded) must be(char)
  }

  it must "decode character Z to byte value of 32" in {
    Base58.decode("Z").head must be(32.toByte)
  }

  it must "decode and return same result as bitcoinj" in {
    val address = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    val bitcoinj = org.bitcoinj.core.Base58.decode(address)
    Base58.decode(address) must be(bitcoinj)
  }

  it must "encode tests in base58_encode_decode.json" in {
    Base58.encode("") must be("")
    Base58.encode("61") must be("2g")
    Base58.encode("626262") must be("a3gV")
    Base58.encode("636363") must be("aPEr")
    Base58.encode("73696d706c792061206c6f6e6720737472696e67") must be("2cFupjhnEsSn59qHXstmK2ffpLv2")
    Base58.encode("00eb15231dfceb60925886b67d065299925915aeb172c06647") must be("1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L")
    Base58.encode("516b6fcd0f") must be("ABnLTmg")
    Base58.encode("bf4f89001e670274dd") must be("3SEo3LWLoPntC")
    Base58.encode("572e4794") must be("3EFU7m")
    Base58.encode("ecac89cad93923c02321") must be("EJDM8drfXA6uyA")
    Base58.encode("10c8511e") must be("Rt5zm")
    Base58.encode("00000000000000000000") must be("1111111111")
  }

  it must "decode tests in base58_encode_decode.json" in {
    def decodedBase58EncodeToHex(value: String): String = BitcoinSUtil.encodeHex(Base58.decode(value))
    decodedBase58EncodeToHex("2g") must be("61")
    decodedBase58EncodeToHex("a3gV") must be("626262")
    decodedBase58EncodeToHex("aPEr") must be("636363")
    decodedBase58EncodeToHex("2cFupjhnEsSn59qHXstmK2ffpLv2") must be("73696d706c792061206c6f6e6720737472696e67")
    decodedBase58EncodeToHex("1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L") must be("00eb15231dfceb60925886b67d065299925915aeb172c06647")
    decodedBase58EncodeToHex("ABnLTmg") must be("516b6fcd0f")
    decodedBase58EncodeToHex("3SEo3LWLoPntC") must be("bf4f89001e670274dd")
    decodedBase58EncodeToHex("3EFU7m") must be("572e4794")
    decodedBase58EncodeToHex("EJDM8drfXA6uyA") must be("ecac89cad93923c02321")
    decodedBase58EncodeToHex("Rt5zm") must be("10c8511e")
    decodedBase58EncodeToHex("1111111111") must be("00000000000000000000")
  }

  it must "decode address into bytes, then encode bytes back to address the same as bitcoinj" in {
    //1C4kYhyLftmkn48YarSoLupxHfYFo8kp64
    val address = TestUtil.bitcoinAddress.value
    val bitcoinj = org.bitcoinj.core.Base58.encode(org.bitcoinj.core.Base58.decode(address))
    Base58.encode(Base58.decode(address)) must be(bitcoinj)
    Base58.encode(Base58.decode(address)) must be("1C4kYhyLftmkn48YarSoLupxHfYFo8kp64")
  }

  it must "decode multisig address into bytes then encode back to multisig" in {
    val multi = TestUtil.multiSigAddress.value
    val bitcoinj = org.bitcoinj.core.Base58.encode(org.bitcoinj.core.Base58.decode(multi))
    Base58.encode(Base58.decode(multi)) must be(TestUtil.multiSigAddress.value)
    Base58.encode(Base58.decode(multi)) must be(bitcoinj)
  }

  it must "read base58_keys_valid.json and validate each case" in {
    import org.bitcoins.core.util.testprotocol.Base58ValidTestCaseProtocol._
    val source = Source.fromURL(this.getClass.getResource("/base58_keys_valid.json"))
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCases: Seq[Base58ValidTestCase] = json.convertTo[Seq[Base58ValidTestCase]]
    for {
      testCase <- testCases
    } yield {
      //if testCase is an Address, it must have a valid base58 representation
      if (testCase.addressOrWIFPrivKey.isLeft) {
        Base58.isValid(testCase.addressOrWIFPrivKey.left.get.value) must be(true)
      } else {
        Base58.isValid(testCase.addressOrWIFPrivKey.right.get) must be(true)
      }
    }
  }

  it must "read base58_keys_invalid.json and return each as an invalid base58 string" in {
    import org.bitcoins.core.util.testprotocol.Base58InvalidTestCase
    import org.bitcoins.core.util.testprotocol.Base58InvalidTestCaseProtocol._

    val source = Source.fromURL(this.getClass.getResource("/base58_keys_invalid.json"))
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCases: Seq[Base58InvalidTestCase] = json.convertTo[Seq[Base58InvalidTestCase]]
    for {
      testCase <- testCases
    } yield {
      testCase must be(Base58InvalidTestCaseImpl(testCase.base58EncodedString))
      Base58.isValid(testCase.base58EncodedString) must be(false)
    }
  }

  it must "check validity of base58 string with illegal characters and fail" in {
    Base58.isValid("3CMNFxN1oHBc4R1EpboAL5yzHGgE611Xol") must be(false)
  }

  it must "decodeCheck a string with a length less than 4 and fail" in {
    Base58.decodeCheck("asf").isFailure must be(true)
  }
  it must "decodeCheck a valid string and succeed" in {
    Base58.decodeCheck("3CMNFxN1oHBc4R1EpboAL5yzHGgE611Xou").isSuccess must be(true)
  }
}
