package org.bitcoins.core.crypto

import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion
import play.api.libs.json._
import scodec.bits.{BinStringSyntax, BitVector, ByteVector}

import scala.io.Source
import scala.util.{Failure, Try}

class MnemonicCodeTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "MnemonicCode"

  it must "generate mnemonics of the right length" in {
    forAll(CryptoGenerators.entropy.bits128) { entropy =>
      MnemonicCode.fromEntropy(entropy).words.length == 12
    }

    forAll(CryptoGenerators.entropy.bits160) { entropy =>
      MnemonicCode.fromEntropy(entropy).words.length == 15
    }

    forAll(CryptoGenerators.entropy.bits192) { entropy =>
      MnemonicCode.fromEntropy(entropy).words.length == 18
    }

    forAll(CryptoGenerators.entropy.bits224) { entropy =>
      MnemonicCode.fromEntropy(entropy).words.length == 21
    }

    forAll(CryptoGenerators.entropy.bits256) { entropy =>
      MnemonicCode.fromEntropy(entropy).words.length == 24
    }

  }

  it must "not generate back to back idential entropy bitvectors" in {
    import CryptoGenerators.entropy

    def notEqual(b1: BitVector, b2: BitVector): Assertion = assert(b1 != b2)

    forAll(entropy.bits128, entropy.bits128)(notEqual)

    forAll(entropy.bits160, entropy.bits160)(notEqual)

    forAll(entropy.bits192, entropy.bits192)(notEqual)

    forAll(entropy.bits224, entropy.bits224)(notEqual)

    forAll(entropy.bits256, entropy.bits256)(notEqual)

    forAll(entropy.any, entropy.any)(notEqual)
  }

  it must "fail to generate mnemonics from entropy of bad length" in {
    def testEntropyFailure(entropy: BitVector): Assertion = {
      val short = entropy.drop(1)
      val long = entropy ++ bin"1"
      assertThrows[IllegalArgumentException](MnemonicCode.fromEntropy(short))
      assertThrows[IllegalArgumentException](MnemonicCode.fromEntropy(long))
    }

    testEntropyFailure(MnemonicCode.getEntropy128Bits)
    testEntropyFailure(MnemonicCode.getEntropy160Bits)
    testEntropyFailure(MnemonicCode.getEntropy192Bits)
    testEntropyFailure(MnemonicCode.getEntropy224Bits)
    testEntropyFailure(MnemonicCode.getEntropy256Bits)
  }

  it must "fail to generate mnemonics from too long entropy" in {
    import CryptoGenerators.entropy
    forAll(entropy.bits128, entropy.bits160) { (e1, e2) =>
      val attempt = Try { MnemonicCode.fromEntropy(e1 ++ e2) }
      assert(attempt.isFailure)

      val exc = attempt
        .asInstanceOf[Failure[_]]
        .exception

      val message = exc.getMessage

      message contains "Entropy cannot be longer"
    }
  }

  it must "fail to generate mnemonics from too short entropy" in {
    import CryptoGenerators.entropy
    forAll(entropy.bits128) { e =>
      val attempt = Try {
        MnemonicCode.fromEntropy(
          // to provide a bitvector of bad length, but correct entropy multiple
          e.take(MnemonicCode.ENTROPY_MULTIPLE))
      }
      assert(attempt.isFailure)
      val exc = attempt
        .asInstanceOf[Failure[_]]
        .exception

      val message = exc.getMessage
      assert(message contains "Entropy must be at least")
    }
  }

  it must "fail to generate mnemonics from entropy of bad length multiple" in {
    import CryptoGenerators.entropy
    forAll(entropy.bits192) { e =>
      val codeT = Try(MnemonicCode.fromEntropy(e.drop(1)))

      assert(codeT.isFailure)
      val exc = codeT.asInstanceOf[Failure[_]].exception
      assert(exc.getMessage contains "Entropy must be a multiple")
    }
  }

  it must "have serialization symmetry - entropy" in {
    forAll(CryptoGenerators.mnemonicCode) { code =>
      val entropy = code.toEntropy
      val newCode = MnemonicCode.fromEntropy(entropy)
      assert(code == newCode)
    }
  }

  it must "fail on a seed phrase with bad checksum" in {
    val correctSeed = Vector("phone",
                             "dilemma",
                             "early",
                             "never",
                             "test",
                             "surge",
                             "ecology",
                             "rail",
                             "medal",
                             "benefit",
                             "mystery",
                             "toward",
                             "lounge",
                             "candy",
                             "syrup")

    val newLastWord = "satoshi"
    val fromWordsT = Try {
      MnemonicCode.fromWords(correctSeed.dropRight(1) :+ newLastWord)
    }

    assert(fromWordsT.isFailure)
    val exc = fromWordsT.asInstanceOf[Failure[_]].exception
    assert(exc.getMessage contains "checksum is not valid")
  }

  it must "fail to generate mnemonic codes from word vectors of bad length" in {
    forAll(CryptoGenerators.mnemonicPhrase) { phrase =>
      val attempt = Try { MnemonicCode.fromWords(phrase.dropRight(1)) }
      assert(attempt.isFailure)

      val exc = attempt
        .asInstanceOf[Failure[_]]
        .exception

      val message = exc.getMessage

      assert(message contains "Number of words")
    }

    forAll(CryptoGenerators.mnemonicPhrase) { phrase =>
      val attempt = Try { MnemonicCode.fromWords(phrase ++ phrase.take(1)) }

      assert(attempt.isFailure)

      val exc = attempt
        .asInstanceOf[Failure[_]]
        .exception

      val message = exc.getMessage
      assert(message contains "Number of words")
    }
  }

  private case class RawTrezorTestVector(
      entropy: String,
      words: String,
      seed: String,
      xpriv: String)

  private case class TrezorTestVector(
      entropy: ByteVector,
      expectedWords: Vector[String],
      expectedSeed: BIP39Seed,
      expectedXPriv: ExtPrivateKey)

  private def testTrezorVector(
      vector: TrezorTestVector
  ): Assertion = {
    val TrezorTestVector(entropy, expectedWords, expectedSeed, expectedXPriv) =
      vector

    val code = MnemonicCode.fromEntropy(entropy)
    code.words must be(expectedWords)

    val calculatedEntropy = code.toEntropy
    calculatedEntropy must be(entropy.toBitVector)

    val seed = BIP39Seed.fromMnemonic(code, "TREZOR")
    seed must be(expectedSeed)

    val xpriv = seed.toExtPrivateKey(ExtKeyVersion.LegacyMainNetPriv)
    xpriv must be(expectedXPriv)
  }
  it must "pass all Trezor test vectors" in {

    def trezorFromRaw(raw: RawTrezorTestVector): TrezorTestVector = {
      val RawTrezorTestVector(rawEntropy, rawWords, rawSeed, rawXpriv) = raw
      TrezorTestVector(
        entropy = ByteVector.fromValidHex(rawEntropy),
        expectedWords = rawWords.split(" ").toVector,
        expectedSeed = BIP39Seed.fromHex(rawSeed),
        expectedXPriv = ExtPrivateKey.fromString(rawXpriv)
      )
    }

    implicit object TrezorReads extends Reads[RawTrezorTestVector] {
      def reads(json: JsValue): JsResult[RawTrezorTestVector] = {
        for {
          arr <- json.validate[JsArray]
          entropy <- arr(0).validate[String]
          words <- arr(1).validate[String]
          seed <- arr(2).validate[String]
          xpriv <- arr(3).validate[String]
        } yield RawTrezorTestVector(entropy, words, seed, xpriv)
      }
    }

    val rawJsonStream = getClass
      .getResourceAsStream("/trezor_bip39_vectors.json")

    val rawJson =
      Source
        .fromInputStream(rawJsonStream)
        .getLines()
        .mkString("\n")

    val json = Json.parse(rawJson)
    val testVectors =
      (json \ "english")
        .validate[Vector[RawTrezorTestVector]]
        .map(_.map(trezorFromRaw))
        .get

    testVectors.map(testTrezorVector(_))
  }

  it must "not serialize a MnemonicCode toString" in {
    val correctSeed = Vector("phone",
                             "dilemma",
                             "early",
                             "never",
                             "test",
                             "surge",
                             "ecology",
                             "rail",
                             "medal",
                             "benefit",
                             "mystery",
                             "toward",
                             "lounge",
                             "candy",
                             "syrup")

    val mnemonicCode = MnemonicCode.fromWords(correctSeed)

    mnemonicCode.toString must be("Masked(MnemonicCodeImpl)")

    mnemonicCode.toStringSensitive must be(correctSeed.mkString(","))
  }
}
