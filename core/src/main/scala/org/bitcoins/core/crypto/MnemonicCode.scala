package org.bitcoins.core.crypto

import java.security.SecureRandom

import org.bitcoins.core.util.{CryptoUtil, MaskedToString}
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec
import scala.io.Source

/**
  * A mnemonic code conforming to [[https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki BIP39]].
  * BIP39 mnemonic codes consist of a varying number of words (most often English,
  * possible with other languages as well) that can be used to generate an
  * [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey ]] which again
  * can be the root of a [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki  BIP32]]
  * HD wallet.
  */
sealed abstract class MnemonicCode extends MaskedToString {
  require(
    MnemonicCode.VALID_LENGTHS.contains(words.length), {
      val validLengths = MnemonicCode.VALID_LENGTHS.mkString(", ")
      s"Number of words must be one of the following: $validLengths, got: ${words.length} "
    }
  )

  require({
    val entropy = toEntropyWithChecksum
    isEntropyWithChecksumValid(entropy)
  }, "Entropy checksum is not valid!")

  /**
    * Checks that the provided entropy has a valid checksum
    * attached at the end
    */
  private[crypto] def isEntropyWithChecksumValid(
      entropyWithChecksum: BitVector): Boolean = {

    val codeInfo = MnemonicCode.getMnemonicCodeInfo(words)
    val entropyNoChecksum = entropyWithChecksum.take(codeInfo.entropyBits)

    val hashedEntropy = CryptoUtil.sha256(entropyNoChecksum).bytes.toBitVector
    val checksum = hashedEntropy.take(codeInfo.checksumLength)

    entropyNoChecksum ++ checksum == entropyWithChecksum

  }

  /**
    * The mnemonic code itself
    */
  def words: Vector[String]

  /**
    * Returns the entropy initially provided to construct
    * this mnemonic code
    */
  private[bitcoins] def toEntropy: BitVector = {
    val entropyWithChecksumBits = toEntropyWithChecksum
    val lengthNoEntropy = MnemonicCode
      .getMnemonicCodeInfo(words)
      .entropyBits

    entropyWithChecksumBits.take(lengthNoEntropy)
  }

  /**
    * Returns the entropy _with checksum_ originally provided
    * to construct this mnemonic code
    */
  private[crypto] def toEntropyWithChecksum: BitVector = {

    def bitsForWord(word: String): BitVector = {

      // tail-recursive binary search for word in word list
      // stolen from https://codereview.stackexchange.com/questions/161887/re-inventing-the-wheel-binary-search-in-scala
      import MnemonicCode.{ENGLISH_WORDS => wordlist}
      @tailrec
      def search(
          start: Int = 0,
          end: Int = wordlist.length - 1
      ): Option[Int] = {
        val mid = start + (end - start) / 2
        if (start > end) None // can't be found
        else if (wordlist(mid) == word) Some(mid) // found
        else if (wordlist(mid) > word)
          search(start, mid - 1) // narrow the field
        else search(mid + 1, end)
      }

      val indexOpt = search()

      val index = indexOpt.getOrElse(
        throw new IllegalArgumentException(s"Couldn't find $word in word list!")
      )

      BitVector.fromInt(index, size = MnemonicCode.BIT_GROUP_LENGTH)

    }

    val bits: BitVector = words
      .map(curr => bitsForWord(curr))
      .fold(BitVector.empty)((accum, curr) => accum ++ curr)

    val codeInfo = MnemonicCode.getMnemonicCodeInfo(words)

    bits.take(codeInfo.entropyAndChecksumBits)
  }

  override def toStringSensitive: String = {
    words.mkString(",")
  }
}

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki BIP39]]
  */
object MnemonicCode {

  /**
    * The valid lengths a BIP39 mnemonic code phrase can be, according to
    * [[https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki BIP39]]
    */
  private[crypto] val VALID_LENGTHS = Vector(12, 15, 18, 21, 24)

  /**
    * Length of bit groups entropy is split into to
    * derive words from wordlist
    */
  private[crypto] val BIT_GROUP_LENGTH = 11

  private case class MnemonicCodeImpl(words: Vector[String])
      extends MnemonicCode

  /**
    * Creates a mnemonic code from the provided words
    */
  def fromWords(words: Vector[String]): MnemonicCode = MnemonicCodeImpl(words)

  /**
    * Generates a mnemonic code from the given entropy bytes
    */
  def fromEntropy(entropy: ByteVector): MnemonicCode =
    fromEntropy(entropy.bits)

  /**
    * The minimum number of entropy bits needed to construct a mnemonic code
    */
  private[crypto] val MIN_ENTROPY_BITS = 128

  /**
    * The maximum number of entropy bits allowed to construct mnemonic code
    */
  private[crypto] val MAX_ENTROPY_BITS = 256

  /**
    * The entropy for a mnemonic code has to be a multiple of 32
    */
  private[crypto] val ENTROPY_MULTIPLE = 32

  /**
    * Generates a mnemonic code from the given entropy bits
    */
  def fromEntropy(entropy: BitVector): MnemonicCode = {
    require(
      entropy.length >= MIN_ENTROPY_BITS,
      s"Entropy must be at least $MIN_ENTROPY_BITS bits"
    )

    require(
      entropy.length <= MAX_ENTROPY_BITS,
      s"Entropy cannot be longer than $MAX_ENTROPY_BITS bits"
    )

    require(
      entropy.length % ENTROPY_MULTIPLE == 0,
      s"Entropy must be a multiple of $ENTROPY_MULTIPLE bits!"
    )

    val ENTROPY_CHECKSUM_DIVISOR = 32
    val checksumLength = entropy.length / ENTROPY_CHECKSUM_DIVISOR

    val hashedEntropy = CryptoUtil.sha256(entropy)
    val hashedEntropyBits = hashedEntropy.bytes.toBitVector
    val checkSum = hashedEntropyBits.take(checksumLength)
    val entropyWithChecksum = entropy ++ checkSum

    fromEntropyWithCheck(entropyWithChecksum)
  }

  private[crypto] def fromEntropyWithCheck(
      entropyWithChecksum: BitVector): MnemonicCode = {
    val bitGroups = entropyWithChecksum.grouped(BIT_GROUP_LENGTH)
    val words = bitGroups.map { group =>
      val index = group.toInt(signed = false)
      ENGLISH_WORDS(index)
    }.toVector
    MnemonicCodeImpl(words)

  }

  /**
    * Generates the specified bits of entropy
    */
  private def getEntropy(bits: Int): BitVector = {
    require(bits % 8 == 0,
            s"Given amount if bits ($bits) must be a multiple of 8!")

    val randomGenerator: SecureRandom = new SecureRandom

    val byteArray: Array[Byte] = new Array[Byte](bits / 8)
    randomGenerator.nextBytes(byteArray)
    val bitVector = BitVector(byteArray)

    bitVector.ensuring(
      bitVector => bits == bitVector.length,
      s"Did not generate enough bits of entropy! Exepcted=$bits, actual=${bitVector.length}"
    )
  }

  /**
    * Gets 128 bits of cryptographically secure entropy
    */
  def getEntropy128Bits: BitVector = getEntropy(128)

  /**
    * Gets 16o bits of cryptographically secure entropy
    */
  def getEntropy160Bits: BitVector = getEntropy(160)

  /**
    * Gets 192 bits of cryptographically secure entropy
    */
  def getEntropy192Bits: BitVector = getEntropy(192)

  /**
    * Gets 224 bits of cryptographically secure entropy
    */
  def getEntropy224Bits: BitVector = getEntropy(224)

  /**
    * Gets 256 bits of cryptographically secure entropy
    */
  def getEntropy256Bits: BitVector = getEntropy(256)

  private[crypto] case class MnemonicCodeInfo(
      entropyBits: Int,
      checksumLength: Int,
      entropyAndChecksumBits: Int,
      words: Int)

  /**
    * Taken from
    * [[https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#generating-the-mnemonic BIP39]]
    */
  private[crypto] def getMnemonicCodeInfo(
      words: Vector[String]): MnemonicCodeInfo =
    words.length match {
      case 12 => MnemonicCodeInfo(128, 4, 132, 12)
      case 15 => MnemonicCodeInfo(160, 5, 165, 15)
      case 18 => MnemonicCodeInfo(192, 6, 198, 18)
      case 21 => MnemonicCodeInfo(224, 7, 231, 21)
      case 24 => MnemonicCodeInfo(256, 8, 264, 24)
      case _: Int =>
        throw new RuntimeException(s"Bad mnemonic code length: ${words.length}")
    }

  private val ENGLISH_WORDS_FILE = "/bip39-wordlists/english.txt"

  private[crypto] lazy val ENGLISH_WORDS: Vector[String] = {
    val resourceStream = getClass.getResourceAsStream(ENGLISH_WORDS_FILE)
    val source = Source.fromInputStream(resourceStream)

    val lines = source.getLines
    val linesVec = lines.toVector
    source.close()
    linesVec
  }.ensuring(words => words.length == 2048, "Word list must be 2048 words long")
}
