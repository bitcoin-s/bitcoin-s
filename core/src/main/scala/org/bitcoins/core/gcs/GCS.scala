package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bouncycastle.crypto.macs.SipHash
import org.bouncycastle.crypto.params.KeyParameter
import scodec.bits.{BitVector, BinStringSyntax, ByteVector}

import scala.annotation.tailrec

// TODO: Replace ByteVector with a type for keys
/**
  * Defines all functionality dealing with Golomb-Coded Sets
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#GolombCoded_Sets]]
  */
object GCS {

  /**
    * Given parameters and data, golomb-encodes the data
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-construction]]
    */
  def buildGCS(
      data: Vector[ByteVector],
      key: ByteVector,
      p: UInt8,
      m: UInt64): BitVector = {
    val hashedValues = hashedSetConstruct(data, key, m)
    val sortedHashedValues = hashedValues.sortWith(_ < _)
    encodeSortedSet(sortedHashedValues, p)
  }

  /**
    * Given parameters and data, constructs a GolombFilter for that data
    */
  def buildGolombFilter(
      data: Vector[ByteVector],
      key: ByteVector,
      p: UInt8,
      m: UInt64): GolombFilter = {
    val encodedData = buildGCS(data, key, p, m)

    GolombFilter(key, m, p, CompactSizeUInt(UInt64(data.length)), encodedData)
  }

  /**
    * Given data, constructs a GolombFilter for that data using Basic Block Filter parameters
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  def buildBasicBlockFilter(
      data: Vector[ByteVector],
      key: ByteVector): GolombFilter = {
    buildGolombFilter(data, key, BlockFilter.P, BlockFilter.M)
  }

  //item remains bytevector key to be changed create new file in GCS folder that redefines sipkeyhash
  //will probably have a require statement bytevector length of 16 bytes or 128 bits. Add a couple methods
  //to turn the key into an array of bytes (look to toArray method on line 65)
  private def sipHash(item: ByteVector, key: ByteVector): UInt64 = {
    // https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects
    val sipHashCParam = 2
    val sipHashDParam = 4

    val sh = new SipHash(sipHashCParam, sipHashDParam)

    val keyParam = new KeyParameter(key.toArray)

    sh.init(keyParam)

    val offset = 0

    sh.update(item.toArray, offset, item.length.toInt)

    val digest = sh.doFinal()

    UInt64.fromHex(digest.toHexString)
  }

  /**
    * Hashes the item to the range [0, f)
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects]]
    */
  def hashToRange(item: ByteVector, f: UInt64, key: ByteVector): UInt64 = {
    val hash = sipHash(item, key)

    val bigInt = (hash.toBigInt * f.toBigInt) >> 64

    UInt64(bigInt)
  }

  /**
    * Hashes the items of a set of items
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects]]
    */
  def hashedSetConstruct(
      rawItems: Vector[ByteVector],
      key: ByteVector,
      m: UInt64): Vector[UInt64] = {
    val n = rawItems.length
    val f = m * n

    val hashedItemsBuilder = Vector.newBuilder[UInt64]

    rawItems.foreach { item =>
      val setValue = hashToRange(item, f, key)
      hashedItemsBuilder.+=(setValue)
    }

    hashedItemsBuilder.result()
  }

  /**
    * Converts num to unary (6 = 1111110)
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    *
    * TODO: protect against large inputs which cause OutOfMemoryErrors and even larger ones which fail on toInt
    */
  def toUnary(num: UInt64): BitVector = {
    if (num == UInt64.zero) {
      bin"0"
    } else {
      /*
       * We use the fact that 2^n - 1 = 111...1 (in binary) where there are n 1 digits
       */
      val binUnary = (BigInt(1) << num.toInt) - 1
      val leftPadded = BitVector(binUnary.toByteArray)
      val noPadding = dropLeftPadding(leftPadded)

      noPadding.:+(false)
    }
  }

  /**
    * Encodes a hash into a unary prefix and binary suffix
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    */
  def golombEncode(item: UInt64, p: UInt8): BitVector = {
    val q = item >> p.toInt

    val prefix = toUnary(q)

    val pBits = item.bytes.toBitVector.takeRight(p.toInt)

    prefix ++ pBits
  }

  /**
    * Decodes an item off of the front of a BitVector by reversing [[GCS.golombEncode]]
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    */
  def golombDecode(codedItem: BitVector, p: UInt8): UInt64 = {
    @tailrec
    def split(vec: BitVector, accum: UInt64): (UInt64, BitVector) = {
      if (vec.head) {
        split(vec.tail, accum + UInt64.one)
      } else {
        (accum, vec.tail)
      }
    }

    val (q, pBits) = split(codedItem, UInt64.zero)

    val sizeWithPadding = (8 - (p.toInt % 8)) + p.toInt

    val pBitsAsBytes = {
      val withoutRightPaddingOrData = pBits.take(p.toInt)
      val withLeftPadding = withoutRightPaddingOrData.padLeft(sizeWithPadding)
      withLeftPadding.toByteVector
    }

    (q << p.toInt) + UInt64.fromBytes(pBitsAsBytes)
  }

  @tailrec
  private def dropLeftPadding(padded: BitVector): BitVector = {
    if (padded.isEmpty || padded.head) {
      padded
    } else {
      dropLeftPadding(padded.tail)
    }
  }

  /** Returns the first hash gcs-encoded at the front of a BitVector, as well as the remaining BitVector */
  private def golombDecodeItemFromSet(
      encodedData: BitVector,
      p: UInt8): (UInt64, BitVector) = {
    val head = golombDecode(encodedData, p)

    val prefixSize = (head >> p.toInt).toInt + 1

    (head, encodedData.drop(prefixSize + p.toInt))
  }

  /**
    * Decodes all hashes from golomb-encoded data, reversing [[GCS.encodeSortedSet]]
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-queryingdecompression]]
    */
  def golombDecodeSet(encodedData: BitVector, p: UInt8): Vector[UInt64] = {
    @tailrec
    def loop(
        encoded: BitVector,
        decoded: Vector[UInt64],
        lastHash: UInt64): Vector[UInt64] = {
      if (encoded.length < p.toInt + 1) { // Only padding left
        decoded
      } else {
        val (delta, encodedLeft) = golombDecodeItemFromSet(encoded, p)
        val hash = lastHash + delta

        loop(encodedLeft, decoded.:+(hash), hash)
      }
    }

    loop(encoded = encodedData, decoded = Vector.empty, lastHash = UInt64.zero)
  }

  /**
    * Given a set of ascending hashes, golomb-encodes them
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-construction]]
    */
  def encodeSortedSet(hashes: Vector[UInt64], p: UInt8): BitVector = {
    val (golombStream, _) = hashes.foldLeft((BitVector.empty, UInt64.zero)) {
      case ((accum, lastHash), hash) =>
        val delta = hash - lastHash
        val encoded = golombEncode(delta, p)
        (accum ++ encoded, hash)
    }

    golombStream
  }
}
