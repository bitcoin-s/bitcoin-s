package org.bitcoins.core.bloom

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  P2PKScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptToken}
import org.bitcoins.core.serializers.bloom.RawBloomFilterSerializer
import org.bitcoins.core.util.{BitcoinSLogger, BytesUtil}
import scodec.bits.{BitVector, ByteVector}

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import org.bitcoins.crypto.{
  CryptoUtil,
  DoubleSha256Digest,
  ECPublicKey,
  Factory,
  HashDigest,
  NetworkElement,
  Sha256Hash160Digest
}

/**
  * Implements a bloom filter that abides by the semantics of BIP37
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki BIP37]].
  * @see [[https://github.com/bitcoin/bitcoin/blob/master/src/bloom.h Bitcoin Core bloom.h]]
  */
sealed abstract class BloomFilter extends NetworkElement with BitcoinSLogger {

  /** How large the bloom filter is, in Bytes */
  def filterSize: CompactSizeUInt

  /** The bits that are set inside of the bloom filter */
  def data: ByteVector

  /** The number of hash functions used in the bloom filter */
  def hashFuncs: UInt32

  /** An arbitrary value to add to the seed value in the hash function used by the bloom filter. */
  def tweak: UInt32

  /**
    * A set of flags that control how outpoints corresponding to a matched pubkey script are added to the filter.
    * See the 'Comparing Transaction Elements to a Bloom Filter' section in this
    * @see [[https://bitcoin.org/en/developer-reference#filterload link]]
    */
  def flags: BloomFlag

  /** Inserts a sequence of bytes into the [[org.bitcoins.core.bloom.BloomFilter BloomFilter]] */
  def insert(bytes: ByteVector): BloomFilter = {
    //these are the bit indexes that need to be set inside of data
    val bitIndexes = (0 until hashFuncs.toInt).map(i => murmurHash(i, bytes))
    @tailrec
    def loop(remainingBitIndexes: Seq[Int], accum: ByteVector): ByteVector = {
      if (remainingBitIndexes.isEmpty) accum
      else {
        val currentIndex = remainingBitIndexes.head
        //since we are dealing with a bit vector, this gets the byteIndex we need to set
        //the bit inside of.
        val byteIndex = currentIndex >>> 3
        //we need to calculate the bitIndex we need to set inside of our byte
        val bitIndex = (1 << (7 & currentIndex)).toByte
        val byte = accum(byteIndex)
        val setBitByte: Byte = (byte | bitIndex).toByte
        //replace old byte with new byte with bit set
        val newAccum: ByteVector = accum.update(byteIndex, setBitByte)
        loop(remainingBitIndexes.tail, newAccum)
      }
    }
    val newData = loop(bitIndexes, data)
    BloomFilter(filterSize, newData, hashFuncs, tweak, flags)
  }

  /** Inserts a hash into the filter */
  def insert(hash: HashDigest): BloomFilter = insert(hash.bytes)

  /** Inserts a sequence of hashes into the filter */
  def insertHashes(hashes: Seq[HashDigest]): BloomFilter = {
    val byteVectors = hashes.map(_.bytes)
    insertByteVectors(byteVectors)
  }

  /** Inserts a transaction outpoint into the filter */
  def insert(outPoint: TransactionOutPoint): BloomFilter =
    insert(outPoint.bytes)

  /**
    * Inserts a public key and it's corresponding hash into the bloom filter
    *
    * @note The rationale for inserting both the pubkey and its hash is that
    *       in most cases where you have an "interesting pubkey" that you
    *       want to track on the P2P network what you really need to do is
    *       insert the hash of the public key, as that is what appears on
    *       the blockchain and that nodes you query with bloom filters will
    *       see.
    *
    * @see  The
    *       [[https://github.com/bitcoinj/bitcoinj/blob/806afa04419ebdc3c15d5adf065979aa7303e7f6/core/src/main/java/org/bitcoinj/core/BloomFilter.java#L244 BitcoinJ]]
    *       implementation of this function
    */
  def insert(pubkey: ECPublicKey): BloomFilter = {
    val pubkeyBytes = pubkey.bytes
    val hash = CryptoUtil.sha256Hash160(pubkeyBytes)
    insert(pubkeyBytes).insert(hash)
  }

  /** Checks if the filter contains the given public key */
  def contains(pubkey: ECPublicKey): Boolean = contains(pubkey.bytes)

  /** Checks if the filter contains the given bytes */
  def contains(bytes: ByteVector): Boolean = {
    val bitIndexes = (0 until hashFuncs.toInt).map(i => murmurHash(i, bytes))
    @tailrec
    def loop(remainingBitIndexes: Seq[Int], accum: BitVector): Boolean = {
      if (remainingBitIndexes.isEmpty) {
        !accum.toIndexedSeq.exists(_ == false)
      } else {
        val currentIndex = remainingBitIndexes.head
        val byteIndex = currentIndex >>> 3
        val bitIndex = (1 << (7 & currentIndex)).toByte
        val byte = data(byteIndex)
        val isBitSet = (byte & bitIndex) != 0
        loop(remainingBitIndexes.tail, isBitSet +: accum)
      }
    }
    loop(bitIndexes, BitVector.empty)
  }

  /** Checks if `data` contains a [[DoubleSha256Digest DoubleSha256Digest]] */
  def contains(hash: DoubleSha256Digest): Boolean = contains(hash.bytes)

  /** Checks if `data` contains a [[org.bitcoins.core.protocol.transaction.TransactionOutPoint TransactionOutPoint]] */
  def contains(outPoint: TransactionOutPoint): Boolean =
    contains(outPoint.bytes)

  /** Checks if `data` contains a [[DoubleSha256Digest Sha256Hash160Digest]] */
  def contains(hash: Sha256Hash160Digest): Boolean = contains(hash.bytes)

  /**
    * Checks if the transaction's txid, or any of the constants in it's scriptPubKeys/scriptSigs match our BloomFilter
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#filter-matching-algorithm BIP37]]
    * for exact details on what is relevant to a bloom filter and what is not relevant
    */
  def isRelevant(transaction: Transaction): Boolean = {
    val scriptPubKeys = transaction.outputs.map(_.scriptPubKey)
    //pull out all of the constants in the scriptPubKey's
    val constantsWithOutputIndex = scriptPubKeys.zipWithIndex.flatMap {
      case (scriptPubKey, index) =>
        scriptPubKey.asm.collect {
          case c: ScriptConstant => (c, index)
        }
    }

    //check if the bloom filter contains any of the script constants in our outputs
    val constantsOutput = constantsWithOutputIndex.filter {
      case (c, _) => contains(c.bytes)
    }

    val scriptSigs = transaction.inputs.map(_.scriptSignature)
    val constantsWithInputIndex = scriptSigs.zipWithIndex.flatMap {
      case (scriptSig, index) =>
        scriptSig.asm.collect {
          case c: ScriptConstant => (c, index)
        }
    }
    //check if the filter contains any of the prevouts in this tx
    val containsOutPoint =
      transaction.inputs.filter(i => contains(i.previousOutput))

    //check if the bloom filter contains any of the script constants in our inputs
    val constantsInput = constantsWithInputIndex.filter {
      case (c, _) => contains(c.bytes)
    }

    constantsOutput.nonEmpty || constantsInput.nonEmpty ||
    containsOutPoint.nonEmpty || contains(transaction.txId)
  }

  /**
    * Updates this bloom filter to contain the relevant information for the given Transaction
    *
    * @see  [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#filter-matching-algorithm BIP37]]
    * for the exact details on what parts of a transaction is added to the bloom filter
    */
  def update(transaction: Transaction): BloomFilter =
    flags match {
      case BloomUpdateAll =>
        val scriptPubKeys = transaction.outputs.map(_.scriptPubKey)
        //a sequence of outPoints that need to be inserted into the filter
        val outPoints: Seq[TransactionOutPoint] =
          scriptPubKeys.zipWithIndex.flatMap {
            case (scriptPubKey, index) =>
              // we filter all constants, and create an outpoint if the constant matches our filter
              scriptPubKey.asm.collect {
                case c: ScriptConstant if contains(c.bytes) =>
                  TransactionOutPoint(transaction.txId, UInt32(index))
              }
          }

        logger.debug("Inserting outPoints: " + outPoints)
        val outPointsBytes = outPoints.map(_.bytes)
        val filterWithOutPoints = insertByteVectors(outPointsBytes)
        //add txid
        val filterWithTxIdAndOutPoints =
          filterWithOutPoints.insert(transaction.txId)
        filterWithTxIdAndOutPoints
      case BloomUpdateNone =>
        logger.debug(
          "You are attempting to update a bloom filter when the flag is set to BloomUpdateNone, " +
            "no information will be added to the bloom filter, specifically this transaction: " + transaction)
        this
      case BloomUpdateP2PKOnly =>
        //update the filter with the outpoint if the filter matches any of the constants in a p2pkh or multisig script pubkey
        val scriptPubKeysWithIndex =
          transaction.outputs.map(_.scriptPubKey).zipWithIndex
        updateP2PKOnly(scriptPubKeysWithIndex, transaction.txId)

    }

  /**
    * Updates a bloom filter according to the rules specified by the
    * [[org.bitcoins.core.bloom.BloomUpdateP2PKOnly BloomUpdateP2PKOnly]] flag
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#filter-matching-algorithm BIP37]]
    * for the exact rules on updating a bloom filter with this flag set
    */
  def updateP2PKOnly(
      scriptPubKeysWithIndex: Seq[(ScriptPubKey, Int)],
      txId: DoubleSha256Digest): BloomFilter = {
    @tailrec
    def loop(
        constantsWithIndex: Seq[(ScriptToken, Int)],
        accumFilter: BloomFilter): BloomFilter =
      constantsWithIndex match {
        case h +: t if accumFilter.contains(h._1.bytes) =>
          logger.debug("Found constant in bloom filter: " + h._1.hex)
          val filter =
            accumFilter.insert(TransactionOutPoint(txId, UInt32(h._2)))
          loop(t, filter)
        case _ +: t => loop(t, accumFilter)
        case Nil    => accumFilter
      }
    val p2pkOrMultiSigScriptPubKeys: Seq[(ScriptPubKey, Int)] =
      scriptPubKeysWithIndex.filter {
        case (s, _) =>
          s.isInstanceOf[P2PKScriptPubKey] ||
            s.isInstanceOf[MultiSignatureScriptPubKey]
      }
    //gets rid of all asm operations in the scriptPubKey except for the constants
    val scriptConstantsWithOutputIndex: Seq[(ScriptToken, Int)] =
      p2pkOrMultiSigScriptPubKeys.flatMap {
        case (scriptPubKey, index) =>
          (scriptPubKey.asm.map(token => (token, index))).filter {
            case (token, _) => token.isInstanceOf[ScriptConstant]
          }
      }
    loop(scriptConstantsWithOutputIndex, this)
  }

  /**
    * Performs the [[scala.util.hashing.MurmurHash3 MurmurHash3]] on the given hash
    *
    * @param hashNum the nth hash function we are using
    * @param bytes the bytes of the data that needs to be inserted into the
    *              [[org.bitcoins.core.bloom.BloomFilter BloomFilter]]
    * @return the index of the bit inside of `data` that needs to be set to 1
    */
  private def murmurHash(hashNum: Int, bytes: ByteVector): Int = {
    //TODO: The call of .toInt is probably the source of a bug here, need to come back and look at this
    //since this isn't consensus critical though I'm leaving this for now
    val seed = (hashNum * murmurConstant.toLong + tweak.toLong).toInt
    val murmurHash = MurmurHash3.bytesHash(bytes.toArray, seed)
    val uint32 = UInt32(BytesUtil.encodeHex(murmurHash))
    val modded = uint32.toLong % (filterSize.num.toInt * 8)
    modded.toInt
  }

  /**
    * See [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#bloom-filter-format BIP37]]
    * to see where this number comes from
    */
  private def murmurConstant = UInt32("fba4c795")

  /** Adds a sequence of byte vectors to our bloom filter then returns that new filter */
  def insertByteVectors(
      bytes: scala.collection.Seq[ByteVector]): BloomFilter = {
    @tailrec
    def loop(
        remainingByteVectors: scala.collection.Seq[ByteVector],
        accumBloomFilter: BloomFilter): BloomFilter = {
      if (remainingByteVectors.isEmpty) accumBloomFilter
      else
        loop(remainingByteVectors.tail,
             accumBloomFilter.insert(remainingByteVectors.head))
    }
    loop(bytes, this)
  }

  override def bytes = RawBloomFilterSerializer.write(this)
}

object BloomFilter extends Factory[BloomFilter] with BitcoinSLogger {

  private case class BloomFilterImpl(
      filterSize: CompactSizeUInt,
      data: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag)
      extends BloomFilter

  /** Max bloom filter size as per [[https://bitcoin.org/en/developer-reference#filterload]] */
  val maxSize = UInt32(36000)

  /** Max hashFunc size as per [[https://bitcoin.org/en/developer-reference#filterload]] */
  val maxHashFuncs = UInt32(50)

  val empty: BloomFilter = BloomFilterImpl(CompactSizeUInt.zero,
                                           ByteVector.empty,
                                           UInt32.zero,
                                           UInt32.zero,
                                           BloomUpdateAll)

  /**
    * Creates a bloom filter based on the number of elements to be inserted into the filter
    * and the desired false positive rate.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#bloom-filter-format BIP37]]
    */
  // todo provide default flag?
  def apply(
      numElements: Int,
      falsePositiveRate: Double,
      flags: BloomFlag): BloomFilter = {
    val random = Math.floor(Math.random() * UInt32.max.toLong).toLong
    val tweak = UInt32(random)
    apply(numElements, falsePositiveRate, tweak, flags)
  }

  // todo: provide a apply method where you can pass in bloom-able filters
  // through a type class

  /**
    * Creates a bloom filter based on the number of elements to be inserted into the filter
    * and the desired false positive rate.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#bloom-filter-format BIP37]]
    *
    * @param tweak A random value that only acts to randomize the filter and increase privacy
    */
  def apply(
      numElements: Int,
      falsePositiveRate: Double,
      tweak: UInt32,
      flags: BloomFlag): BloomFilter = {
    if (numElements == 0) {
      BloomFilter.empty
    } else {
      import scala.math._
      //m = number of bits in the array
      //n = number of elements in the array
      //from https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#bloom-filter-format
      val optimalFilterSize: Double =
        (-1 / pow(log(2), 2) * numElements * log(falsePositiveRate)) / 8
      logger.debug("optimalFilterSize " + optimalFilterSize)
      //BIP37 places limitations on the filter size, namely it cannot be > 36,000 bytes
      val actualFilterSize: Int =
        max(1, min(optimalFilterSize, maxSize.toInt * 8)).toInt
      logger.debug("actualFilterSize: " + actualFilterSize)
      val optimalHashFuncs: Double = actualFilterSize * 8 / numElements * log(2)
      //BIP37 places a limit on the amount of hashFuncs we can use, which is 50
      val actualHashFuncs: Int =
        max(1, min(optimalHashFuncs, maxHashFuncs.toInt)).toInt

      val emptyByteArray = ByteVector(Array.fill(actualFilterSize)(0.toByte))
      BloomFilter(filterSize = CompactSizeUInt(UInt64(actualFilterSize)),
                  data = emptyByteArray,
                  hashFuncs = UInt32(actualHashFuncs),
                  tweak = tweak,
                  flags = flags)
    }

  }

  def apply(
      filterSize: CompactSizeUInt,
      data: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag): BloomFilter = {
    BloomFilterImpl(filterSize, data, hashFuncs, tweak, flags)
  }

  override def fromBytes(bytes: ByteVector): BloomFilter =
    RawBloomFilterSerializer.read(bytes)

}
