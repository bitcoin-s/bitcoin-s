package org.bitcoins.testkit.core.gen

import org.bitcoins.core.number._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.util.NumberUtil
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}

/**
  * Created by chris on 6/16/16.
  */
trait NumberGenerator {

  def positiveShort: Gen[Short] = {
    Gen.chooseNum[Short](0, Short.MaxValue)
  }

  /** Creates a generator that generates positive long numbers */
  def positiveLongs: Gen[Long] = Gen.choose(0, Long.MaxValue)

  /**
    * Integers between 0 and Int.MaxValue
    */
  val positiveInts: Gen[Int] = Gen.choose(0, Int.MaxValue)

  /**
    * Integers between Int.MinValue and -1
    */
  val negativeInts: Gen[Int] = Gen.choose(Int.MinValue, -1)

  /**
    * Random integers
    */
  val ints: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  /** Creates a generator for positive longs without the number zero */
  def positiveLongsNoZero: Gen[Long] = Gen.choose(1, Long.MaxValue)

  /** Creates a number generator that generates negative long numbers */
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue, -1)

  def uInt5: Gen[UInt5] = Gen.choose(0, 31).map(n => UInt5(n))

  def uInt5s: Gen[Seq[UInt5]] = Gen.listOf(uInt5)

  def uInt8: Gen[UInt8] = Gen.choose(0, 255).map(n => UInt8(n.toShort))

  def uInt8s: Gen[Seq[UInt8]] = Gen.listOf(uInt8)

  /**
    * Generates a number in the range 0 <= x <= 2 ^^32 - 1
    * then wraps it in a UInt32
    */
  def uInt32s: Gen[UInt32] =
    Gen.choose(0L, (NumberUtil.pow2(32) - 1).toLong).map(UInt32(_))

  /** Chooses a BigInt in the ranges of 0 <= bigInt < 2^^64 */
  def bigInts: Gen[BigInt] =
    Gen
      .chooseNum(Long.MinValue, Long.MaxValue)
      .map(x => BigInt(x) + BigInt(2).pow(63))

  def positiveBigInts: Gen[BigInt] = bigInts.filter(_ >= 0)

  def bigIntsUInt64Range: Gen[BigInt] =
    positiveBigInts.filter(_ < (BigInt(1) << 64))

  /**
    * Generates a number in the range 0 <= x < 2^^64
    * then wraps it in a UInt64
    */
  def uInt64s: Gen[UInt64] = uInt64

  def uInt64: Gen[UInt64] =
    for {
      bigInt <- bigIntsUInt64Range
    } yield UInt64(bigInt)

  def int32s: Gen[Int32] =
    Gen.choose(Int32.min.toLong, Int32.max.toLong).map(Int32(_))

  def int64s: Gen[Int64] =
    Gen.choose(Int64.min.toLong, Int64.max.toLong).map(Int64(_))

  def scriptNumbers: Gen[ScriptNumber] =
    Gen.choose(Int64.min.toLong, Int64.max.toLong).map(ScriptNumber(_))

  /** The policy bounds for nTimeLock fields (see TxBuilder) */
  def timeLockScriptNumbers: Gen[ScriptNumber] =
    Gen.choose(1L, UInt32.max.toLong - 1L).map(ScriptNumber(_))

  def positiveScriptNumbers: Gen[ScriptNumber] =
    Gen.choose(0L, Int64.max.toLong).map(ScriptNumber(_))

  def compactSizeUInts: Gen[CompactSizeUInt] = uInt64s.map(CompactSizeUInt(_))

  /** Generates an arbitrary [[scala.Byte Byte]] in Scala */
  def byte: Gen[Byte] = arbitrary[Byte]

  /** Generates an arbitrary [[scodec.bits.ByteVector ByteVector]] */
  def bytevector: Gen[ByteVector] = Gen.listOf(byte).map(ByteVector(_))

  /** Generates an arbitrary ByteVector of a given length */
  def bytevector(num: Int): Gen[ByteVector] =
    Gen.listOfN(num, byte).map(ByteVector(_))

  /** Generates a 100 byte sequence */
  def bytes: Gen[List[Byte]] =
    for {
      num <- Gen.choose(0, 100)
      b <- bytes(num)
    } yield b

  /**
    * Generates the number of bytes specified by num
    * @param num
    * @return
    */
  def bytes(num: Int): Gen[List[Byte]] = Gen.listOfN(num, byte)

  /** Generates a random boolean */
  def bool: Gen[Boolean] =
    for {
      num <- Gen.choose(0, 1)
    } yield num == 1

  /** Generates a bit vector */
  def bitVector: Gen[BitVector] =
    for {
      n <- Gen.choose(0, 100)
      vector <- Gen.listOfN(n, bool)
    } yield BitVector.bits(vector)

  /**
    * Generates a random GCS P parameter.
    *
    * Bit parameter for GCS, cannot be more than 32 as we will have a number too large for a UInt64.
    * @see [[https://github.com/Roasbeef/btcutil/blob/b5d74480bb5b02a15a9266cbeae37ecf9dd6ffca/gcs/gcs.go#L67]]
    */
  def genP: Gen[UInt8] = {
    Gen.choose(0, 32).map(UInt8(_))
  }

}

object NumberGenerator extends NumberGenerator
