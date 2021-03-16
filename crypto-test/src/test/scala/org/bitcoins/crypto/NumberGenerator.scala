package org.bitcoins.crypto

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}

/** Created by chris on 6/16/16.
  */
trait NumberGenerator {

  def positiveShort: Gen[Short] = {
    Gen.chooseNum[Short](0, Short.MaxValue)
  }

  /** Creates a generator that generates positive long numbers */
  def positiveLongs: Gen[Long] = Gen.choose(0, Long.MaxValue)

  /** Integers between 0 and Int.MaxValue
    */
  val positiveInts: Gen[Int] = Gen.choose(0, Int.MaxValue)

  /** Integers between Int.MinValue and -1
    */
  val negativeInts: Gen[Int] = Gen.choose(Int.MinValue, -1)

  /** Random integers
    */
  val ints: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)

  /** Creates a generator for positive longs without the number zero */
  def positiveLongsNoZero: Gen[Long] = Gen.choose(1, Long.MaxValue)

  /** Creates a number generator that generates negative long numbers */
  def negativeLongs: Gen[Long] = Gen.choose(Long.MinValue, -1)

  /** Chooses a BigInt in the ranges of 0 <= bigInt < 2^^64 */
  def bigInts: Gen[BigInt] =
    Gen
      .chooseNum(Long.MinValue, Long.MaxValue)
      .map(x => BigInt(x) + BigInt(2).pow(63))

  def positiveBigInts: Gen[BigInt] = bigInts.filter(_ >= 0)

  def bigIntsUInt64Range: Gen[BigInt] =
    positiveBigInts.filter(_ < (BigInt(1) << 64))

  /** Generates an arbitrary [[scala.Byte Byte]] in Scala */
  def byte: Gen[Byte] = arbitrary[Byte]

  /** Generates an arbitrary [[scodec.bits.ByteVector ByteVector]] */
  def bytevector: Gen[ByteVector] = Gen.listOf(byte).map(ByteVector(_))

  def bytevector(length: Int): Gen[ByteVector] =
    Gen.listOfN(length, byte).map(ByteVector(_))

  /** Generates a 100 byte sequence */
  def bytes: Gen[List[Byte]] =
    for {
      num <- Gen.choose(0, 100)
      b <- bytes(num)
    } yield b

  /** Generates the number of bytes specified by num
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

}

object NumberGenerator extends NumberGenerator
