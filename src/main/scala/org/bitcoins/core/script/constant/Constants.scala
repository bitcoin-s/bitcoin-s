package org.bitcoins.core.script.constant

import org.bitcoins.core.number.Int64
import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.util.{ BitcoinSUtil, BitcoinScriptUtil, Factory }

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 1/6/16.
 */

/**
 * This is the root class of Script. Every element in the Script language is a
 * ScriptToken - think of this the same way you think about Object in Java.
 */
sealed trait ScriptToken {
  /** The hexadecimal representation of this [[ScriptToken]]. */
  def hex: String

  /** The byte representation of this [[ScriptToken]]. */
  def bytes: Seq[Byte] = BitcoinSUtil.decodeHex(hex)

  /** The conversion from the byte representation of a [[ScriptToken]] to a number. */
  def toLong = ScriptNumberUtil.toLong(hex)
}

/**
 * A script operation is an instruction that takes an input and gives an output
 * Think of these as functions.
 */
trait ScriptOperation extends ScriptToken {
  def opCode: Int

  override def hex: String = BitcoinSUtil.encodeHex(opCode.toByte)
}

/** A constant in the Script language for instance as String or a number. */
sealed abstract class ScriptConstant extends ScriptToken {
  /** Returns if the [[ScriptConstant]] is encoded in the shortest possible way. */
  def isShortestEncoding: Boolean = BitcoinScriptUtil.isShortestEncoding(this)
}

/** Represents a [[ScriptNumber]] in the Script language. */
sealed abstract class ScriptNumber extends ScriptConstant {
  def +(that: ScriptNumber): ScriptNumber = ScriptNumber(underlying + that.underlying)

  def unary_- = ScriptNumber(-underlying)

  def -(that: ScriptNumber): ScriptNumber = ScriptNumber(underlying - that.underlying)

  def *(that: ScriptNumber): ScriptNumber = ScriptNumber(underlying * that.underlying)

  def <(that: ScriptNumber): Boolean = underlying < that.underlying

  def <=(that: ScriptNumber): Boolean = underlying <= that.underlying

  def >(that: ScriptNumber): Boolean = underlying > that.underlying

  def >=(that: ScriptNumber): Boolean = underlying >= that.underlying

  def <(that: Int64): Boolean = underlying < that.toLong

  def <=(that: Int64): Boolean = underlying <= that.toLong

  def >(that: Int64): Boolean = underlying > that.toLong

  def >=(that: Int64): Boolean = underlying >= that.toLong

  def &(that: ScriptNumber): ScriptNumber = ScriptNumber(underlying & that.underlying)

  def &(that: Int64): ScriptNumber = ScriptNumber(underlying & that.toLong)

  def |(that: ScriptNumber): ScriptNumber = ScriptNumber(underlying | that.underlying)

  /**
   * This equality just checks that the underlying scala numbers are equivalent, NOT if the numbers
   * are bitwise equivalent in Script. For instance ScriptNumber(0x01).numEqual(ScriptNumber(0x00000000001)) == true
   * but (ScriptNumber(0x01) == (ScriptNumber(0x00000000001))) == false.
   */
  def numEqual(that: ScriptNumber): Boolean = underlying == that.underlying

  def toInt = {
    val l = toLong
    require(l <= Int.MaxValue && l >= Int.MinValue)
    l.toInt
  }

  override def toLong = underlying

  /** The underlying number of the [[ScriptNumber]]. */
  protected def underlying: Long
}

object ScriptNumber extends Factory[ScriptNumber] {

  /** Represents the number zero inside of bitcoin's script language. */
  lazy val zero: ScriptNumber = ScriptNumberImpl(0, "")
  /** Represents the number one inside of bitcoin's script language. */
  lazy val one: ScriptNumber = ScriptNumberImpl(1)
  /** Represents the number negative one inside of bitcoin's script language. */
  lazy val negativeOne: ScriptNumber = ScriptNumberImpl(-1)
  /** Bitcoin has a numbering system which has a negative zero. */
  lazy val negativeZero: ScriptNumber = fromHex("80")

  def fromBytes(bytes: Seq[Byte]) = {
    if (bytes.isEmpty) zero
    else ScriptNumberImpl(ScriptNumberUtil.toLong(bytes), BitcoinSUtil.encodeHex(bytes))
  }

  def apply(underlying: Long): ScriptNumber = {
    if (underlying == 0) zero else apply(ScriptNumberUtil.longToHex(underlying))
  }

  def apply(bytes: Seq[Byte], requireMinimal: Boolean): Try[ScriptNumber] = apply(BitcoinSUtil.encodeHex(bytes), requireMinimal)

  def apply(hex: String, requireMinimal: Boolean): Try[ScriptNumber] = {
    if (requireMinimal && !BitcoinScriptUtil.isShortestEncoding(hex)) {
      Failure(new IllegalArgumentException("The given hex was not the shortest encoding for the script number: " + hex))
    } else {
      val number = apply(hex)
      Success(number)
    }
  }

  /**
   * This represents a [[ScriptNumber]] inside of bitcoin
   *
   * @param underlying the number being represented
   * @param hex        the hex representation of the number - this can be different than the obvious value for
   *                   the number. For instance we could have padded the number with another word of zeros
   */
  private case class ScriptNumberImpl(underlying: Long, override val hex: String) extends ScriptNumber

  /**
   * Companion object for [[ScriptNumberImpl]] that gives us access to more constructor types for the
   * [[ScriptNumberImpl]] case class.
   */
  private object ScriptNumberImpl {
    def apply(hex: String): ScriptNumber = ScriptNumberImpl(ScriptNumberUtil.toLong(hex), hex)

    def apply(bytes: Seq[Byte]): ScriptNumber = ScriptNumberImpl(ScriptNumberUtil.toLong(bytes))

    def apply(underlying: Long): ScriptNumber = ScriptNumberImpl(underlying, ScriptNumberUtil.longToHex(underlying))

    def apply(int64: Int64): ScriptNumber = ScriptNumberImpl(int64.toLong)
  }

}

/** The next byte contains the number of bytes to be pushed onto the stack. */
case object OP_PUSHDATA1 extends ScriptOperation {
  override def opCode = 76

  /** The maximum amount of bytes OP_PUSHDATA1 can push onto the stack. */
  def max = 255
}

/** The next two bytes contain the number of bytes to be pushed onto the stack. */
case object OP_PUSHDATA2 extends ScriptOperation {
  override def opCode = 77

  /** The max amount of data that OP_PUSHDATA2 can push onto the stack. */
  def max = 65535
}

/** The next four bytes contain the number of bytes to be pushed onto the stack. */
case object OP_PUSHDATA4 extends ScriptOperation {
  override def opCode = 78

  /** The maximum amount of data that OP_PUSHDATA4 can be push on the stack. */
  def max = 4294967295L
}

/**
 * Represents a [[ScriptNumberOperation]] where the the number in the operation is pushed onto the stack
 * i.e. OP_0 would be push 0 onto the stack, OP_1 would be push 1 onto the stack.
 */
sealed abstract class ScriptNumberOperation extends ScriptNumber with ScriptOperation {
  override def hex = opCode.toHexString
}

/** An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.) */
case object OP_0 extends ScriptNumberOperation {
  override def opCode = 0

  override def hex = "00"

  override def underlying = 0
}

/** An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.) */
case object OP_FALSE extends ScriptNumberOperation {
  override def opCode = OP_0.opCode

  override def hex = OP_0.hex

  override def underlying = OP_0.underlying

  override def bytes = OP_0.bytes
}

/** The number 1 is pushed onto the stack. */
case object OP_TRUE extends ScriptNumberOperation {
  override def opCode = 81

  override def underlying = 1
}

/** The number -1 is pushed onto the stack. */
case object OP_1NEGATE extends ScriptNumberOperation {
  override def opCode = 79

  override def underlying = -1
}

/** The number 1 is pushed onto the stack. */
case object OP_1 extends ScriptNumberOperation {
  override def opCode = OP_TRUE.opCode

  override def underlying = OP_TRUE.underlying
}

/** The number 2 is pushed onto the stack. */
case object OP_2 extends ScriptNumberOperation {
  override def opCode = 82

  override def underlying = 2
}

/** The number 3 is pushed onto the stack. */
case object OP_3 extends ScriptNumberOperation {
  override def opCode = 83

  override def underlying = 3
}

/** The number 4 is pushed onto the stack. */
case object OP_4 extends ScriptNumberOperation {
  override def opCode = 84

  override def underlying = 4
}

/** The number 5 is pushed onto the stack. */
case object OP_5 extends ScriptNumberOperation {
  override def opCode = 85

  override def underlying = 5
}

/** The number 6 is pushed onto the stack. */
case object OP_6 extends ScriptNumberOperation {
  override def opCode = 86

  override def underlying = 6
}

/** The number 7 is pushed onto the stack. */
case object OP_7 extends ScriptNumberOperation {
  override def opCode = 87

  override def underlying = 7
}

/** The number 8 is pushed onto the stack. */
case object OP_8 extends ScriptNumberOperation {
  override def opCode = 88

  override def underlying = 8
}

/** The number 9 is pushed onto the stack. */
case object OP_9 extends ScriptNumberOperation {
  override def opCode = 89

  override def underlying = 9
}

/** The number 10 is pushed onto the stack. */
case object OP_10 extends ScriptNumberOperation {
  override def opCode = 90

  override def underlying = 10
}

/** The number 11 is pushed onto the stack. */
case object OP_11 extends ScriptNumberOperation {
  override def opCode = 91

  override def underlying = 11
}

/** The number 12 is pushed onto the stack. */
case object OP_12 extends ScriptNumberOperation {
  override def opCode = 92

  override def underlying = 12
}

/** The number 13 is pushed onto the stack. */
case object OP_13 extends ScriptNumberOperation {
  override def opCode = 93

  override def underlying = 13
}

/** The number 14 is pushed onto the stack. */
case object OP_14 extends ScriptNumberOperation {
  override def opCode = 94

  override def underlying = 14
}

/** The number 15 is pushed onto the stack. */
case object OP_15 extends ScriptNumberOperation {
  override def opCode = 95

  override def underlying = 15
}

/** The number 16 is pushed onto the stack. */
case object OP_16 extends ScriptNumberOperation {
  override def opCode = 96

  override def underlying = 16
}

object ScriptNumberOperation extends ScriptOperationFactory[ScriptNumberOperation] {

  /** Finds the [[ScriptNumberOperation]] based on the given integer. */
  def fromNumber(underlying: Long): Option[ScriptNumberOperation] = operations.find(_.underlying == underlying)

  def operations = Seq(OP_0, OP_1, OP_1NEGATE, OP_2, OP_3, OP_4, OP_5, OP_6, OP_7, OP_8, OP_9, OP_10, OP_11, OP_12, OP_13, OP_14, OP_15, OP_16)

}

object ScriptConstant extends Factory[ScriptConstant] {

  lazy val zero = ScriptConstant("00")
  lazy val negativeZero = ScriptConstant("80")
  lazy val negativeOne = ScriptConstant("81")

  /** Creates a [[ScriptConstant]] from a sequence of bytes. */
  def fromBytes(bytes: Seq[Byte]): ScriptConstant = ScriptConstantImpl(BitcoinSUtil.encodeHex(bytes))

  /** Represent a public key or hash of a public key on our stack. */
  private case class ScriptConstantImpl(hex: String) extends ScriptConstant {
    def this(bytes: List[Byte]) = this(BitcoinSUtil.encodeHex(bytes))
  }

}
