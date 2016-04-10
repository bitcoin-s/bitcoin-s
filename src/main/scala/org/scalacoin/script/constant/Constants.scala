package org.scalacoin.script.constant

import org.scalacoin.util.{BitcoinSUtil}

/**
 * Created by chris on 1/6/16.
 */


sealed trait ScriptToken {
  def hex : String
  def bytes = BitcoinSUtil.decodeHex(hex)
  def bytesSize = bytes.size
  def toLong = BitcoinSUtil.hexToLong(hex)
}

trait ScriptOperation extends ScriptToken {
  def opCode : Int
  override def hex : String = BitcoinSUtil.encodeHex(opCode.toByte)
}

sealed trait ScriptConstant extends ScriptToken

sealed trait ScriptNumber extends ScriptConstant {
  def num : Long

  def + (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num + that.num)
  def - (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num - that.num)
  def * (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num * that.num)

  def < (that : ScriptNumber) : Boolean = num < that.num
  def <= (that : ScriptNumber) : Boolean = num <= that.num
  def > (that : ScriptNumber) : Boolean = num > that.num
  def >= (that : ScriptNumber) : Boolean = num >= that.num
}

/**
 * This represents a script number inside of bitcoin
 * @param num the number being represented
 * @param hex the hex representation of the number - this can be different than the obvious value for
 *            the number. For instance we could have padded the number with another word of zeros
 */
case class ScriptNumberImpl(num : Long, override val hex : String) extends ScriptNumber

object ScriptNumberImpl {
  def apply(num : Long) : ScriptNumber = ScriptNumberImpl(num, BitcoinSUtil.longToHex(num))
  def apply(hex : String) : ScriptNumber = ScriptNumberImpl(BitcoinSUtil.hexToLong(hex), hex)
  def apply(bytes : Seq[Byte]) : ScriptNumber = ScriptNumberImpl(BitcoinSUtil.encodeHex(bytes))
}
sealed trait ScriptBoolean extends ScriptConstant

case object ScriptTrue extends ScriptBoolean {
  override def hex = "01"
}

case object ScriptFalse extends ScriptBoolean {
  override def hex = "00"
}

/**
 * Represent a pubkey or hash of a pub key on our stack
 * @param hex
 */
case class ScriptConstantImpl(hex : String) extends ScriptConstant {
  def this(bytes : List[Byte]) = this(BitcoinSUtil.encodeHex(bytes))
}


/**
 * 	The next byte contains the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA1 extends ScriptOperation {
  override def opCode = 76
}

/**
 * 	The next two bytes contain the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA2 extends ScriptOperation {
  override def opCode = 77
}

/**
 * The next four bytes contain the number of bytes to be pushed onto the stack.
 */
case object OP_PUSHDATA4 extends ScriptOperation {
  override def opCode = 78
}


/**
 * Represents a script number operation where the the number in the operation is pushed onto the stack
 * i.e. OP_0 would be push 0 onto the stack, OP_1 would be push 1 onto the stack
 */
sealed trait ScriptNumberOperation extends ScriptNumber with ScriptOperation {

  override def hex = opCode.toHexString
  override def num = scriptNumber.num
  /**
   * Represents the script number that needs to be pushed onto the stack
   * if the op is interpreted
   * i.e. OP_1 would be matched with the ScriptNumber(1)
   * @return
   */
  def scriptNumber : ScriptNumber


}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_0 extends ScriptNumberOperation {
  override def opCode = 0
  override def hex = "00"
  override def scriptNumber = ScriptNumberFactory.zero

  //empty byte vector
  override def bytes = List()

  override def bytesSize = 1
}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_FALSE extends ScriptNumberOperation {
  override def opCode = OP_0.opCode
  override def hex = OP_0.hex
  override def scriptNumber = OP_0.scriptNumber
  //empty byte vector
  override def bytes = List()

  override def bytesSize = 1
}

/**
 * The number 1 is pushed onto the stack.
 */
case object OP_TRUE extends ScriptNumberOperation {
  override def opCode = 81
  override def scriptNumber = ScriptNumberFactory.one
}

/**
 * The number -1 is pushed onto the stack.
 */
case object OP_1NEGATE extends ScriptNumberOperation {
  override def opCode = 79
  override def scriptNumber = ScriptNumberFactory.fromNumber(-1)
}


/**
 * The number 1 is pushed onto the stack.
 */
case object OP_1 extends ScriptNumberOperation {
  override def opCode = OP_TRUE.opCode
  override def scriptNumber = OP_TRUE.scriptNumber
}

/**
 * The number 2 is pushed onto the stack.
 */
case object OP_2 extends ScriptNumberOperation {
  override def opCode = 82
  override def scriptNumber = ScriptNumberFactory.fromNumber(2)
}

/**
 * The number 3 is pushed onto the stack.
 */
case object OP_3 extends ScriptNumberOperation {
  override def opCode = 83
  override def scriptNumber = ScriptNumberFactory.fromNumber(3)
}

/**
 * The number 4 is pushed onto the stack.
 */
case object OP_4 extends ScriptNumberOperation {
  override def opCode = 84
  override def scriptNumber = ScriptNumberFactory.fromNumber(4)
}

/**
 * The number 5 is pushed onto the stack.
 */
case object OP_5 extends ScriptNumberOperation {
  override def opCode = 85
  override def scriptNumber = ScriptNumberFactory.fromNumber(5)
}

/**
 * The number 6 is pushed onto the stack.
 */
case object OP_6 extends ScriptNumberOperation {
  override def opCode = 86
  override def scriptNumber = ScriptNumberFactory.fromNumber(6)
}

/**
 * The number 7 is pushed onto the stack.
 */
case object OP_7 extends ScriptNumberOperation {
  override def opCode = 87
  override def scriptNumber = ScriptNumberFactory.fromNumber(7)
}

/**
 * The number 8 is pushed onto the stack.
 */
case object OP_8 extends ScriptNumberOperation {
  override def opCode = 88
  override def scriptNumber = ScriptNumberFactory.fromNumber(8)
}

/**
 * The number 9 is pushed onto the stack.
 */
case object OP_9 extends ScriptNumberOperation {
  override def opCode = 89
  override def scriptNumber = ScriptNumberFactory.fromNumber(9)
}

/**
 * The number 10 is pushed onto the stack.
 */
case object OP_10 extends ScriptNumberOperation {
  override def opCode = 90
  override def scriptNumber = ScriptNumberFactory.fromNumber(10)
}

/**
 * The number 11 is pushed onto the stack.
 */
case object OP_11 extends ScriptNumberOperation {
  override def opCode = 91
  override def scriptNumber = ScriptNumberFactory.fromNumber(11)
}

/**
 * The number 12 is pushed onto the stack.
 */
case object OP_12 extends ScriptNumberOperation {
  override def opCode = 92
  override def scriptNumber = ScriptNumberFactory.fromNumber(12)
}

/**
 * The number 13 is pushed onto the stack.
 */
case object OP_13 extends ScriptNumberOperation {
  override def opCode = 93
  override def scriptNumber = ScriptNumberFactory.fromNumber(13)
}

/**
 * The number 14 is pushed onto the stack.
 */
case object OP_14 extends ScriptNumberOperation {
  override def opCode = 94
  override def scriptNumber = ScriptNumberFactory.fromNumber(14)
}

/**
 * The number 15 is pushed onto the stack.
 */
case object OP_15 extends ScriptNumberOperation {
  override def opCode = 95
  override def scriptNumber = ScriptNumberFactory.fromNumber(15)
}

/**
 * The number 16 is pushed onto the stack.
 */
case object OP_16 extends ScriptNumberOperation {
  override def opCode = 96
  override def scriptNumber = ScriptNumberFactory.fromNumber(16)
}




