package org.scalacoin.script.constant

import org.scalacoin.util.{BitcoinSUtil}

/**
 * Created by chris on 1/6/16.
 */

/**
 * This is the root class of Script. Every element in the Script language is a
 * ScriptToken - think of this the same way you think about Object in Java.
 */
sealed trait ScriptToken {
  /**
   * The hexadecimal representation of this script token
   * @return
   */
  def hex : String

  /**
   * The byte representation of this script token
   * @return
   */
  def bytes = BitcoinSUtil.decodeHex(hex)

  /**
   * The conversion from the byte representation of a token to a number
   * @return
   */
  def toLong = BitcoinSUtil.hexToLong(hex)
}

/**
 * A script operation is an instruction that takes an input and gives an output
 * Think of these as functions
 */
trait ScriptOperation extends ScriptToken {
  def opCode : Int
  override def hex : String = BitcoinSUtil.encodeHex(opCode.toByte)
}

/**
 * A constant in the Script language for instance as String or a number
 */
sealed trait ScriptConstant extends ScriptToken


/**
 * Represents a number in the Script language
 */
sealed trait ScriptNumber extends ScriptConstant {
  /**
   * The underlying number of the ScriptNumber
   * @return
   */
  def num : Long

  def + (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num + that.num)

  def - = ScriptNumberFactory.fromNumber(-num)
  def - (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num - that.num)
  def * (that : ScriptNumber) : ScriptNumber = ScriptNumberFactory.fromNumber(num * that.num)

  def < (that : ScriptNumber) : Boolean = num < that.num
  def <= (that : ScriptNumber) : Boolean = num <= that.num
  def > (that : ScriptNumber) : Boolean = num > that.num
  def >= (that : ScriptNumber) : Boolean = num >= that.num

  /**
   * This equality just checks that the underlying scala numbers are equivalent, NOT if the numbers
   * are bitwise equivalent in Script. For instance ScriptNumber(0x01).numEqual(ScriptNumber(0x00000000001)) == true
   * but (ScriptNumber(0x01) == (ScriptNumber(0x00000000001))) == false
   * @param that
   * @return
   */
  def numEqual(that : ScriptNumber) : Boolean = num == that.num
}

/**
 * This represents a script number inside of bitcoin
 * @param num the number being represented
 * @param hex the hex representation of the number - this can be different than the obvious value for
 *            the number. For instance we could have padded the number with another word of zeros
 */
case class ScriptNumberImpl(num : Long, override val hex : String) extends ScriptNumber


/**
 * Companion object for ScriptNumberImpl that gives us access to more constructor types for the
 * ScriptNumberImpl case class
 */
object ScriptNumberImpl {
  def apply(num : Long) : ScriptNumber = ScriptNumberImpl(num, BitcoinSUtil.longToHex(num))
  def apply(hex : String) : ScriptNumber = ScriptNumberImpl(BitcoinSUtil.hexToLong(hex), hex)
  def apply(bytes : Seq[Byte]) : ScriptNumber = ScriptNumberImpl(BitcoinSUtil.encodeHex(bytes))
}

sealed trait ScriptBoolean extends ScriptNumber

//TODO: Need to remove ScriptTrue & ScriptFalse - make OP_TRUE/FALSE inherit from ScriptBoolean
case object ScriptTrue extends ScriptBoolean {
  override def hex = OP_TRUE.hex
  override def num = OP_TRUE.num
}

case object ScriptFalse extends ScriptBoolean {
  override def hex = OP_FALSE.hex
  override def num = OP_FALSE.num
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


}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_0 extends ScriptNumberOperation {
  override def opCode = 0
  override def hex = "00"
  override def num = 0
  /*override def scriptNumber = ScriptNumberFactory.zero*/
}
/**
 * An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
 */
case object OP_FALSE extends ScriptNumberOperation {
  override def opCode = OP_0.opCode
  override def hex = OP_0.hex
  /*override def scriptNumber = OP_0.scriptNumber*/
  //empty byte vector
  override def num = OP_0.num
  override def bytes = OP_0.bytes
}

/**
 * The number 1 is pushed onto the stack.
 */
case object OP_TRUE extends ScriptNumberOperation {
  override def opCode = 81
  override def num = 1
  /*override def scriptNumber = ScriptNumberFactory.one*/
}

/**
 * The number -1 is pushed onto the stack.
 */
case object OP_1NEGATE extends ScriptNumberOperation {
  override def opCode = 79
  override def num = -1
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(-1)*/
}


/**
 * The number 1 is pushed onto the stack.
 */
case object OP_1 extends ScriptNumberOperation {
  override def opCode = OP_TRUE.opCode
  override def num = OP_TRUE.num
  /*override def scriptNumber = OP_TRUE.scriptNumber*/
}

/**
 * The number 2 is pushed onto the stack.
 */
case object OP_2 extends ScriptNumberOperation {
  override def opCode = 82
  override def num = 2
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(2)*/
}

/**
 * The number 3 is pushed onto the stack.
 */
case object OP_3 extends ScriptNumberOperation {
  override def opCode = 83
  override def num = 3
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(3)*/
}

/**
 * The number 4 is pushed onto the stack.
 */
case object OP_4 extends ScriptNumberOperation {
  override def opCode = 84
  override def num = 4
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(4)*/
}

/**
 * The number 5 is pushed onto the stack.
 */
case object OP_5 extends ScriptNumberOperation {
  override def opCode = 85
  override def num = 5
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(5)*/
}

/**
 * The number 6 is pushed onto the stack.
 */
case object OP_6 extends ScriptNumberOperation {
  override def opCode = 86
  override def num = 6
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(6)*/
}

/**
 * The number 7 is pushed onto the stack.
 */
case object OP_7 extends ScriptNumberOperation {
  override def opCode = 87
  override def num = 7
  /*override def scriptNumber = ScriptNumberFactory.fromNumber(7)*/
}

/**
 * The number 8 is pushed onto the stack.
 */
case object OP_8 extends ScriptNumberOperation {
  override def opCode = 88
  override def num = 8
  //override def scriptNumber = ScriptNumberFactory.fromNumber(8)
}

/**
 * The number 9 is pushed onto the stack.
 */
case object OP_9 extends ScriptNumberOperation {
  override def opCode = 89
  override def num = 9
  //override def scriptNumber = ScriptNumberFactory.fromNumber(9)
}

/**
 * The number 10 is pushed onto the stack.
 */
case object OP_10 extends ScriptNumberOperation {
  override def opCode = 90
  override def num = 10
  //override def scriptNumber = ScriptNumberFactory.fromNumber(10)
}

/**
 * The number 11 is pushed onto the stack.
 */
case object OP_11 extends ScriptNumberOperation {
  override def opCode = 91
  override def num = 11
  //override def scriptNumber = ScriptNumberFactory.fromNumber(11)
}

/**
 * The number 12 is pushed onto the stack.
 */
case object OP_12 extends ScriptNumberOperation {
  override def opCode = 92
  override def num = 12
  //override def scriptNumber = ScriptNumberFactory.fromNumber(12)
}

/**
 * The number 13 is pushed onto the stack.
 */
case object OP_13 extends ScriptNumberOperation {
  override def opCode = 93
  override def num = 13
  //override def scriptNumber = ScriptNumberFactory.fromNumber(13)
}

/**
 * The number 14 is pushed onto the stack.
 */
case object OP_14 extends ScriptNumberOperation {
  override def opCode = 94
  override def num = 14
  //override def scriptNumber = ScriptNumberFactory.fromNumber(14)
}

/**
 * The number 15 is pushed onto the stack.
 */
case object OP_15 extends ScriptNumberOperation {
  override def opCode = 95
  override def num = 15
  //override def scriptNumber = ScriptNumberFactory.fromNumber(15)
}

/**
 * The number 16 is pushed onto the stack.
 */
case object OP_16 extends ScriptNumberOperation {
  override def opCode = 96
  override def num = 16
  //override def scriptNumber = ScriptNumberFactory.fromNumber(16)
}




