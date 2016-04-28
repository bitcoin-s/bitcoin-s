package org.bitcoins.util

import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.script.flag.{ScriptFlag, ScriptFlagUtil}
import org.bitcoins.script.{ScriptProgram, ExecutionInProgressScriptProgram, ScriptSettings}
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_CHECKSIGVERIFY}
import org.bitcoins.script.reserved.{OP_RESERVED, NOP, ReservedOperation}

import scala.annotation.tailrec

/**
 * Created by chris on 3/2/16.
 */
trait BitcoinScriptUtil {

  /**
   * Takes in a sequence of script tokens and converts them to their hexadecimal value
 *
   * @param asm
   * @return
   */
  def asmToHex(asm : Seq[ScriptToken]) : String = {
    val hex = asm.map(_.hex).mkString
    hex
  }


  /**
   * Converts a sequence of script tokens to them to their byte values
 *
   * @param asm
   * @return
   */
  def asmToBytes(asm : Seq[ScriptToken]) : Seq[Byte] = BitcoinSUtil.decodeHex(asmToHex(asm))

  /**
   * Filters out push operations in our sequence of script tokens
   * this removes OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4 and all ByteToPushOntoStack tokens
 *
   * @param asm
   * @return
   */
  def filterPushOps(asm : Seq[ScriptToken]) : Seq[ScriptToken] = {
    asm.filterNot(op => op.isInstanceOf[BytesToPushOntoStack]
      || op == OP_PUSHDATA1
      || op == OP_PUSHDATA2
      || op == OP_PUSHDATA4)
  }

  /**
   * Returns true if the given script token counts towards our max script operations in a script
   * See https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L269-L271
   * which is how bitcoin core handles this
 *
   * @param token
   * @return
   */
  def countsTowardsScriptOpLimit(token : ScriptToken) : Boolean = token match {
    case scriptOp : ScriptOperation if (scriptOp.opCode > OP_16.opCode) => true
    case _ : ScriptToken => false
  }


  /**
   * Counts the amount of sigops in a script
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/script.cpp#L156-L202
 *
   * @param script the script whose sigops are being counted
   * @return the number of signature operations in the script
   */
  def countSigOps(script : Seq[ScriptToken]) : Long = {
    val checkSigCount = script.count(token => token == OP_CHECKSIG || token == OP_CHECKSIGVERIFY)
    val multiSigOps = Seq(OP_CHECKMULTISIG,OP_CHECKMULTISIGVERIFY)
    val multiSigCount : Long = script.zipWithIndex.map { case (token, index) =>
      if (multiSigOps.contains(token) && index != 0) {
        script(index-1) match {
          case scriptNum : ScriptNumber => scriptNum.num
          case scriptConstant : ScriptConstant => BitcoinSUtil.hexToLong(scriptConstant.hex)
          case _ : ScriptToken => ScriptSettings.maxPublicKeysPerMultiSig
        }
      } else 0
    }.sum
    checkSigCount + multiSigCount
  }


  /**
   * Parses the number of signatures on the stack
   * This can only be called when an OP_CHECKMULTISIG operation is about to be executed
   * on the stack
   * For instance if this was a 2/3 multisignature script, it would return the number 3
 *
   * @param program
   * @return
   */
  def numPossibleSignaturesOnStack(program : ScriptProgram) : ScriptNumber = {
    require(program.script.headOption == Some(OP_CHECKMULTISIG) || program.script.headOption == Some(OP_CHECKMULTISIGVERIFY),
    "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op")
    val nPossibleSignatures : ScriptNumber  = program.stack.head match {
      case s : ScriptNumber => s
      case s : ScriptConstant => ScriptNumber(s.bytes)
      case _ : ScriptToken => throw new RuntimeException("n must be a script number or script constant for OP_CHECKMULTISIG")
    }
    nPossibleSignatures
  }

  /**
   * Returns the number of required signatures on the stack, for instance if this was a
   * 2/3 multisignature script, it would return the number 2
 *
   * @param program
   * @return
   */
  def numRequiredSignaturesOnStack(program : ScriptProgram) : ScriptNumber = {
    require(program.script.headOption == Some(OP_CHECKMULTISIG) || program.script.headOption == Some(OP_CHECKMULTISIGVERIFY),
      "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op")
    val nPossibleSignatures = numPossibleSignaturesOnStack(program)
    val stackWithoutPubKeys = program.stack.tail.slice(nPossibleSignatures.num.toInt,program.stack.tail.size)
    val mRequiredSignatures : ScriptNumber = stackWithoutPubKeys.head match {
      case s: ScriptNumber => s
      case s : ScriptConstant => ScriptNumber(s.bytes)
      case _ : ScriptToken => throw new RuntimeException("m must be a script number or script constant for OP_CHECKMULTISIG")
    }
    mRequiredSignatures
  }


  /**
   * Determines if a script contains only script operations
   * This is equivalent to
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/script.cpp#L213
 *
   * @param script
   * @return
   */
  def isPushOnly(script : Seq[ScriptToken]) : Boolean = {
    def loop(tokens: Seq[ScriptToken], accum: List[Boolean]): Seq[Boolean] = tokens match {
      case h :: t => h match {
        case scriptOp: ScriptOperation => loop(t, (scriptOp.opCode < OP_16.opCode) :: accum)
        case _ : ScriptToken => loop(t, true :: accum)
      }
      case Nil => accum
    }
    !loop(script, List()).exists(_ == false)
  }


  /**
   * Determines if the token being pushed onto the stack is being pushed by the SMALLEST push operation possible
   * This is equivalent to
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L209
 *
   * @param pushOp the operation that is pushing the data onto the stack
   * @param token the token that is being pushed onto the stack by the pushOp
   * @return
   */
  def isMinimalPush(pushOp : ScriptToken, token : ScriptToken) : Boolean = token match {
    case scriptNumOp : ScriptNumberOperation =>
      scriptNumOp == pushOp
    case ScriptConstant.zero | ScriptConstant.negativeZero =>
      //weird case where OP_0 pushes an empty byte vector on the stack, NOT "00" or "81"
      //so we can push the constant "00" or "81" onto the stack with a BytesToPushOntoStack pushop
      pushOp == BytesToPushOntoStack(1).get
    case _ : ScriptToken if ( token.bytes.size == 1 && ScriptNumberOperation.fromNumber(token.toLong.toInt).isDefined) =>
      //could have used the ScriptNumberOperation to push the number onto the stack
      false
    case token : ScriptToken => token.bytes.size match {
      case size if (size == 0) => pushOp == OP_0
      case size if (size == 1 && token.bytes.head == OP_1NEGATE.opCode) =>
        pushOp == OP_1NEGATE
      case size if (size <= 75) => token.bytes.size == pushOp.toLong
      case size if (size <= 255) => pushOp == OP_PUSHDATA1
      case size if (size <= 65535) => pushOp == OP_PUSHDATA2
      case size =>
        //default case is true because we have to use the largest push op as possible which is OP_PUSHDATA4
        true
    }
  }


  /**
   * Whenever a script constant is interpreted to a number BIP62 could enforce that number to be encoded
   * in the smallest encoding possible
   * https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L220-L237
 *
   * @param constant
   * @return
   */
  def isShortestEncoding(constant : ScriptConstant) : Boolean = {
    // If the most-significant-byte - excluding the sign bit - is zero
    // then we're not minimal. Note how this test also rejects the
    // negative-zero encoding, 0x80.
    if ((constant.bytes.size > 0 && (constant.bytes.last & 0x7f) == 0)) {
      // One exception: if there's more than one byte and the most
      // significant bit of the second-most-significant-byte is set
      // it would conflict with the sign bit. An example of this case
      // is +-255, which encode to 0xff00 and 0xff80 respectively.
      // (big-endian).
      if (constant.bytes.size <= 1 || (constant.bytes(constant.bytes.size - 2) & 0x80) == 0) {
        false
      } else true
     } else true
  }

  /**
   * Checks the public key encoding according to bitcoin core's function
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L202
 *
   * @param key the key whose encoding we are checking
   * @param program the program whose flags which dictate the rules for the public keys encoding
   * @return if the key is encoded correctly against the rules give in the flags parameter
   */
  def checkPubKeyEncoding(key : ECPublicKey, program : ScriptProgram) : Boolean = checkPubKeyEncoding(key,program.flags)

  /**
   * Checks the public key encoding according to bitcoin core's function
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L202
 *
   * @param key the key whose encoding we are checking
   * @param flags the flags which dictate the rules for the public keys encoding
   * @return if the key is encoded correctly against the rules givein the flags parameter
   */
  def checkPubKeyEncoding(key : ECPublicKey, flags : Seq[ScriptFlag]) : Boolean = {
    if (ScriptFlagUtil.requireStrictEncoding(flags) &&
      !isCompressedOrUncompressedPubKey(key)) false else true
  }


  /**
   * Returns true if the key is compressed or uncompressed, false otherwise
   * https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L66
 *
   * @param key the public key that is being checked
   * @return true if the key is compressed/uncompressed otherwise false
   */
  def isCompressedOrUncompressedPubKey(key : ECPublicKey) : Boolean = {
    if (key.bytes.size < 33) {
      //  Non-canonical public key: too short
      return false
    }
    if (key.bytes.head == 0x04) {
      if (key.bytes.size != 65) {
        //  Non-canonical public key: invalid length for uncompressed key
        return false
      }
    } else if (key.bytes.head == 0x02 || key.bytes.head == 0x03) {
      if (key.bytes.size != 33) {
        //  Non-canonical public key: invalid length for compressed key
        return false
      }
    } else {
      //  Non-canonical public key: neither compressed nor uncompressed
      return false
    }
    return true
  }
}


object BitcoinScriptUtil extends BitcoinScriptUtil
