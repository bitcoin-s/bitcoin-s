package org.scalacoin.util

import org.scalacoin.script.{ScriptProgram, ExecutionInProgressScriptProgram, ScriptSettings}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_CHECKSIGVERIFY}
import org.scalacoin.script.reserved.{OP_RESERVED, NOP, ReservedOperation}

/**
 * Created by chris on 3/2/16.
 */
trait BitcoinScriptUtil {

  /**
   * Takes in a sequence of script tokens and converts them to their hexadecimal value
   * @param asm
   * @return
   */
  def asmToHex(asm : Seq[ScriptToken]) : String = {
    val hex = asm.map(_.hex).mkString
    hex
  }


  /**
   * Converts a sequence of script tokens to them to their byte values
   * @param asm
   * @return
   */
  def asmToBytes(asm : Seq[ScriptToken]) : Seq[Byte] = BitcoinSUtil.decodeHex(asmToHex(asm))

  /**
   * Filters out push operations in our sequence of script tokens
   * this removes OP_PUSHDATA1, OP_PUSHDATA2, OP_PUSHDATA4 and all ByteToPushOntoStack tokens
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
   * @param program
   * @return
   */
  def numPossibleSignaturesOnStack(program : ScriptProgram) : Int = {
    require(program.script.headOption == Some(OP_CHECKMULTISIG) || program.script.headOption == Some(OP_CHECKMULTISIGVERIFY),
    "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op")
    val nPossibleSignatures : Int  = program.stack.head match {
      case s : ScriptNumber => s.num.toInt
      case s : ScriptConstant => BitcoinSUtil.hexToInt(s.hex)
      case _ : ScriptToken => throw new RuntimeException("n must be a script number or script constant for OP_CHECKMULTISIG")
    }
    nPossibleSignatures
  }

  /**
   * Returns the number of required signatures on the stack, for instance if this was a
   * 2/3 multisignature script, it would return the number 2
   * @param program
   * @return
   */
  def numRequiredSignaturesOnStack(program : ScriptProgram) : Int = {
    require(program.script.headOption == Some(OP_CHECKMULTISIG) || program.script.headOption == Some(OP_CHECKMULTISIGVERIFY),
      "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op")
    val nPossibleSignatures = numPossibleSignaturesOnStack(program)
    val stackWithoutPubKeys = program.stack.tail.slice(nPossibleSignatures,program.stack.tail.size)
    val mRequiredSignatures : Int = stackWithoutPubKeys.head match {
      case s: ScriptNumber => s.num.toInt
      case s : ScriptConstant => BitcoinSUtil.hexToInt(s.hex)
      case _ : ScriptToken => throw new RuntimeException("m must be a script number or script constant for OP_CHECKMULTISIG")
    }
    mRequiredSignatures
  }

}


object BitcoinScriptUtil extends BitcoinScriptUtil
