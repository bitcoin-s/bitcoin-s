package org.scalacoin.script.arithmetic

import org.scalacoin.script.constant.{ScriptNumber, ScriptNumberImpl, ScriptConstantImpl, ScriptToken}

/**
 * Created by chris on 1/25/16.
 */
trait ArithmeticInterpreter {

  /**
   * 	a is added to b.
   * @param stack
   * @param script
   * @return
   */
  def opAdd(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_ADD, "Script top must be OP_ADD")
    require(stack.size > 1, "Stack size must be 2 or more perform an OP_ADD")

    val b : Int  = intFromScriptToken(stack.head)
    val a : Int = intFromScriptToken(stack(1))

    val result = numberToScriptToken(a + b)
    (result :: stack.slice(2,stack.size), script.tail)
  }


  /**
   * Wraps a scala number into a script token for the script language
   * @param num
   * @return
   */
  private def numberToScriptToken(num : Int) : ScriptToken = {
    if (num < 76) ScriptNumberImpl(num)
    else ScriptConstantImpl(num.toHexString)
  }


  /**
   * Converts a script token to an integer
   * @param token
   * @return
   */
  private def intFromScriptToken(token : ScriptToken) : Int = token match {
    case x : ScriptNumber => x.opCode
    case x : ScriptConstantImpl => Integer.parseInt(x.hex,16)
  }

}
