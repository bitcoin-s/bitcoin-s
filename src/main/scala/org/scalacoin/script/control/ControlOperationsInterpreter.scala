package org.scalacoin.script.control

import org.scalacoin.script.constant._

/**
 * Created by chris on 1/6/16.
 */
trait ControlOperationsInterpreter {


  /**
   * Marks transaction as invalid if top stack value is not true.
   * @param stack
   * @param script
   * @return
   */
  def verify(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken],List[ScriptToken],Boolean) = {
    require(stack.size > 0, "Stack must not be empty to verify it")
    require(script.headOption.isDefined && script.head == OP_VERIFY, "Top of script stack must be OP_VERIFY")
    if (stack.head == ScriptTrue) (stack.tail,script.tail,true) else (stack.tail,script.tail,false)
  }


  /**
   * If the top stack value is not 0, the statements are executed. The top stack value is removed.
   * @param stack
   * @param script
   * @return
   */
  def opIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_IF, "Script top was not OP_IF")
    stack.head match {
      case OP_0 =>
        //need to remove the statements from the script since
        //they should not be executed
        val indexes = findIndexesOP_ELSE_OP_ENDIF(script.tail)
        require(indexes._2.isDefined,"Every OP_IF must have a matching OP_ENDIF statement")
        //means that we have an else statement which needs to be executed
        if (indexes._1.isDefined) {
          //removes the OP_ELSE as well
          val newScript = script.tail.slice(indexes._1.get+1,script.size)
          (stack.tail,newScript)
        } else {
          //means that we do not have an OP_ELSE statement
          //removes the OP_ENDIF as well
          val newScript = script.tail.slice(indexes._2.get+1,script.size)
          (stack.tail,newScript)
        }
      case _ => (stack.tail,script.tail)
    }
  }


  /**
   * If the top stack value is 0, the statements are executed. The top stack value is removed.
   * @param stack
   * @param script
   * @return
   */
  def opNotIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_NOTIF, "Script top was not OP_NOTIF")
    //since OP_NOTIF does the exact opposite of OP_NOTIF, we can just replace the stack/script tops with
    //the opposites and get the same functionality
    if (stack.head == OP_0) opIf(OP_1 :: stack.tail,OP_IF :: script.tail)
    else opIf(OP_0 :: stack.tail, OP_IF :: script.tail)
  }


  /**
   * Evaluates the OP_ENDIF operator
   * @param stack
   * @param script
   * @return
   */
  def opEndIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_ENDIF, "Script top must be OP_ENDIF")
    (stack,script.tail)
  }

  /**
   * Returns the first index of an OP_ENDIF
   * @param script
   * @return
   */
  def findOP_ENDIF(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ENDIF)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /**
   * Returns the first index of an OP_ENDIF
   * @param script
   * @return
   */
  def findOP_ELSE(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ELSE)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /**
   * Finds the indexes of our OP_ELSE (if it exists) and our OP_ENDIF
   * @param script
   * @return
   */
  def findIndexesOP_ELSE_OP_ENDIF(script : List[ScriptToken]) : (Option[Int],Option[Int]) = {
    val indexOP_ELSE = findOP_ELSE(script)
    val indexOP_ENDIF = findOP_ENDIF(script)
    (indexOP_ELSE,indexOP_ENDIF)
  }
}
