package org.scalacoin.script.control

import org.scalacoin.script.constant._
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/6/16.
 */
trait ControlOperationsInterpreter {


  private def logger = LoggerFactory.getLogger(this.getClass())
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
    val (opElseIndex,opEndIfIndex) = findIndexesOpElseOpEndIf(script)
    stack.head match {
      case OP_0 =>
        //need to remove the statements from the script since
        //they should not be executed

        require(opEndIfIndex.isDefined,"Every OP_IF must have a matching OP_ENDIF statement")
        //means that we have an else statement which needs to be executed
        if (opElseIndex.isDefined) {
          //removes the OP_ELSE as well
          val newScript = script.slice(opElseIndex.get,script.size)
          opElse(stack.tail,newScript)
        } else {
          //means that we do not have an OP_ELSE statement
          //removes the OP_ENDIF as well
          val newScript = script.slice(opEndIfIndex.get+1,script.size)
          (stack.tail,newScript)
        }
      case _ =>
        if (opElseIndex.isDefined) {
          logger.debug("OP_ELSE index: " + opElseIndex.get)
          logger.debug("OP_ENDIF index: " + opEndIfIndex.get)
          //means we have an OP_ELSE expression that needs to be removed
          //start at index 1 to remove the OP_IF
          val scriptPart1 = script.slice(1,opElseIndex.get)

          val scriptWithoutOpElse = script.zipWithIndex.filter(_._2 != opElseIndex.get).map(_._1)
          //val scriptPart2 = script.slice(opEndIfIndex.get,script.size)
          //val newScript = scriptPart1 ++ scriptPart2


          val newOpElseIndex = findOpElse(scriptWithoutOpElse)

          val scriptPart2 = if (newOpElseIndex.isDefined) {
            //the +1 is because we removed the OP_ELSE
            script.slice(newOpElseIndex.get+1,script.size)
          } else script.slice(opEndIfIndex.get,script.size)
          val newScript = scriptPart1 ++ scriptPart2
          (stack.tail,newScript)
        } else (stack.tail,script.tail)
    }
  }

  /**
   * Evaluates the OP_ELSE operator
   * @param stack
   * @param script
   * @return
   */
  def opElse(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_ELSE, "First script opt must be OP_ELSE")
    (stack,script.tail)
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
  def findOpEndIf(script : List[ScriptToken]) : Option[Int] = {
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
  def findOpElse(script : List[ScriptToken]) : Option[Int] = {
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
  def findIndexesOpElseOpEndIf(script : List[ScriptToken]) : (Option[Int],Option[Int]) = {
    val indexOpElse = findOpElse(script)
    val indexOpEndIf = findOpEndIf(script)
    (indexOpElse,indexOpEndIf)
  }
}
