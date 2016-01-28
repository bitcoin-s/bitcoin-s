package org.scalacoin.script.control

import org.scalacoin.script.constant._
import org.scalacoin.util.{Leaf, Node, Empty, BinaryTree}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

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
/*
  def opIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_IF, "Script top was not OP_IF")
    val (opElseIndex,opEndIfIndex) = findFirstIndexesOpElseOpEndIf(script)
    stack.head match {
      case OP_0 =>
        //means that we need to execute the OP_ELSE statement if one exists
        //need to remove the OP_IF expression from the script
        //since it should not be executed
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
        //means that we need to execute the OP_IF expression
        //and delete its corresponding OP_ELSE if one exists
        if (opElseIndex.isDefined) {
          logger.debug("OP_ELSE index: " + opElseIndex.get)
          logger.debug("OP_ENDIF index: " + opEndIfIndex.get)
          //means we have an OP_ELSE expression that needs to be removed
          //start at index 1 to remove the OP_IF
          val scriptPart1 = script.slice(1,opElseIndex.get)

          val scriptWithoutOpElse = script.zipWithIndex.filter(_._2 != opElseIndex.get).map(_._1)

          val newOpElseIndex = findFirstOpElse(scriptWithoutOpElse)

          //means that we have another OP_ELSE before our OP_ENDIF.
          val scriptPart2 = if (newOpElseIndex.isDefined && newOpElseIndex.get < opEndIfIndex.get) {
            //the +1 is because we removed the OP_ELSE
            script.slice(newOpElseIndex.get+1,script.size)
          } else script.slice(opEndIfIndex.get,script.size)

          val newScript = scriptPart1 ++ scriptPart2
          (stack.tail,newScript)
        } else (stack.tail,script.tail)
    }
  }
*/


  def opIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_IF, "Script top was not OP_IF")
    val binaryTree = parseBinaryTree(script)
    if (stack.head != OP_0) {
      //remove the OP_ELSE if one exists
      val newTree : Option[BinaryTree[ScriptToken]] = binaryTree.left
      if (newTree.isDefined) (stack.tail,newTree.get.toSeq.toList) else (stack.tail,List())
    } else {
      //remove the OP_IF
      val scriptWithoutOpIf = removeFirstOpIf(script)
      if (scriptWithoutOpIf.headOption == Some(OP_ENDIF)) {
        val scriptWithoutOpEndIf = opEndIf(stack,scriptWithoutOpIf)
        (scriptWithoutOpEndIf._1.tail, scriptWithoutOpEndIf._2)
      } else (stack.tail,scriptWithoutOpIf)
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
   * Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value output
   * with a scriptPubKey consisting of OP_RETURN followed by exactly one pushdata op. Such outputs are provably unspendable,
   * reducing their cost to the network. Currently it is usually considered non-standard (though valid) for a transaction to
   * have more than one OP_RETURN output or an OP_RETURN output with more than one pushdata op.
   * @param stack
   * @param script
   * @return
   */
  def opReturn(stack : List[ScriptToken], script : List[ScriptToken]) : Boolean = {
    require(script.headOption.isDefined && script.head == OP_RETURN)
    false
  }


  /**
   * Parses a list of script tokens into its corresponding binary tree
   * @param script
   * @return
   */
  def parseBinaryTree(script : List[ScriptToken]) : BinaryTree[ScriptToken] = {

    def loop(script : List[ScriptToken]) : BinaryTree[ScriptToken] = script match {
      case OP_ENDIF  :: t => Leaf(OP_ENDIF)
      case OP_IF :: t  =>
        val lastOpEndIfIndex = findLastOpEndIf(t)
        val lastOpElseIndex = findLastOpElse(t)

        if (lastOpEndIfIndex.isDefined && !t.contains(OP_IF)) {
          val opIfExpression = t.slice(0, lastOpEndIfIndex.get)
          val restOfScript = t.slice(lastOpEndIfIndex.get, script.size)
          Node(OP_IF, loop(opIfExpression), loop(restOfScript))
        } else if (lastOpElseIndex.isDefined) {
          val opIfExpression = t.slice(0,lastOpElseIndex.get)
          val restOfScript = t.slice(lastOpElseIndex.get,script.size)
          Node(OP_IF,loop(opIfExpression),loop(restOfScript))
        } else Node(OP_IF,loop(t),Empty)

      case OP_ELSE :: t =>
        val lastOpEndIf = findLastOpEndIf(t)
        if (lastOpEndIf.isDefined) {
          val opElseExpression = t.slice(0,lastOpEndIf.get)
          val restOfScript = t.slice(lastOpEndIf.get,t.size)
          Node(OP_ELSE, loop(opElseExpression), loop(restOfScript))
        } else Node(OP_ELSE,loop(t),Empty)

      case (x : ScriptConstant) :: t => Node(x,loop(t),Empty)
      case (x : ScriptNumber) :: t => Node(x,loop(t),Empty)
      case scriptToken :: t => Node(scriptToken,loop(t),Empty)
      case Nil => Empty
    }

    val bTree = loop(script)
    bTree
  }

  /**
   * Returns the first index of an OP_ENDIF
   * @param script
   * @return
   */
  def findFirstOpEndIf(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ENDIF)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }


  /**
   * Finds the last OP_ENDIF in the given script
   * @param script
   * @return
   */
  def findLastOpEndIf(script : List[ScriptToken]) : Option[Int] = {
    val lastOpEndIf = findFirstOpEndIf(script.reverse)
    if (lastOpEndIf.isDefined) Some(script.size - lastOpEndIf.get - 1)
    else None
  }

  /**
   * Returns the first index of an OP_ENDIF
   * @param script
   * @return
   */
  def findFirstOpElse(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ELSE)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /**
   * Returns the index of the last OP_ELSE statement
   * @param script
   * @return
   */
  def findLastOpElse(script : List[ScriptToken]) : Option[Int] = {
    val zipped = script.zipWithIndex.filter(_._1 == OP_ELSE)
    if (zipped.size > 0) Some(zipped.last._2) else None
  }


  /**
   * Removes the first OP_ELSE expression encountered in the script
   * @param script
   * @return
   */
  def removeFirstOpElse(script : List[ScriptToken]) : List[ScriptToken] = {
    if (script.contains(OP_ELSE)) {
      val firstOpElseIndex = findFirstOpElse(script)
      val scriptWithoutFirstOpElse = script.zipWithIndex.filter(_._2 != firstOpElseIndex.get).map(_._1)
      val nextOpElseIndex = findFirstOpElse(scriptWithoutFirstOpElse)
      if(nextOpElseIndex.isDefined) {
        script.slice(0,firstOpElseIndex.get) ++ script.slice(nextOpElseIndex.get + 1, script.size)
      } else {
        val firstOpEndIfIndex = findFirstOpEndIf(script)
        script.slice(0,firstOpElseIndex.get) ++ script.slice(firstOpEndIfIndex.get, script.size)
      }
    } else script
  }



  /**
   * Removes the first OP_IF { expression } encountered in the script
   * @param script
   * @return
   */
  def removeFirstOpIf(script : List[ScriptToken]) : List[ScriptToken] = {
    val firstOpIfIndex = script.indexOf(OP_IF)
    val matchingOpEndIfIndex = findMatchingOpEndIf(script)

    val opIfExpression = script.slice(firstOpIfIndex,matchingOpEndIfIndex)
    val hasNestedIfExpression = opIfExpression.filter(_ == OP_IF).size > 1
    val (firstOpElseIndex,_) = findFirstIndexesOpElseOpEndIf(opIfExpression)

    if (firstOpElseIndex.isDefined && !hasNestedIfExpression) {
      script.slice(0,firstOpIfIndex) ++ script.slice(firstOpElseIndex.get,script.size)
    } else if (opIfExpression.contains(OP_IF)) {
      //if there is an OP_IF inside of the sub script then any OP_ELSES inside of there are bound to
      //that OP_IF
      script.slice(0,firstOpIfIndex) ++ script.slice(matchingOpEndIfIndex,script.size)
    }  else script.slice(0,firstOpIfIndex) ++ script.slice(matchingOpEndIfIndex,script.size)

  }

  /**
   * Finds the indexes of our OP_ELSE (if it exists) and our OP_ENDIF
   * @param script
   * @return
   */
  def findFirstIndexesOpElseOpEndIf(script : List[ScriptToken]) : (Option[Int],Option[Int]) = {
    val indexOpElse = findFirstOpElse(script)
    val indexOpEndIf = findFirstOpEndIf(script)
    (indexOpElse,indexOpEndIf)
  }


  /**
   * Returns the index of the matching OP_ENDIF for the OP_IF statement
   * @param script
   * @return
   */
  def findMatchingOpEndIf(script : List[ScriptToken]) : Int = {
    val matchingOpEndIfIndex = findLastOpEndIf(script)
    require(matchingOpEndIfIndex.isDefined, "Every OP_IF must have a matching OP_ENDIF: " + script)
    matchingOpEndIfIndex.get
  }
}
