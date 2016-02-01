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


  def opIf(stack : List[ScriptToken], script : List[ScriptToken]) : (List[ScriptToken], List[ScriptToken]) = {
    require(script.headOption.isDefined && script.head == OP_IF, "Script top was not OP_IF")
    val binaryTree = parseBinaryTree(script)
    if (stack.head != OP_0) {
      //remove OP_ELSE from binary tree
      val newTreeWithoutOpElse = removeFirstOpElse(binaryTree)
      val newScript = newTreeWithoutOpElse.toList
      (stack.tail,newScript.tail)
    } else {
      //remove the OP_IF
      val scriptWithoutOpIf : BinaryTree[ScriptToken] = removeFirstOpIf(binaryTree)
      (stack.tail,scriptWithoutOpIf.toList)
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
    val bTree = loop(script)
    bTree
  }

  /**
   * The loop that parses a list of script tokens into a binary tree
   * @param script
   * @return
   */
  private def loop(script : List[ScriptToken]) : BinaryTree[ScriptToken] = script match {
    case OP_IF :: t  => parseOpIf(script)
    case OP_ELSE :: t => parseOpElse(script)
    case OP_ENDIF :: Nil => Leaf(OP_ENDIF)
    case OP_ENDIF :: t => Node(OP_ENDIF,loop(t),Empty)
    case (x : ScriptConstant) :: t => Node(x,loop(t),Empty)
    case (x : ScriptNumber) :: t => Node(x,loop(t),Empty)
    case scriptToken :: t => Node(scriptToken,loop(t),Empty)
    case Nil => Empty
  }


  /**
   * Parses an OP_IF expression in Script
   * @param t
   * @return
   */
  private def parseOpIf(script : List[ScriptToken]) : BinaryTree[ScriptToken] = {
    val _ :: t = script
    val lastOpEndIfIndex = findLastOpEndIf(t)
    val lastOpElseIndex = findLastOpElse(t)
    if (!t.contains(OP_IF) && lastOpEndIfIndex.isDefined) {
      //means that we do not have any nested OP_IF expressions
      val opElseIndex : Option[Int] = findFirstOpElse(t)
      //slice off the entire OP_IF expression
      val opIfExpression = if (opElseIndex.isDefined) t.slice(0,opElseIndex.get) else t.slice(0, lastOpEndIfIndex.get)
      //slice off everything that isn't the OP_IF expression
      val restOfScript = if (opElseIndex.isDefined) t.slice(opElseIndex.get, t.size) else t.slice(lastOpEndIfIndex.get, t.size)
      //build the OP_IF expression as the left subtree, the rest of the script as the right subtree
      Node(OP_IF, loop(opIfExpression), loop(restOfScript))
    } else if (t.contains(OP_IF) && lastOpEndIfIndex.isDefined) {
      //means that we have a nested OP_IF expresion
      //we need to parse the inner OP_IF expression
      val nestedOpIfIndex = findFirstOpIf(t).get
      //if we have multiple nested OP_IFs we need this to get the correct OP_ENDIF
      val nextNestedOpIfIndex = findFirstOpIf(t.slice(nestedOpIfIndex+1,t.size))

      //find the nested OP_ENDIF matching our nested OP_IF
      val nestedLastOpEndIfIndex = nextNestedOpIfIndex.isDefined match {
        //means that we need to search t until the next nested OP_IF index
        case true => findLastOpEndIf(t.slice(0,nextNestedOpIfIndex.get))
        //means that we can search all of t until the last OP_ENDIF
        case false => findLastOpEndIf(t.slice(0,lastOpEndIfIndex.get))
      }
      //every OP_IF must be matched with a OP_ENDIF
      require(nestedLastOpEndIfIndex.isDefined,"Every OP_IF must have a matching OP_ENDIF")
      //slice off the nested OP_IF expression
      //indexing starts at 0 here because we need to include script tokens that prefix the nested OP_IF
      //i.e. OP_IF OP_0 OP_IF OP_ENDIF OP_ENDIF
      val firstNestedOpIfExpression = t.slice(0,nestedLastOpEndIfIndex.get+1)
      val restOfScript = t.slice(nestedLastOpEndIfIndex.get+1,t.size)
      //parse the nested OP_IF expression as the left subtree
      //the rest of the parent OP_IF as the right subtree
      Node(OP_IF,loop(firstNestedOpIfExpression),loop(restOfScript))
    } else Node(OP_IF,loop(t),Empty)
  }


  def checkMatchingOpIfOpEndIf(script : List[ScriptToken]) : Boolean = {
    script.count(_ == OP_IF) == script.count(_ == OP_ENDIF)
  }


  /**
   * Parses and OP_ELSE expression
   * @param script
   * @return
   */
  private def parseOpElse(script : List[ScriptToken]) : BinaryTree[ScriptToken] = {
    val _ :: t = script
    val nestedOpElseIndex = findFirstOpElse(t)
    val lastOpEndIfIndex = findLastOpEndIf(t)

    if (t.count(_ == OP_ENDIF) > 1) {
      //check to see if there is are multiple OP_ENDIFs
      //find the index of the nested OP_IF
      val firstOpIfIndex = findFirstOpIf(t)
      //find the nested OP_IFs corresponding OP_ENDIF
      val nestedOpEndIfIndex = findFirstOpEndIf(t)
      //slice off the nested OP_IF expression
      //need this to start at 0 index in case there are constants ahead of the OP_IF
      //i.e. OP_0 OP_1 OP_IF ... OP_ENDIF
      val nestedOpIfExpression = t.slice(0,nestedOpEndIfIndex.get+1)

      val restOfScript = t.slice(nestedOpEndIfIndex.get+1,t.size)
      Node(OP_ELSE,loop(nestedOpIfExpression),loop(restOfScript))
    } else if (t.count(_ == OP_IF) == 1) {
      //check to see if there is a nested OP_IF inside of the OP_ELSE
      //find the index of the nested OP_IF
      val firstOpIfIndex = findFirstOpIf(t)
      //find the nested OP_IFs corresponding OP_ENDIF
      val nestedOpEndIfIndex = findLastOpEndIf(t.slice(firstOpIfIndex.get,lastOpEndIfIndex.get))
      //slice off the nested OP_IF expression
      val nestedOpIfExpression = t.slice(0,nestedOpEndIfIndex.get+1)

      val restOfScript = t.slice(nestedOpEndIfIndex.get+1,t.size)
      Node(OP_ELSE,loop(nestedOpIfExpression),loop(restOfScript))
    } else if (nestedOpElseIndex.isDefined && lastOpEndIfIndex.isDefined && nestedOpElseIndex.get < lastOpEndIfIndex.get) {
      //if we have a nested OP_ELSE before our last OP_ENDIF index
      val opElseExpression = t.slice(0,nestedOpElseIndex.get)
      val restOfScript = t.slice(nestedOpElseIndex.get,t.size)
      Node(OP_ELSE, loop(opElseExpression), loop(restOfScript))
    } else if (lastOpEndIfIndex.isDefined) {
      val opElseExpression = t.slice(0,lastOpEndIfIndex.get)
      val restOfScript = t.slice(lastOpEndIfIndex.get,t.size)
      Node(OP_ELSE,loop(opElseExpression),loop(restOfScript))
    } else Node(OP_ELSE,loop(t),Empty)
  }

  def findFirstOpIf(script : List[ScriptToken]) : Option[Int] = findFirstScriptToken(script,OP_IF)

  /**
   * Finds the first instance of a given script token
   * @param script
   * @param scriptToken
   * @return
   */
  private def findFirstScriptToken(script : List[ScriptToken], scriptToken : ScriptToken) : Option[Int] = {
    val index = script.indexOf(scriptToken)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /**
   * Finds the last instance of  a given script token
   * @param script
   * @param scriptToken
   * @return
   */
  private def findLastScriptToken(script : List[ScriptToken], scriptToken : ScriptToken) = {
    val index = script.reverse.indexOf(scriptToken)
    index match {
      case -1 => None
      case _ => Some(script.size - index)
    }
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
   * Removes the first op else in a binary tree
   * @param tree
   * @tparam T
   * @return
   */
  def removeFirstOpElse[T](tree : BinaryTree[T]) : BinaryTree[T] = {
    //need to traverse the tree to see if there is an OP_ENDIF on the left hand side

    if (tree.right.isDefined && tree.right.get.value == Some(OP_ELSE)) {
      Node(tree.value.get,tree.left.getOrElse(Empty),tree.right.get.right.getOrElse(Empty))
    } else tree
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

  def removeFirstOpIf(tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    require(tree.value.isDefined && tree.value.get == OP_IF, "Top of the tree must be OP_IF to remove the OP_IF")
    tree.right.getOrElse(Empty)
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
