package org.scalacoin.script.control

import org.scalacoin.script.{ScriptProgramFactory, ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.constant._
import org.scalacoin.util._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ControlOperationsInterpreter {


  private def logger = LoggerFactory.getLogger(this.getClass())

  /**
   * If the top stack value is not 0, the statements are executed. The top stack value is removed.
   * @param program
   * @return
   */
  def opIf(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_IF, "Script top was not OP_IF")
    val binaryTree = parseBinaryTree(program.script)
    logger.debug("Parsed binary tree: " + binaryTree)
    if (program.stackTopIsTrue) {
      //if the left branch contains and OP_IF & OP_ENDIF there must be a nested OP_IF
      //remove OP_ELSE from binary tree
      val newTreeWithoutOpElse = removeFirstOpElse(binaryTree)
      val newScript = newTreeWithoutOpElse.toList
      ScriptProgramFactory.factory(program, program.stack.tail,newScript.tail)
    } else {
      //remove the OP_IF
      val scriptWithoutOpIf : BinaryTree[ScriptToken] = removeFirstOpIf(binaryTree)
      ScriptProgramFactory.factory(program, program.stack.tail,scriptWithoutOpIf.toList)
    }

  }


  /**
   * If the top stack value is 0, the statements are executed. The top stack value is removed.
   * @param program
   * @return
   */
  def opNotIf(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_NOTIF, "Script top was not OP_NOTIF")
    val binaryTree = parseBinaryTree(program.script)
    logger.debug("Parsed binary tree: " + binaryTree)
    if (program.stackTopIsTrue) {
      //remove the OP_NOTIF
      val scriptWithoutOpIf : BinaryTree[ScriptToken] = removeFirstOpIf(binaryTree)
      ScriptProgramFactory.factory(program, program.stack.tail,scriptWithoutOpIf.toList)
    } else {
      //if the left branch contains and OP_NOTIF & OP_ENDIF there must be a nested OP_IF or OP_NOTIF
      //remove OP_ELSE from binary tree
      val newTreeWithoutOpElse = removeFirstOpElse(binaryTree)
      val newScript = newTreeWithoutOpElse.toList
      ScriptProgramFactory.factory(program, program.stack.tail,newScript.tail)
    }
  }

  /**
   * Evaluates the OP_ELSE operator
   * @param program
   * @return
   */
  def opElse(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ELSE, "First script opt must be OP_ELSE")
    val tree = parseBinaryTree(program.script)
    val treeWithNextOpElseRemoved = tree match {
      case Empty => Empty
      case leaf : Leaf[ScriptToken] => leaf
      case node : Node[ScriptToken] =>
        if (node.r.value == Some(OP_ELSE)) {
          val replacementTree = node.r.left.getOrElse(Empty).findFirstDFS[ScriptToken](OP_ENDIF)().getOrElse(Empty)
          val replacementNode = replacementTree match {
            case Empty => Empty
            case leaf : Leaf[ScriptToken] => Node(leaf.v, Empty, node.r.right.getOrElse(Empty))
            case node1 : Node[ScriptToken] => Node(node1.v,node1.l,node.r.right.getOrElse(Empty))
          }
          Node(node.v,node.l,replacementNode)
        }
        else node
    }
    ScriptProgramFactory.factory(program, program.stack,treeWithNextOpElseRemoved.toList.tail)
  }


  /**
   * Evaluates an OP_ENDIF operator
   * @param program
   * @return
   */
  def opEndIf(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_ENDIF, "Script top must be OP_ENDIF")
    ScriptProgramFactory.factory(program, program.stack,program.script.tail)
  }


  /**
   * Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value output
   * with a scriptPubKey consisting of OP_RETURN followed by exactly one pushdata op. Such outputs are provably unspendable,
   * reducing their cost to the network. Currently it is usually considered non-standard (though valid) for a transaction to
   * have more than one OP_RETURN output or an OP_RETURN output with more than one pushdata op.
   * @param program
   * @return
   */
  def opReturn(program : ScriptProgram) : Boolean = {
    require(program.script.headOption.isDefined && program.script.head == OP_RETURN)
    false
  }


  /**
   * Marks transaction as invalid if top stack value is not true.
   * @param program
   * @return
   */
  def opVerify(program : ScriptProgram) : ScriptProgram = {
    //TODO: There is a bug here, if the value is cast to an int and is != 0 then we need to pop the cast values
    //off of the stack..
    require(program.script.headOption.isDefined && program.script.head == OP_VERIFY, "Script top must be OP_VERIFY")
    require(program.stack.size > 0, "Stack must have at least one element on it to run OP_VERIFY")
    if (program.stack.head != OP_0 && program.stack.head != ScriptFalse ) {
      ScriptProgramFactory.factory(program, program.stack.tail,program.script.tail,true)
    } else if (program.stack.exists(t => t != OP_0 && t != ScriptFalse)) {
      ScriptProgramFactory.factory(program, program.stack.tail,program.script.tail,true)
    } else ScriptProgramFactory.factory(program, program.stack,program.script.tail, false)
  }


  /**
   * Parses a list of script tokens into its corresponding binary tree
   * @param script
   * @return
   */
  def parseBinaryTree(script : List[ScriptToken]) : BinaryTree[ScriptToken] = {
    val bTree = loop(script,Empty)
    bTree
  }

  /**
   * The loop that parses a list of script tokens into a binary tree
   * @param script
   * @return
   */
  @tailrec
  private def loop(script : List[ScriptToken], tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
/*    logger.debug("Script : " + script)
    logger.debug("Tree: " + tree)*/
    script match {
      case OP_IF :: t =>
        val (newTail, parsedTree) = parseOpIf(script, Empty)
        val newTree = insertSubTree(tree,parsedTree)
        loop(newTail, newTree)
      case OP_NOTIF :: t =>
        val (newTail, parsedTree) = parseOpNotIf(script, Empty)
        val newTree = insertSubTree(tree,parsedTree)
        loop(newTail, newTree)
      case OP_ELSE :: t =>
        val (newTail, parsedTree) = parseOpElse(script, Empty)
        val newTree = insertSubTree(tree,parsedTree)
        loop(newTail, newTree)
      case OP_ENDIF :: t =>
        val (newTail, parsedTree) = parseOpEndIf(script, Empty)
        val newTree = insertSubTree(tree,parsedTree)
        loop(newTail, newTree)
      case (x: ScriptConstant) :: Nil => insertSubTree(tree, Leaf(x))
      case (x: BytesToPushOntoStack) :: Nil => insertSubTree(tree, Leaf(x))
      case (x: ScriptConstant) :: t => loop(t, insertSubTree(tree, Leaf(x)))
      case (x: BytesToPushOntoStack) :: t => loop(t, insertSubTree(tree, Leaf(x)))
      case h :: t => loop(t,insertSubTree(tree,Leaf(h)))
      case Nil => tree
    }
  }


  /**
   * Inserts a sub tree into the parse tree of Script.
   * @param tree the parse tree of the control flow of the Script program
   * @param subTree the parse tree that needs to be inserted into the control flow of the program
   * @return the full parse tree combined
   */
  private def insertSubTree(tree : BinaryTree[ScriptToken],subTree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    //TODO: Optimize this to a tailrec function
    //logger.debug("Inserting subTree: " + subTree + " into tree: " + tree)
      tree match {
        case Empty => subTree
        case leaf : Leaf[ScriptToken] => Node(leaf.v, subTree, Empty)
        case node : Node[ScriptToken] =>
          if (subTree.value.isDefined && subTree.value.get == OP_ELSE) {
            //need to insert the OP_ELSE within the proper OP_IF
            //get count of OP_IFs and OP_ENDIFS inside of the tree
            val opIfCount = node.l.count[ScriptToken](OP_IF)
            val opNotIfCount = node.l.count[ScriptToken](OP_NOTIF)
            val opEndIfCount = node.l.count[ScriptToken](OP_ENDIF)
            //means that the subtree is not balanced, need to insert the OP_ELSE inside
            //the left subtree
            if (opIfCount + opNotIfCount != opEndIfCount) Node(node.v,insertSubTree(tree.left.get,subTree),node.r)
            else Node(node.v,node.l,insertSubTree(tree.right.getOrElse(Empty),subTree))
          } else if (node.r.value.isDefined && node.r.value.get == OP_ELSE) {
            //since there is an OP_ELSE defined to right
            //we need to insert all script tokens on that node
            Node(node.v,node.l,insertSubTree(node.r,subTree))
          }
          else Node(node.v, insertSubTree(node.l, subTree), node.r)
      }

  }
  /**
   * Parses an OP_IF expression in Script
   * @param t
   * @return
   */
  private def parseOpIf(script : List[ScriptToken],tree : BinaryTree[ScriptToken]) : (List[ScriptToken],BinaryTree[ScriptToken]) = script match {
    case OP_IF :: t => tree match {
      case n : Node[ScriptToken] => (t,Node(n.v,Node(OP_IF,Empty,Empty),n.r))
      case l : Leaf[ScriptToken] => (t,Node(l.v,Node(OP_IF,Empty,Empty),Empty))
      case Empty => (t, Node(OP_IF,Empty,Empty))
    }
    case h :: t => throw new RuntimeException("Cannot parse " + h + " as an OP_IF")
    case Nil => (script,tree)
  }

  /**
   * Parses an OP_NOTIF expression in Script
   * @param t
   * @return
   */
  private def parseOpNotIf(script : List[ScriptToken],tree : BinaryTree[ScriptToken]) : (List[ScriptToken],BinaryTree[ScriptToken]) = script match {
    case OP_NOTIF :: t => tree match {
      case n : Node[ScriptToken] => (t,Node(n.v,Node(OP_NOTIF,Empty,Empty),n.r))
      case l : Leaf[ScriptToken] => (t,Node(l.v,Node(OP_NOTIF,Empty,Empty),Empty))
      case Empty => (t, Node(OP_NOTIF,Empty,Empty))
    }
    case h :: t => throw new RuntimeException("Cannot parse " + h + " as an OP_NOTIF")
    case Nil => (script,tree)
  }

  /**
   * Parses and OP_ELSE expression
   * @param script
   * @return
   */
  private def parseOpElse(script : List[ScriptToken],tree : BinaryTree[ScriptToken]) : (List[ScriptToken],BinaryTree[ScriptToken]) = script match {
    case OP_ELSE :: t => tree match {
      case n : Node[ScriptToken] => n.r match {
        //means that the right branch is already populated
        //this is a problem because we always insert OP_ELSES on the right branch
        case Empty => (t,Node(n.v,n.l,Node(OP_ELSE,Empty,Empty)))
        case l : Leaf[ScriptToken] => (t, Node(n.v,n.l,Node(l.v, Empty, Node(OP_ELSE,Empty,Empty))))
        case n1 : Node[ScriptToken] => parseOpElse(script,n1)
      }
      case l : Leaf[ScriptToken] => (t,Node(l.v,Empty,Node(OP_ELSE,Empty,Empty)))
      case Empty => (t,Node(OP_ELSE,Empty,Empty))
    }
    case h :: t => throw new RuntimeException("Cannot parse " + h + " as an OP_ELSE")
    case Nil => (script,tree)
  }

  private def parseOpEndIf(script : List[ScriptToken],tree : BinaryTree[ScriptToken]) : (List[ScriptToken],BinaryTree[ScriptToken]) = script match {
    case OP_ENDIF :: t => tree match {
      case Empty => (t,Leaf(OP_ENDIF))
      case l : Leaf[ScriptToken] => (t,Node(l.v, Leaf(OP_ENDIF), Empty))
      case n : Node[ScriptToken] => (t, insertSubTree(tree,Leaf(OP_ENDIF)))
    }
    case h :: t => throw new RuntimeException("Cannot parse " + h + " as an OP_ENDIF")
    case Nil => (script,tree)
  }


  def checkMatchingOpIfOpEndIf(script : List[ScriptToken]) : Boolean = {
    script.count(_ == OP_IF) == script.count(_ == OP_ENDIF)
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
   * Removes the first OP_ELSE {expression} in a binary tree
   * @param tree
   * @tparam T
   * @return
   */
  def removeFirstOpElse(tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    tree match {
      case Empty => Empty
      case leaf : Leaf[ScriptToken] => leaf
      case node : Node[ScriptToken] =>
        //need to traverse the tree to see if there is an OP_ENDIF on the left hand side
        val leftBranchContainsOpElse = node.l.contains[ScriptToken](OP_ELSE)()
        val leftBranchContainsOpIf = node.l.contains[ScriptToken](OP_IF)()
        if (leftBranchContainsOpElse && !leftBranchContainsOpIf) {
          //if the left branch contains an OP_ELSE but no OP_IF
          //then we need to delete the OP_ELSE in the left branch
          val subTree: Option[BinaryTree[ScriptToken]] = node.l.findFirstDFS[ScriptToken](OP_ELSE)()
          logger.debug("Sub tree: " + subTree)
          //need to remove the subtree for the OP_ELSE
          //need to insert the right branch of the subtree into the original place of the OP_ELSE
          if (subTree.isDefined) tree.replace(subTree.get, subTree.get.right.getOrElse(Empty))
          else tree
        } else if (node.r.value == Some(OP_ELSE)) {
          logger.debug("============================**********************************")
          Node(node.v,node.l,node.r.right.getOrElse(Empty))
        } else tree
    }

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
   * Removes the first occurrence of OP_IF or OP_NOTIF in the binary tree
   * @param tree
   * @return
   */
  def removeFirstOpIf(tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    require(tree.value.isDefined && (tree.value.get == OP_IF || tree.value.get == OP_NOTIF) , "Top of the tree must be OP_IF or OP_NOTIF to remove the OP_IF or OP_NOTIF")
    if (tree.right.isDefined && tree.right.get.value == Some(OP_ELSE)) tree.right.getOrElse(Empty)
    else tree.findFirstDFS[ScriptToken](OP_ENDIF)().getOrElse(Empty)
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
