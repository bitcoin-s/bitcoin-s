package org.bitcoins.core.script.control

import org.bitcoins.core.protocol.script.SigVersionWitnessV0
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.util._

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ControlOperationsInterpreter {
  private def logger = BitcoinSLogger.logger
  /** If the top stack value is not 0, the statements are executed. The top stack value is removed. */
  def opIf(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.contains(OP_IF), "Script top was not OP_IF")
    val sigVersion = program.txSignatureComponent.sigVersion
    val flags = program.flags
    val minimalIfEnabled = ScriptFlagUtil.minimalIfEnabled(flags)
    val binaryTree = parseBinaryTree(program.script)
    val stackTop = program.stack.headOption
    logger.debug("Parsed binary tree: " + binaryTree)
    if (!checkMatchingOpIfOpNotIfOpEndIf(program.originalScript)) {
      logger.error("We do not have a matching OP_ENDIF for every OP_IF we have")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else if (program.stack.isEmpty) {
      logger.error("We do not have any stack elements for our OP_IF")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else if (sigVersion == SigVersionWitnessV0 && minimalIfEnabled
      && (stackTop.get.bytes.size > 1 ||
      (stackTop.get.bytes.size == 1 && stackTop.get.bytes.head != 1))) {
      //see: https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L447-L452
      //https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2016-August/013014.html
      logger.error("OP_IF argument was not minimally encoded, got: " + stackTop)
      ScriptProgram(program, ScriptErrorMinimalIf)
    } else if (program.stackTopIsTrue) {
      logger.debug("OP_IF stack top was true")
      logger.debug("Stack top: " + program.stack)
      //if the left branch contains and OP_IF & OP_ENDIF there must be a nested OP_IF
      //remove OP_ELSE from binary tree
      val newTreeWithoutOpElse = removeFirstOpElse(binaryTree)
      val newScript = newTreeWithoutOpElse.toList
      logger.debug("New script after removing OP_ELSE branch " + newScript.tail)
      logger.debug("New stack after removing OP_ELSE branch: " + program.stack.tail)
      ScriptProgram(program, program.stack.tail,newScript.tail)
    }  else {
      logger.debug("OP_IF stack top was false")
      //remove the OP_IF
      val scriptWithoutOpIf : BinaryTree[ScriptToken] = removeFirstOpIf(binaryTree)
      ScriptProgram(program, program.stack.tail,scriptWithoutOpIf.toList)
    }
  }

  /** If the top stack value is 0, the statements are executed. The top stack value is removed. */
  def opNotIf(program : ScriptProgram) : ScriptProgram = {
    //TODO: Try and reduce this down to using OP_IF by inverting the stack top
    require(program.script.headOption.contains(OP_NOTIF), "Script top was not OP_NOTIF")
    val binaryTree = parseBinaryTree(program.script)
    val sigVersion = program.txSignatureComponent.sigVersion
    val flags = program.flags
    val minimalIfEnabled = ScriptFlagUtil.minimalIfEnabled(flags)
    val stackTop = program.stack.headOption
    logger.debug("Parsed binary tree: " + binaryTree)
    if (!checkMatchingOpIfOpNotIfOpEndIf(program.originalScript)) {
      logger.error("We do not have a matching OP_ENDIF for every OP_NOTIF we have")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else if (program.stack.isEmpty) {
      logger.error("We do not have any stack elements for our OP_NOTIF")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else if (sigVersion == SigVersionWitnessV0 && minimalIfEnabled
      && (stackTop.get.bytes.size > 1 ||
      (stackTop.get.bytes.size == 1 && stackTop.get.bytes.head != 1))) {
      //see: https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L447-L452
      //https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2016-August/013014.html
      logger.error("OP_NOTIF argument was not minimally encoded, got: " + stackTop)
      ScriptProgram(program, ScriptErrorMinimalIf)
    } else if (program.stackTopIsTrue) {
      //remove the OP_NOTIF
      val scriptWithoutOpIf : BinaryTree[ScriptToken] = removeFirstOpIf(binaryTree)
      ScriptProgram(program, program.stack.tail,scriptWithoutOpIf.toList)
    } else {
      //if the left branch contains and OP_NOTIF & OP_ENDIF there must be a nested OP_IF or OP_NOTIF
      //remove OP_ELSE from binary tree
      val newTreeWithoutOpElse = removeFirstOpElse(binaryTree)
      val newScript = newTreeWithoutOpElse.toList
      ScriptProgram(program, program.stack.tail,newScript.tail)
    }
  }

  /** Evaluates the [[OP_ELSE]] operator. */
  def opElse(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.contains(OP_ELSE), "First script opt must be OP_ELSE")
    if (!program.script.tail.contains(OP_ENDIF)) {
      logger.error("OP_ELSE does not have a OP_ENDIF")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else {
      val tree = parseBinaryTree(program.script)
      val treeWithNextOpElseRemoved = tree match {
        case Empty => Empty
        case leaf : Leaf[ScriptToken] => leaf
        case node : Node[ScriptToken] =>
          removeFirstOpElse(node)
      }
      ScriptProgram(program, program.stack,treeWithNextOpElseRemoved.toList.tail)
    }
  }

  /** Evaluates an [[OP_ENDIF]] operator. */
  def opEndIf(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.contains(OP_ENDIF), "Script top must be OP_ENDIF")
    if (!checkMatchingOpIfOpNotIfOpEndIf(program.originalScript)) {
      //means we do not have a matching OP_IF for our OP_ENDIF
      logger.error("We do not have a matching OP_IF/OP_NOTIF for every OP_ENDIF we have")
      ScriptProgram(program,ScriptErrorUnbalancedConditional)
    } else ScriptProgram(program, program.stack,program.script.tail)
  }

  /** Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value output
   * with a [[org.bitcoins.core.protocol.script.ScriptPubKey]] consisting of [[OP_RETURN]] followed by exactly one pushdata op. Such outputs are provably unspendable,
   * reducing their cost to the network. Currently it is usually considered non-standard (though valid) for a transaction to
   * have more than one OP_RETURN output or an OP_RETURN output with more than one pushdata op. */
  def opReturn(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.contains(OP_RETURN))
    ScriptProgram(program,ScriptErrorOpReturn)
  }

  /** Marks [[org.bitcoins.core.protocol.transaction.Transaction]] as invalid if top stack value is not true. */
  def opVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.contains(OP_VERIFY), "Script top must be OP_VERIFY")
    program.stack.nonEmpty match {
      case true =>
        logger.debug("Stack for OP_VERIFY: " + program.stack)
        if (program.stackTopIsFalse) ScriptProgram(program,ScriptErrorVerify)
        else ScriptProgram(program, program.stack.tail,program.script.tail)
      case false =>
        logger.error("OP_VERIFY requires an element to be on the stack")
        ScriptProgram(program,ScriptErrorInvalidStackOperation)
    }
  }

  /** Parses a list of [[ScriptToken]]s into its corresponding [[BinaryTree]] */
  def parseBinaryTree(script : List[ScriptToken]): BinaryTree[ScriptToken] = {
    //@tailrec
    def loop(remaining: List[ScriptToken], parentTree: BinaryTree[ScriptToken]): (BinaryTree[ScriptToken], List[ScriptToken]) = {
      if (remaining.isEmpty) (parentTree,Nil)
      else {
        if (parentTree.right.isDefined && parentTree.right.get.value == Some(OP_ELSE)) {
          //for the case of OP_IF OP_1 OP_ELSE OP_2 OP_ELSE OP_3 ... OP_ELSE OP_N OP_ENDIF
          val (elseTree,newRemaining) = loop(remaining, parentTree.right.getOrElse(Empty))
          val n = Node(parentTree.value.get, parentTree.left.getOrElse(Empty), elseTree)
          (n,newRemaining)
        } else {
          val (tree, newRemaining) = parse(remaining,parentTree)
          loop(newRemaining,tree)
        }
      }
    }
    val (t, remaining) = loop(script,Empty)
    require(remaining.isEmpty, "Should not have any script tokens after parsing a binary tree, got: " + remaining)
    t
  }

  /** The loop that parses a list of [[ScriptToken]]s into a [[BinaryTree]]. */
  private def parse(script : List[ScriptToken], tree : BinaryTree[ScriptToken]): (BinaryTree[ScriptToken], List[ScriptToken]) = {
    logger.debug("Tree: " + tree)
    logger.debug("script: " + (if (script.nonEmpty) script else Nil))
    script match {
      case OP_ENDIF :: t =>
        //require(t.isEmpty, "Must not have any tail after parsing an OP_ENDIF, got: "+ t)
        val ifTree = insertSubTree(tree,Leaf(OP_ENDIF))
        (ifTree,t)
      case h :: t if (h == OP_IF || h == OP_NOTIF) =>
        val (ifTree,remaining) = parse(t, Leaf(h))
        val fullTree = insertSubTree(tree,ifTree)
        (fullTree,remaining)
      case h :: t if h == OP_ELSE =>
        val (subTree,remaining) = parse(t,Node(OP_ELSE,Empty,Empty))
        val opElseTree = tree match {
          case Empty => subTree
          case l: Leaf[ScriptToken] => Node(l.v,Empty,subTree)
          case n: Node[ScriptToken] => Node(n.v,n.l,insertSubTree(n.r,subTree))
        }
        (opElseTree,remaining)
      case h :: t => parse(t,insertSubTree(tree,Leaf(h)))
      case Nil =>
        logger.debug("Done parsing tree, got: "  + tree)
        (tree,Nil)
    }
  }

  /**
   * Inserts a sub tree into the parse tree of Script.
   * @param tree the parse tree of the control flow of the Script program
   * @param subTree the parse tree that needs to be inserted into the control flow of the program
   * @return the full parse tree combined
   */
  private def insertSubTree(tree: BinaryTree[ScriptToken],
                            subTree: BinaryTree[ScriptToken]): BinaryTree[ScriptToken] = tree match {
    case Empty => subTree
    case leaf: Leaf[ScriptToken] =>
      if (subTree == Empty) leaf
      else if (subTree == Leaf(OP_ENDIF)) Node(leaf.v,Empty,subTree)
      else Node(leaf.v,subTree,Empty)
    case node : Node[ScriptToken] if (node.v == OP_IF || node.v == OP_NOTIF || node.v == OP_ELSE || node.v == OP_ENDIF) =>
      if (subTree.value.isDefined && Seq(OP_ELSE,OP_ENDIF).contains(subTree.value.get)) {
        Node(node.v,node.l,insertSubTree(node.r,subTree))
      } else if (node.r != Empty && Seq(OP_ELSE,OP_ENDIF).contains(node.r.value.get)) {
        Node(node.v,node.l,insertSubTree(node.r,subTree))
      } else {
        Node(node.v,insertSubTree(node.l,subTree),node.r)
      }
    case node: Node[ScriptToken] =>
      Node(node.v,insertSubTree(node.l,subTree), node.r)
  }

  /** Checks if an [[OP_IF]]/[[OP_NOTIF]] [[ScriptToken]] has a matching [[OP_ENDIF]] */
  def checkMatchingOpIfOpNotIfOpEndIf(script : List[ScriptToken]) : Boolean = {
    @tailrec
    def loop(script : List[ScriptToken], counter : Int) : Boolean = script match {
      case _ if (counter < 0) => false
      case OP_ENDIF :: t => loop(t,counter-1)
      case OP_IF :: t => loop(t, counter + 1)
      case OP_NOTIF :: t => loop(t, counter + 1)
      case (_: ScriptToken) :: t => loop(t, counter)
      case Nil => counter == 0
    }
    loop(script,0)
  }

  /** Returns the first index of an [[OP_ENDIF]]. */
  def findFirstOpEndIf(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ENDIF)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /** Finds the last [[OP_ENDIF]] in the given script. */
  def findLastOpEndIf(script : List[ScriptToken]) : Option[Int] = {
    val lastOpEndIf = findFirstOpEndIf(script.reverse)
    if (lastOpEndIf.isDefined) Some(script.size - lastOpEndIf.get - 1)
    else None
  }

  /** Returns the first index of an [[OP_ENDIF]]. */
  def findFirstOpElse(script : List[ScriptToken]) : Option[Int] = {
    val index = script.indexOf(OP_ELSE)
    index match {
      case -1 => None
      case _ => Some(index)
    }
  }

  /** Removes the first [[OP_ELSE]] expression encountered in the script. */
  def removeFirstOpElse(script : List[ScriptToken]) : List[ScriptToken] = {
    removeFirstOpElse(parseBinaryTree(script)).toList
  }

  /** Removes the first [[OP_ELSE]] in a [[BinaryTree]]. */
  def removeFirstOpElse(tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    //@tailrec
    def loop(child: BinaryTree[ScriptToken], parent: Node[ScriptToken]): BinaryTree[ScriptToken] = child match {
      case Empty => Empty
      case l: Leaf[ScriptToken] => l
      case Node(OP_ELSE,_,r) => r
      case n: Node[ScriptToken] =>
        Node(n.v,n.l,loop(n.r,n))
    }
    tree match {
      case Empty => Empty
      case l: Leaf[ScriptToken] => l
      case n: Node[ScriptToken] =>
        val result = Node(n.v,n.l,loop(n.r, n))
        result
    }
  }


  /** Removes the first [[OP_IF]] encountered in the script. */
  def removeFirstOpIf(script : List[ScriptToken]) : List[ScriptToken] = {
    val firstOpIfIndex = script.indexOf(OP_IF)
    val matchingOpEndIfIndex = findMatchingOpEndIf(script)

    val opIfExpression = script.slice(firstOpIfIndex,matchingOpEndIfIndex)
    val hasNestedIfExpression = opIfExpression.count(_ == OP_IF) > 1
    val (firstOpElseIndex,_) = findFirstIndexesOpElseOpEndIf(opIfExpression)

    if (firstOpElseIndex.isDefined && !hasNestedIfExpression) {
      script.slice(0,firstOpIfIndex) ++ script.slice(firstOpElseIndex.get,script.size)
    } else if (opIfExpression.contains(OP_IF)) {
      //if there is an OP_IF inside of the sub script then any OP_ELSES inside of there are bound to
      //that OP_IF
      script.slice(0,firstOpIfIndex) ++ script.slice(matchingOpEndIfIndex,script.size)
    }  else script.slice(0,firstOpIfIndex) ++ script.slice(matchingOpEndIfIndex,script.size)
  }

  /** Removes the first occurrence of [[OP_IF]] or [[OP_NOTIF]] in the [[BinaryTree]]. */
  def removeFirstOpIf(tree : BinaryTree[ScriptToken]) : BinaryTree[ScriptToken] = {
    require(tree.value.isDefined && (tree.value.get == OP_IF || tree.value.get == OP_NOTIF) , "Top of the tree must be OP_IF or OP_NOTIF to remove the OP_IF or OP_NOTIF")
    if (tree.right.isDefined && tree.right.get.value == Some(OP_ELSE)) tree.right.getOrElse(Empty)
    else tree.findFirstDFS[ScriptToken](OP_ENDIF)().getOrElse(Empty)
  }

  /** Finds the indexes of our [[OP_ELSE]] (if it exists) and our [[OP_ENDIF]]. */
  def findFirstIndexesOpElseOpEndIf(script : List[ScriptToken]) : (Option[Int],Option[Int]) = {
    val indexOpElse = findFirstOpElse(script)
    val indexOpEndIf = findFirstOpEndIf(script)
    (indexOpElse,indexOpEndIf)
  }

   /** Returns the index of the matching [[OP_ENDIF]] for the [[OP_IF]] statement. */
  def findMatchingOpEndIf(script : List[ScriptToken]) : Int = {
    val matchingOpEndIfIndex = findLastOpEndIf(script)
    require(matchingOpEndIfIndex.isDefined, "Every OP_IF must have a matching OP_ENDIF: " + script)
    matchingOpEndIfIndex.get
  }
}
