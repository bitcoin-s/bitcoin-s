package org.bitcoins.core.wallet.utxo

/** Represents the spending branch being taken in a ScriptPubKey's execution
  *
  * If you over-specify a path, such as giving a condition where none is needed,
  * then the remaining over-specified path will be ignored.
  *
  * For example, if you wanted to spend a ConditionalScriptPubKey(P2PK1, P2PK2)
  * (which looks like OP_IF <P2PK1> OP_ELSE <P2PK2> OP_ENDIF) with the P2PK1 case,
  * then you would construct a ConditionalSpendingInfo using nonNestedTrue as your
  * ConditionalPath. Otherwise if you wanted to use P2PK2 you would use nonNestedFalse.
  */
sealed trait ConditionalPath {
  def headOption: Option[Boolean]
}

sealed trait Conditional extends ConditionalPath {
  def condition: Boolean

  override def headOption: Option[Boolean] = Some(condition)
}

object ConditionalPath {

  case object NoCondition extends ConditionalPath {
    override val headOption: Option[Boolean] = None
  }

  case class ConditionTrue(nextCondition: ConditionalPath) extends Conditional {
    override val condition: Boolean = true
  }

  case class ConditionFalse(nextCondition: ConditionalPath)
      extends Conditional {
    override val condition: Boolean = false
  }

  val nonNestedTrue: ConditionalPath = ConditionTrue(NoCondition)
  val nonNestedFalse: ConditionalPath = ConditionFalse(NoCondition)

  def fromBranch(branch: Vector[Boolean]): ConditionalPath = {
    if (branch.isEmpty) {
      NoCondition
    } else {
      if (branch.head) {
        ConditionTrue(fromBranch(branch.tail))
      } else {
        ConditionFalse(fromBranch(branch.tail))
      }
    }
  }
}
