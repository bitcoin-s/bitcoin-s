package org.bitcoins.core.script.control

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/**
  * Created by chris on 1/6/16.
  */
sealed abstract class ControlOperations extends ScriptOperation

/** Type for { OP_IF, OP_NOTIF } */
sealed abstract class ConditionalOperation extends ControlOperations

/** If the top stack value is not 0, the statements are executed. The top stack value is removed. */
case object OP_IF extends ConditionalOperation {
  override val opCode: Int = 99
}

/** If the top stack value is 0, the statements are executed. The top stack value is removed. */
case object OP_NOTIF extends ConditionalOperation {
  override val opCode: Int = 100
}

/**
  * If the preceding `OP_IF` or `OP_NOTIF` or `OP_ELSE` was not executed then these statements are and
  * if the preceding `OP_IF` or `OP_NOTIF` or `OP_ELSE` was executed then these statements are not.
  */
case object OP_ELSE extends ControlOperations {
  override val opCode: Int = 103
}

/**
  * Ends an if/else block. All blocks must end, or the transaction is invalid.
  * An `OP_ENDIF` without `OP_IF` earlier is also invalid.
  */
case object OP_ENDIF extends ControlOperations {
  override val opCode: Int = 104
}

/** Marks transaction as invalid if top stack value is not true. */
case object OP_VERIFY extends ControlOperations {
  override val opCode: Int = 105
}

/**
  * Marks transaction as invalid. A standard way of attaching extra data to transactions is to add a zero-value
  * output with a scriptPubKey consisting of `OP_RETURN` followed by exactly one pushdata op.
  * Such outputs are provably unspendable, reducing their cost to the network.
  * Currently it is usually considered non-standard
  * (though valid) for a transaction to have more than one `OP_RETURN` output or an `OP_RETURN` output
  * with more than one pushdata op.
  */
case object OP_RETURN extends ControlOperations {
  override val opCode: Int = 106
}

object ControlOperations extends ScriptOperationFactory[ControlOperations] {
  override val operations =
    Vector(OP_ELSE, OP_ENDIF, OP_IF, OP_NOTIF, OP_RETURN, OP_VERIFY)
}
