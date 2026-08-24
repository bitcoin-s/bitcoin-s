package org.bitcoins.core.script.reserved

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** The OP_SUCCESSx opcodes introduced by BIP342 (Tapscript). Encountering any
  * of these opcodes during Tapscript execution causes script validation to
  * succeed immediately, unlike the other reserved/NOP opcodes.
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki#new-op_success-opcodes BIP342]]
  */
case class OP_SUCCESSx(opCode: Int) extends ScriptOperation {
  require(opCode >= 187 && opCode <= 254,
          s"OP_SUCCESSx opcode must be in [187, 254], got $opCode")

  override def toString: String = s"OP_SUCCESS$opCode"
}

object OpSuccessOperation extends ScriptOperationFactory[OP_SUCCESSx] {

  override val operations: Vector[OP_SUCCESSx] =
    (187 to 254).map(OP_SUCCESSx(_)).toVector
}
