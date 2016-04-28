package org.bitcoins.script.error

/**
 * Created by chris on 4/18/16.
 */
sealed trait ScriptError {
  def description : String
}

//SCRIPT_ERR_OK = 0,
case object ScriptErrorOk extends ScriptError {
  override def description : String = "OK"
}


//SCRIPT_ERR_UNKNOWN_ERROR,
case object ScriptErrorUnknownError extends ScriptError {
  override def description : String = "UNKNOWN_ERROR"
}

//SCRIPT_ERR_EVAL_FALSE,
case object ScriptErrorEvalFalse extends ScriptError {
  override def description : String = "EVAL_FALSE"
}

//SCRIPT_ERR_OP_RETURN,
case object ScriptErrorOpReturn extends ScriptError {
  override def description : String = "OP_RETURN"
}


/* Max sizes */
//SCRIPT_ERR_SCRIPT_SIZE,
case object ScriptErrorScriptSize extends ScriptError {
  override def description : String = "SCRIPT_SIZE"
}


//SCRIPT_ERR_PUSH_SIZE,
case object ScriptErrorPushSize extends ScriptError {
  override def description : String = "PUSH_SIZE"
}

//SCRIPT_ERR_OP_COUNT,
case object ScriptErrorOpCount extends ScriptError {
  override def description : String = "OP_COUNT"
}


//SCRIPT_ERR_STACK_SIZE,
case object ScriptErrorStackSize extends ScriptError {
  override def description : String = "STACK_SIZE"
}


//SCRIPT_ERR_SIG_COUNT,
case object ScriptErrorSigCount extends ScriptError {
  override def description : String = "SIG_COUNT"
}


//SCRIPT_ERR_PUBKEY_COUNT,
case object ScriptErrorPubKeyCount extends ScriptError {
  override def description : String = "PUBKEY_COUNT"
}


/* Failed verify operations */

//SCRIPT_ERR_VERIFY,
case object ScriptErrorVerify extends ScriptError {
  override def description : String = "VERIFY"
}

//SCRIPT_ERR_EQUALVERIFY,
case object ScriptErrorEqualVerify extends ScriptError {
  override def description : String = "EQUALVERIFY"
}

//SCRIPT_ERR_CHECKMULTISIGVERIFY,
case object ScriptErrorCheckMultiSigVerify extends ScriptError {
  override def description : String = "CHECKMULTISIGVERIFY"
}

//SCRIPT_ERR_CHECKSIGVERIFY,
case object ScriptErrorCheckSigVerify extends ScriptError {
  override def description : String = "CHECKSIGVERIFY"
}

//SCRIPT_ERR_NUMEQUALVERIFY,
case object ScriptErrorNumEqualVerify extends ScriptError {
  override def description : String = "NUMEQUALVERIFY"
}


/* Logical/Format/Canonical errors */
//SCRIPT_ERR_BAD_OPCODE,
case object ScriptErrorBadOpCode extends ScriptError {
  override def description : String = "BAD_OPCODE"
}

//SCRIPT_ERR_DISABLED_OPCODE,
case object ScriptErrorDisabledOpCode extends ScriptError {
  override def description : String = "DISABLED_OPCODE"
}

//SCRIPT_ERR_INVALID_STACK_OPERATION,
case object ScriptErrorInvalidStackOperation extends ScriptError {
  override def description : String = ""
}

//SCRIPT_ERR_INVALID_ALTSTACK_OPERATION,
case object ScriptErrorInvalidAltStackOperation extends ScriptError {
  override def description : String = "INVALID_ALTSTACK_OPERATION"
}

//SCRIPT_ERR_UNBALANCED_CONDITIONAL,
case object ScriptErrorUnbalancedConditional extends ScriptError {
  override def description : String = "UNBALANCED_CONDITIONAL"
}


/* CHECKLOCKTIMEVERIFY and CHECKSEQUENCEVERIFY */
//SCRIPT_ERR_NEGATIVE_LOCKTIME,
case object ScriptErrorNegativeLockTime extends ScriptError {
  override def description : String = "NEGATIVE_LOCKTIME"
}

//SCRIPT_ERR_UNSATISFIED_LOCKTIME,
case object ScriptErrorUnsatisfiedLocktime extends ScriptError {
  override def description : String = "UNSATISFIED_LOCKTIME"
}


/* BIP62 */
//SCRIPT_ERR_SIG_HASHTYPE,
case object ScriptErrorSigHashType extends ScriptError {
  override def description : String = "SIG_HASHTYPE"
}

//SCRIPT_ERR_SIG_DER,
case object ScriptErrorSigDer extends ScriptError {
  override def description : String = "SIG_DER"
}

//SCRIPT_ERR_MINIMALDATA,
case object ScriptErrorMinimalData extends ScriptError {
  override def description : String = "MINIMALDATA"
}

//SCRIPT_ERR_SIG_PUSHONLY,
case object ScriptErrorSigPushOnly extends ScriptError {
  override def description : String = "SIG_PUSHONLY"
}

//SCRIPT_ERR_SIG_HIGH_S,
case object ScriptErrorSigHighS extends ScriptError {
  override def description : String = "SIG_HIGH_S"
}

//SCRIPT_ERR_SIG_NULLDUMMY,
case object ScriptErrorSigNullDummy extends ScriptError {
  override def description : String = "SIG_NULLDUMMY"
}

//SCRIPT_ERR_PUBKEYTYPE,
case object ScriptErrorPubKeyType extends ScriptError {
  override def description : String = "PUBKEYTYPE"
}

//SCRIPT_ERR_CLEANSTACK,
case object ScriptErrorCleanStack extends ScriptError {
  override def description : String = "CLEANSTACK"
}


/* softfork safeness */
//SCRIPT_ERR_DISCOURAGE_UPGRADABLE_NOPS,
case object ScriptErrorDiscourageUpgradableNOPs extends ScriptError {
  override def description : String = ""
}

//SCRIPT_ERR_ERROR_COUNT
case object ScriptErrorCount extends ScriptError {
  override def description : String = "DISCOURAGE_UPGRADABLE_NOPS"
}



/**
 * Factory companion object for creating ScriptError objects
 */
object ScriptError {
  def errors : Seq[ScriptError] = Seq(ScriptErrorOk,ScriptErrorUnknownError,ScriptErrorEvalFalse, ScriptErrorOpReturn,
    ScriptErrorPushSize, ScriptErrorScriptSize, ScriptErrorOpCount, ScriptErrorStackSize, ScriptErrorSigCount,
    ScriptErrorPubKeyCount,ScriptErrorVerify, ScriptErrorEqualVerify,ScriptErrorCheckSigVerify, ScriptErrorCheckMultiSigVerify,
    ScriptErrorNumEqualVerify, ScriptErrorBadOpCode,ScriptErrorDisabledOpCode,ScriptErrorInvalidStackOperation,
    ScriptErrorInvalidAltStackOperation, ScriptErrorUnbalancedConditional, ScriptErrorNegativeLockTime,
    ScriptErrorUnsatisfiedLocktime, ScriptErrorSigHashType, ScriptErrorSigDer, ScriptErrorMinimalData, ScriptErrorSigPushOnly,
    ScriptErrorSigHighS, ScriptErrorSigNullDummy,ScriptErrorPubKeyType, ScriptErrorCleanStack, ScriptErrorDiscourageUpgradableNOPs,
    ScriptErrorCount)
  def apply(str : String) : ScriptError = errors.filter(_.description == str).head
}