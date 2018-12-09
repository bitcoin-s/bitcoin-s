package org.bitcoins.core.script.result

sealed trait ScriptResult {
  def description: String
}

/**
  * [[https://github.com/bitcoin/bitcoin/blob/master/src/test/script_tests.cpp#L61]]
  */
sealed trait ScriptError extends ScriptResult

//SCRIPT_ERR_OK = 0,
case object ScriptOk extends ScriptResult {
  override def description: String = "OK"
}

//SCRIPT_ERR_UNKNOWN_ERROR,
case object ScriptErrorUnknownError extends ScriptError {
  override def description: String = "UNKNOWN_ERROR"
}

//SCRIPT_ERR_EVAL_FALSE,
case object ScriptErrorEvalFalse extends ScriptError {
  override def description: String = "EVAL_FALSE"
}

//SCRIPT_ERR_OP_RETURN,
case object ScriptErrorOpReturn extends ScriptError {
  override def description: String = "OP_RETURN"
}

/* Max sizes */
//SCRIPT_ERR_SCRIPT_SIZE,
case object ScriptErrorScriptSize extends ScriptError {
  override def description: String = "SCRIPT_SIZE"
}

//SCRIPT_ERR_PUSH_SIZE,
case object ScriptErrorPushSize extends ScriptError {
  override def description: String = "PUSH_SIZE"
}

//SCRIPT_ERR_OP_COUNT,
case object ScriptErrorOpCount extends ScriptError {
  override def description: String = "OP_COUNT"
}

//SCRIPT_ERR_STACK_SIZE,
case object ScriptErrorStackSize extends ScriptError {
  override def description: String = "STACK_SIZE"
}

//SCRIPT_ERR_SIG_COUNT,
case object ScriptErrorSigCount extends ScriptError {
  override def description: String = "SIG_COUNT"
}

//SCRIPT_ERR_PUBKEY_COUNT,
case object ScriptErrorPubKeyCount extends ScriptError {
  override def description: String = "PUBKEY_COUNT"
}

/* Failed verify operations */

//SCRIPT_ERR_VERIFY,
case object ScriptErrorVerify extends ScriptError {
  override def description: String = "VERIFY"
}

//SCRIPT_ERR_EQUALVERIFY,
case object ScriptErrorEqualVerify extends ScriptError {
  override def description: String = "EQUALVERIFY"
}

//SCRIPT_ERR_CHECKMULTISIGVERIFY,
case object ScriptErrorCheckMultiSigVerify extends ScriptError {
  override def description: String = "CHECKMULTISIGVERIFY"
}

//SCRIPT_ERR_CHECKSIGVERIFY,
case object ScriptErrorCheckSigVerify extends ScriptError {
  override def description: String = "CHECKSIGVERIFY"
}

//SCRIPT_ERR_NUMEQUALVERIFY,
case object ScriptErrorNumEqualVerify extends ScriptError {
  override def description: String = "NUMEQUALVERIFY"
}

/* Logical/Format/Canonical errors */
//SCRIPT_ERR_BAD_OPCODE,
case object ScriptErrorBadOpCode extends ScriptError {
  override def description: String = "BAD_OPCODE"
}

//SCRIPT_ERR_DISABLED_OPCODE,
case object ScriptErrorDisabledOpCode extends ScriptError {
  override def description: String = "DISABLED_OPCODE"
}

//SCRIPT_ERR_INVALID_STACK_OPERATION,
case object ScriptErrorInvalidStackOperation extends ScriptError {
  override def description: String = "INVALID_STACK_OPERATION"
}

//SCRIPT_ERR_INVALID_ALTSTACK_OPERATION,
case object ScriptErrorInvalidAltStackOperation extends ScriptError {
  override def description: String = "INVALID_ALTSTACK_OPERATION"
}

//SCRIPT_ERR_UNBALANCED_CONDITIONAL,
case object ScriptErrorUnbalancedConditional extends ScriptError {
  override def description: String = "UNBALANCED_CONDITIONAL"
}

/* CHECKLOCKTIMEVERIFY and CHECKSEQUENCEVERIFY */
//SCRIPT_ERR_NEGATIVE_LOCKTIME,
case object ScriptErrorNegativeLockTime extends ScriptError {
  override def description: String = "NEGATIVE_LOCKTIME"
}

//SCRIPT_ERR_UNSATISFIED_LOCKTIME,
case object ScriptErrorUnsatisfiedLocktime extends ScriptError {
  override def description: String = "UNSATISFIED_LOCKTIME"
}

/* BIP62 */
//SCRIPT_ERR_SIG_HASHTYPE,
case object ScriptErrorSigHashType extends ScriptError {
  override def description: String = "SIG_HASHTYPE"
}

//SCRIPT_ERR_SIG_DER,
case object ScriptErrorSigDer extends ScriptError {
  override def description: String = "SIG_DER"
}

//SCRIPT_ERR_MINIMALDATA,
case object ScriptErrorMinimalData extends ScriptError {
  override def description: String = "MINIMALDATA"
}

//SCRIPT_ERR_SIG_PUSHONLY,
case object ScriptErrorSigPushOnly extends ScriptError {
  override def description: String = "SIG_PUSHONLY"
}

//SCRIPT_ERR_SIG_HIGH_S,
case object ScriptErrorSigHighS extends ScriptError {
  override def description: String = "SIG_HIGH_S"
}

//SCRIPT_ERR_SIG_NULLDUMMY,
case object ScriptErrorSigNullDummy extends ScriptError {
  override def description: String = "SIG_NULLDUMMY"
}

//SCRIPT_ERR_PUBKEYTYPE,
case object ScriptErrorPubKeyType extends ScriptError {
  override def description: String = "PUBKEYTYPE"
}

//SCRIPT_ERR_CLEANSTACK,
case object ScriptErrorCleanStack extends ScriptError {
  override def description: String = "CLEANSTACK"
}

/* softfork safeness */
//SCRIPT_ERR_DISCOURAGE_UPGRADABLE_NOPS,
case object ScriptErrorDiscourageUpgradableNOPs extends ScriptError {
  override def description: String = "DISCOURAGE_UPGRADABLE_NOPS"
}

//SCRIPT_ERR_ERROR_COUNT
case object ScriptErrorCount extends ScriptError {
  override def description: String = "ERROR_COUNT"
}

//SCRIPT_ERR_MINIMALIF
case object ScriptErrorMinimalIf extends ScriptError {
  override def description = "MINIMALIF"
}
//SCRIPT_ERR_SIG_NULLFAIL
case object ScriptErrorSigNullFail extends ScriptError {
  override def description = "NULLFAIL"
}

//SCRIPT_ERR_DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM
case object ScriptErrorDiscourageUpgradeableWitnessProgram extends ScriptError {
  override def description = "DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM"
}

//SCRIPT_ERR_WITNESS_PROGRAM_WRONG_LENGTH

case object ScriptErrorWitnessProgramWrongLength extends ScriptError {
  override def description = "WITNESS_PROGRAM_WRONG_LENGTH"
}

//SCRIPT_ERR_WITNESS_PROGRAM_WITNESS_EMPTY
case object ScriptErrorWitnessProgramWitnessEmpty extends ScriptError {
  override def description = "WITNESS_PROGRAM_WITNESS_EMPTY"
}

//SCRIPT_ERR_WITNESS_PROGRAM_MISMATCH
case object ScriptErrorWitnessProgramMisMatch extends ScriptError {
  override def description = "WITNESS_PROGRAM_MISMATCH"
}

//SCRIPT_ERR_WITNESS_MALLEATED
case object ScriptErrorWitnessMalleated extends ScriptError {
  override def description = "WITNESS_MALLEATED"
}
//SCRIPT_ERR_WITNESS_MALLEATED_P2SH
case object ScriptErrorWitnessMalleatedP2SH extends ScriptError {
  override def description = "WITNESS_MALLEATED_P2SH"
}

//SCRIPT_ERR_WITNESS_UNEXPECTED
case object ScriptErrorWitnessUnexpected extends ScriptError {
  override def description = "WITNESS_UNEXPECTED"
}

//SCRIPT_ERR_WITNESS_PUBKEYTYPE
case object ScriptErrorWitnessPubKeyType extends ScriptError {
  override def description = "WITNESS_PUBKEYTYPE"
}

/**
  * Factory companion object for creating ScriptError objects
  */
object ScriptResult {

  def results: Seq[ScriptResult] =
    Seq(
      ScriptOk,
      ScriptErrorUnknownError,
      ScriptErrorEvalFalse,
      ScriptErrorOpReturn,
      ScriptErrorPushSize,
      ScriptErrorScriptSize,
      ScriptErrorOpCount,
      ScriptErrorStackSize,
      ScriptErrorSigCount,
      ScriptErrorPubKeyCount,
      ScriptErrorVerify,
      ScriptErrorEqualVerify,
      ScriptErrorCheckSigVerify,
      ScriptErrorCheckMultiSigVerify,
      ScriptErrorNumEqualVerify,
      ScriptErrorBadOpCode,
      ScriptErrorDisabledOpCode,
      ScriptErrorInvalidStackOperation,
      ScriptErrorInvalidAltStackOperation,
      ScriptErrorUnbalancedConditional,
      ScriptErrorNegativeLockTime,
      ScriptErrorUnsatisfiedLocktime,
      ScriptErrorSigHashType,
      ScriptErrorSigDer,
      ScriptErrorMinimalData,
      ScriptErrorSigPushOnly,
      ScriptErrorSigHighS,
      ScriptErrorSigNullDummy,
      ScriptErrorPubKeyType,
      ScriptErrorCleanStack,
      ScriptErrorDiscourageUpgradableNOPs,
      ScriptErrorCount,
      ScriptErrorMinimalIf,
      ScriptErrorSigNullFail,
      ScriptErrorDiscourageUpgradeableWitnessProgram,
      ScriptErrorWitnessProgramWrongLength,
      ScriptErrorWitnessProgramWitnessEmpty,
      ScriptErrorWitnessProgramMisMatch,
      ScriptErrorWitnessMalleated,
      ScriptErrorWitnessMalleatedP2SH,
      ScriptErrorWitnessUnexpected,
      ScriptErrorWitnessPubKeyType
    )

  def apply(str: String): ScriptResult =
    results.filter(_.description == str).head
}
