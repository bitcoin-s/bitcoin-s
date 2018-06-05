package org.bitcoins.core.script.crypto

import org.bitcoins.core.script.ScriptOperationFactory

/**
  * Created by chris on 3/24/16.
  */
trait CryptoSignatureEvaluationFactory
    extends ScriptOperationFactory[CryptoSignatureEvaluation] {

  /** The current [[CryptoSignatureEvaluation]] operations. */
  def operations =
    Seq(OP_CHECKMULTISIG,
        OP_CHECKMULTISIGVERIFY,
        OP_CHECKSIG,
        OP_CHECKSIGVERIFY)

}

object CryptoSignatureEvaluationFactory extends CryptoSignatureEvaluationFactory
