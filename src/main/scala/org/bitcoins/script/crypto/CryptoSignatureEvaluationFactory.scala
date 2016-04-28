package org.bitcoins.script.crypto

import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.util.Factory

/**
 * Created by chris on 3/24/16.
 */
trait CryptoSignatureEvaluationFactory extends ScriptOperationFactory[CryptoSignatureEvaluation]  {

  /**
   * The current crypto signature evaluation operations
 *
   * @return the sequence of crypto signature evaluation operations
   */
  def operations = Seq(OP_CHECKMULTISIG,OP_CHECKMULTISIGVERIFY,OP_CHECKSIG, OP_CHECKSIGVERIFY)

}

object CryptoSignatureEvaluationFactory extends CryptoSignatureEvaluationFactory
