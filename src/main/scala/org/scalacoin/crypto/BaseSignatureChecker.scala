package org.scalacoin.crypto

import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.Transaction

/**
 * Created by chris on 2/16/16.
 */
trait BaseSignatureChecker {


  def checkSignature(scriptSignature : ScriptSignature, pubKey : ECPublicKey)
}
