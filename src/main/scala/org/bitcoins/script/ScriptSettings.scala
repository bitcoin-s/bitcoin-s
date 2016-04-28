package org.bitcoins.script

/**
 * Created by chris on 4/21/16.
 */
trait ScriptSettings {

  /**
   * A integer representing the maximum number of public keys you can have in a
   * OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY operation
   * @return
   */
  def maxPublicKeysPerMultiSig = 20
}

object ScriptSettings extends ScriptSettings
