package org.scalacoin.crypto

import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.constant.ScriptToken
import org.scalacoin.script.crypto.{SIGHASH_SINGLE, HashType}

/**
 * Created by chris on 2/16/16.
 */
trait BaseSignatureChecker {


  def checkSignature(scriptSignature : ScriptSignature, pubKey : ECPublicKey)

  //https://github.com/bitcoin/bitcoin/blob/93c85d458ac3e2c496c1a053e1f5925f55e29100/src/script/interpreter.cpp#L1109
  def signatureHash(scriptPubKey : Seq[ScriptToken], txTo : Transaction, nIn : Int, hashType : HashType) = {
    require(nIn < txTo.inputs.size, "Cannot spend an input that does not exist")
    require(hashType != SIGHASH_SINGLE && nIn > txTo.outputs.size, "Cannot hash for a signature of a output that does not exist" )

    //val serialization = TransactionSignatureSerializer.serialize(txTo)
    ???
  }
}
