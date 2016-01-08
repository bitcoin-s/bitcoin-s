package org.scalacoin.script.crypto

import org.scalacoin.script.ScriptOperationFactory

/**
 * Created by chris on 1/8/16.
 */
trait CryptoOperationFactory extends ScriptOperationFactory[CryptoOperation] {

  override def operations = Seq(OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY, OP_CHECKSIG, OP_CHECKSIGVERIFY,
    OP_CODESEPARATOR, OP_HASH160, OP_HASH256, OP_RIPEMD160, OP_SHA1, OP_SHA256)
}

object CryptoOperationFactory extends CryptoOperationFactory