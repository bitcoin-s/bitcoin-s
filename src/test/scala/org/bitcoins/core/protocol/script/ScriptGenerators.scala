package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.CryptoGenerators
import org.bitcoins.core.util.StringGenerators
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
trait ScriptGenerators {


  def p2pkScriptSignature : Gen[P2PKScriptSignature] = for {
    digitalSignature <- CryptoGenerators.digitalSignatures
  } yield P2PKScriptSignature(digitalSignature)

  def p2pkhScriptSignature : Gen[P2PKHScriptSignature] = for {
    privKey <- CryptoGenerators.privateKey
    hexString <- StringGenerators.hexString
    signature = privKey.sign(hexString)
  } yield P2PKHScriptSignature(signature,privKey.publicKey)

}

object ScriptGenerators extends ScriptGenerators
