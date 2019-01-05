package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.script._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinScriptUtil
import scodec.bits.ByteVector

/**
  * Created by chris on 1/12/16.
  */
sealed abstract class RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] {

  def read(bytes: ByteVector): ScriptSignature = {
    if (bytes.isEmpty) EmptyScriptSignature
    else {
      BitcoinScriptUtil.parseScript(
        bytes = bytes,
        f = ScriptSignature.fromAsm)
    }
  }

  def write(scriptSig: ScriptSignature): ByteVector = scriptSig.bytes
}

object RawScriptSignatureParser extends RawScriptSignatureParser