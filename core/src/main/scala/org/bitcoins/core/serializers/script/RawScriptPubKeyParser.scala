package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, ScriptPubKey}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinScriptUtil
import scodec.bits.ByteVector

/**
  * Created by chris on 1/12/16.
  */
trait RawScriptPubKeyParser extends RawBitcoinSerializer[ScriptPubKey] {

  override def read(bytes: ByteVector): ScriptPubKey = {
    if (bytes.isEmpty) EmptyScriptPubKey
    else {
      BitcoinScriptUtil.parseScript(
        bytes = bytes,
        f = ScriptPubKey.fromAsm)
    }
  }

  override def write(scriptPubKey: ScriptPubKey): ByteVector = scriptPubKey.bytes
}

object RawScriptPubKeyParser extends RawScriptPubKeyParser