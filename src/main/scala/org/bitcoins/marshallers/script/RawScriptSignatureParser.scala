package org.bitcoins.marshallers.script

import org.bitcoins.marshallers.RawBitcoinSerializer
import org.bitcoins.protocol.script._
import org.bitcoins.script.constant.{ScriptConstant, ScriptToken}
import org.bitcoins.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG}
import org.bitcoins.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature] with BitcoinSLogger  {

  def read(bytes : List[Byte]) : ScriptSignature = {
    val scriptTokens : List[ScriptToken] = ScriptParser.fromBytes(bytes)
    logger.info("Script tokens inside of RawScriptSig: " + scriptTokens)
    ScriptSignature.fromAsm(scriptTokens)
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex



}

object RawScriptSignatureParser extends RawScriptSignatureParser
