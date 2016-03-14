package org.scalacoin.marshallers.script

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.script._
import org.scalacoin.script.constant.{ScriptConstant, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG}
import org.scalacoin.util.{BitcoinSUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/12/16.
 */
trait RawScriptSignatureParser extends RawBitcoinSerializer[ScriptSignature]  {

  private def logger = LoggerFactory.getLogger(this.getClass())
  def read(bytes : List[Byte]) : ScriptSignature = {
    val scriptTokens : List[ScriptToken] = ScriptParser.fromBytes(bytes)
    logger.info("Script tokens inside of RawScriptSig: " + scriptTokens)
    ScriptSignature.fromAsm(scriptTokens)
  }

  def write(scriptSig : ScriptSignature) : String = scriptSig.hex



}

object RawScriptSignatureParser extends RawScriptSignatureParser
