package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECPublicKey, ECDigitalSignature}
import org.scalacoin.marshallers.script.{RawScriptSignatureParser, ScriptParser}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG}
import org.scalacoin.util.{Factory, BitcoinSUtil, ScalacoinUtil}

/**
 * Created by chris on 1/19/16.
 * Responsible for the instantiation of ScriptSignature objects
 */
trait ScriptSignatureFactory extends Factory[ScriptSignature] {

  /**
   * Creates a ScriptSignature object from a given hexadecimal script
   * @param hex
   * @return
   */
  def factory(hex : String) : ScriptSignature = fromHex(hex)

  /**
   * Creates a ScriptSignature object from a given list of bytes
   * @param bytes
   * @return
   */
  def factory(bytes : Seq[Byte]) : ScriptSignature = fromBytes(bytes)


  /**
   * Builds a script signature from a digital signature and a public key
   * this is a pay to public key hash script sig
   * @param signature
   * @param pubKey
   * @return
   */
  def factory(signature : ECDigitalSignature, pubKey : ECPublicKey) : ScriptSignature = {
    val signatureBytesToPushOntoStack = BytesToPushOntoStackFactory.factory(signature.bytes.size)
    val pubKeyBytesToPushOntoStack = BytesToPushOntoStackFactory.factory(pubKey.bytes.size)
    val asm : Seq[ScriptToken] = Seq(signatureBytesToPushOntoStack.get, ScriptConstantImpl(signature.hex),
      pubKeyBytesToPushOntoStack.get, ScriptConstantImpl(pubKey.hex))
    fromAsm(asm)
  }

  /**
   * Returns an empty script signature
   * @return
   */
  def empty : ScriptSignature = EmptyScriptSignature

  def fromBytes(bytes : Seq[Byte]) : ScriptSignature =  {
    RawScriptSignatureParser.read(bytes)
  }


  /**
   * Creates a scriptSignature from the list of script tokens
   * @param tokens
   * @return
   */
  def fromAsm(tokens : Seq[ScriptToken]) : ScriptSignature = {
    val scriptSigHex = tokens.map(_.hex).mkString
    tokens match {
      case Seq() => EmptyScriptSignature
      case _  if (tokens.contains(OP_CHECKMULTISIG) && tokens.count(_.isInstanceOf[ScriptNumberOperation]) == 3) =>
        P2SHScriptSignatureImpl(scriptSigHex,tokens)
      case _ if (tokens.size > 0 && tokens.head == OP_0) => MultiSignatureScriptSignatureImpl(scriptSigHex,tokens)
      case List(w : BytesToPushOntoStack, x : ScriptConstant, y : BytesToPushOntoStack,
        z : ScriptConstant) => P2PKHScriptSignatureImpl(scriptSigHex,tokens)
      case List(w : BytesToPushOntoStack, x : ScriptConstant) => P2PKScriptSignatureImpl(scriptSigHex,tokens)
      case _ => NonStandardScriptSignatureImpl(scriptSigHex,tokens)
    }
  }

}

object ScriptSignatureFactory extends ScriptSignatureFactory

