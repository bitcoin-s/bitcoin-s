package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECPublicKey, ECDigitalSignature}
import org.scalacoin.marshallers.script.{RawScriptSignatureParser, ScriptParser}
import org.scalacoin.script.constant._
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
    val hex = asm.map(_.hex).mkString
    ScriptSignatureImpl(asm,hex)
  }

  /**
   * Returns an empty script signature
   * @return
   */
  def empty = ScriptSignatureImpl(Seq(),"")

  def fromBytes(bytes : Seq[Byte]) : ScriptSignature = RawScriptSignatureParser.read(bytes)
}

object ScriptSignatureFactory extends ScriptSignatureFactory