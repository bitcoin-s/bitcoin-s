package org.scalacoin.protocol.script

import org.scalacoin.crypto.{ECPublicKey, ECDigitalSignature}
import org.scalacoin.marshallers.script.{RawScriptSignatureParser, ScriptParser}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG}
import org.scalacoin.util.{BitcoinSLogger, Factory, BitcoinSUtil}

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 1/19/16.
 * Responsible for the instantiation of ScriptSignature objects
 */
trait ScriptSignatureFactory extends Factory[ScriptSignature] with BitcoinSLogger {

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
      case Nil => EmptyScriptSignature
      case _  if (tokens.size > 1 && isRedeemScript(tokens.last)) =>
        P2SHScriptSignatureImpl(scriptSigHex,tokens)
      case _ if (isMultiSignatureScriptSignature(tokens)) =>
        //the head of the asm does not neccessarily have to be an OP_0 if the NULLDUMMY script
        //flag is not set. It can be any script number operation
        MultiSignatureScriptSignatureImpl(scriptSigHex,tokens)
      case List(w : BytesToPushOntoStack, x : ScriptConstant, y : BytesToPushOntoStack,
        z : ScriptConstant) => P2PKHScriptSignatureImpl(scriptSigHex,tokens)
      case List(w : BytesToPushOntoStack, x : ScriptConstant) => P2PKScriptSignatureImpl(scriptSigHex,tokens)
      case _ => NonStandardScriptSignatureImpl(scriptSigHex,tokens)
    }
  }


  /**
   * Creates a script signature from the given tokens and scriptPubKey
   * @param tokens the script signature's tokens
   * @param scriptPubKey the scriptPubKey which the script signature is trying to spend
   * @return
   */
  def fromScriptPubKey(tokens : Seq[ScriptToken], scriptPubKey : ScriptPubKey) : ScriptSignature = {
    val scriptSigHex = tokens.map(_.hex).mkString
    scriptPubKey match {
      case s : P2SHScriptPubKey => P2SHScriptSignatureImpl(scriptSigHex,tokens)
      case s : P2PKHScriptPubKey => P2PKHScriptSignatureImpl(scriptSigHex,tokens)
      case s : P2PKScriptPubKey => P2PKScriptSignatureImpl(scriptSigHex,tokens)
      case s : MultiSignatureScriptPubKey => MultiSignatureScriptSignatureImpl(scriptSigHex,tokens)
      case s : NonStandardScriptPubKey => NonStandardScriptSignatureImpl(scriptSigHex, tokens)
      case EmptyScriptPubKey if (tokens.size == 0) => EmptyScriptSignature
      case EmptyScriptPubKey => NonStandardScriptSignatureImpl(scriptSigHex,tokens)
    }
  }




  /**
   * Detects if the given script token is a redeem script
   * @param token
   * @return
   */
  private def isRedeemScript(token : ScriptToken) : Boolean = {
    logger.debug("Checking if last token is redeem script")
    val redeemScriptTry : Try[ScriptPubKey] = parseRedeemScript(token)
    redeemScriptTry match {
      case Success(redeemScript) =>
        logger.debug("Possible redeemScript: " + redeemScript)
        redeemScript match {
          case x : P2PKHScriptPubKey => true
          case x : MultiSignatureScriptPubKey => true
          case x : P2SHScriptPubKey => true
          case x : P2PKScriptPubKey => true
          case x : NonStandardScriptPubKey => false
          case EmptyScriptPubKey => false
        }
      case Failure(_) => false
    }
  }

  /**
   * Parses a redeem script from the given script token
   * @param scriptToken
   * @return
   */
  def parseRedeemScript(scriptToken : ScriptToken) : Try[ScriptPubKey] = {
    val redeemScript : Try[ScriptPubKey] = Try(ScriptPubKey(scriptToken.bytes))
    redeemScript
  }

  /**
   * Checks if the given script tokens are a multisignature script sig
   * format: OP_0 <A sig> [B sig] [C sig...]
   * @param asm the asm to check if it falls in the multisignature script sig format
   * @return boolean indicating if the scriptsignature is a multisignature script signature
   */
  def isMultiSignatureScriptSignature(asm : Seq[ScriptToken]) : Boolean = {
    asm.isEmpty match {
      case true => false
      case false if (asm.size == 1) => false
      case false =>
        val firstTokenIsScriptNumberOperation = asm.head.isInstanceOf[ScriptNumberOperation]
        val restOfScriptIsPushOpsOrScriptConstants = asm.tail.map(
          token => token.isInstanceOf[ScriptConstant] || StackPushOperationFactory.isPushOperation(token)
        ).exists(_ == false)
        logger.debug("First number is script op: " + firstTokenIsScriptNumberOperation)
        logger.debug("tail is true: " +restOfScriptIsPushOpsOrScriptConstants )
        firstTokenIsScriptNumberOperation && !restOfScriptIsPushOpsOrScriptConstants
    }
  }
}

object ScriptSignatureFactory extends ScriptSignatureFactory

