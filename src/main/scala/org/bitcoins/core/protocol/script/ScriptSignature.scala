package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey, EmptyDigitalSignature}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.script.{RawScriptPubKeyParser, RawScriptSignatureParser, ScriptParser}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{HashType, HashTypeFactory, OP_CHECKMULTISIG, SIGHASH_ALL}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil, Factory}
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 12/26/15.
  *
  */
sealed trait ScriptSignature extends NetworkElement with BitcoinSLogger {


  /**
    * Representation of a scriptSignature in a parsed assembly format
    * this data structure can be run through the script interpreter to
    * see if a script evaluates to true
    * @return
    */
  lazy val asm : Seq[ScriptToken] = ScriptParser.fromHex(hex)



  /**
    * The digital signatures contained inside of the script signature
    * p2pkh script signatures only have one sig
    * p2pk script signatures only have one sigs
    * p2sh script signatures can have m sigs
    * multisignature scripts can have m sigs
    * @return
    */
  def signatures : Seq[ECDigitalSignature]


  /**
    * Derives the hash type for a given digitalSignature
    * @param digitalSignature
    * @return
    */
  def hashType(digitalSignature: ECDigitalSignature) = {
    digitalSignature match {
      case EmptyDigitalSignature => SIGHASH_ALL()
      case sig : ECDigitalSignature => HashTypeFactory.fromByte(digitalSignature.bytes.last)
    }
  }
}

trait NonStandardScriptSignature extends ScriptSignature {
  def signatures : Seq[ECDigitalSignature] = Seq()
}



/**
 * P2PKH script signatures have only one public key
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * P2PKH scriptSigs follow this format
 * <sig> <pubkey>
 */
trait P2PKHScriptSignature extends ScriptSignature {

  /**
    * P2PKH scriptSigs only have one signature
    * @return
    */
  def signature : ECDigitalSignature = signatures.head

  /**
    * Gives us the public key inside of a p2pkh script signature
    * @return
    */
  def publicKey : ECPublicKey = ECPublicKey(asm.last.bytes)

  /**
    * Returns the hash type for the p2pkh script signature
    * @return
    */
  def hashType : HashType = HashTypeFactory.fromByte(signature.bytes.last)

  override def signatures : Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(asm(1).hex))
  }

}

/**
 * Represents a pay-to-script-hash script signature
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * P2SH scriptSigs have the following format
 * <sig> [sig] [sig...] <redeemScript>
 */
trait P2SHScriptSignature extends ScriptSignature {

  /**
    * The redeemScript represents the conditions that must be satisfied to spend the output
    * @return
    */
  def redeemScript : ScriptPubKey = ScriptPubKey(asm.last.bytes)


  /**
    * Returns the script signature of this p2shScriptSig with no serialized redeemScript
    * @return
    */
  def scriptSignatureNoRedeemScript = ScriptSignature.fromAsm(splitAtRedeemScript(asm)._1)


  /**
    * Returns the public keys for the p2sh scriptSignature
    * @return
    */
  def publicKeys : Seq[ECPublicKey] = {
    val pubKeys : Seq[ScriptToken] = redeemScript.asm.filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
    pubKeys.map(k => ECPublicKey(k.hex))
  }


  /**
    * The digital signatures inside of the scriptSig
    * @return
    */
  def signatures : Seq[ECDigitalSignature] = {
    val nonRedeemScript = splitAtRedeemScript(asm)._1
    val sigs = nonRedeemScript.filter(_.isInstanceOf[ScriptConstant]).filterNot(_.isInstanceOf[ScriptNumberOperation])
    sigs.map(s => ECDigitalSignature(s.hex))
  }


  /**
    * Splits the given asm into two parts
    * the first part is the digital signatures
    * the second part is the redeem script
    * @param asm
    * @return
    */
  def splitAtRedeemScript(asm : Seq[ScriptToken]) : (Seq[ScriptToken],Seq[ScriptToken]) = {
    //call .tail twice to remove the serialized redeemScript & it's bytesToPushOntoStack constant
    (asm.reverse.tail.tail.reverse, Seq(asm.last))
  }

}

/**
 * Represents a multisignature script signature
 * https://bitcoin.org/en/developer-guide#multisig
 * Multisig script sigs have the following format
 * OP_0 <A sig> [B sig] [C sig...]
 */
trait MultiSignatureScriptSignature extends ScriptSignature {

  /**
    * The digital signatures inside of the scriptSig
    * @return
    */
  def signatures : Seq[ECDigitalSignature] = {
    asm.tail.filter(_.isInstanceOf[ScriptConstant])
      .map(sig => ECDigitalSignature(sig.hex))
  }
}

/**
 * Represents a pay to public key script signature
 * https://bitcoin.org/en/developer-guide#pubkey
 * Signature script: <sig>
 */
trait P2PKScriptSignature extends ScriptSignature {

  /**
    * Returns the hash type for the signature inside of the p2pk script signature
    * @return
    */
  def hashType = HashTypeFactory.fromByte(signature.bytes.last)

  /**
    * PubKey scriptSignatures only have one signature
    * @return
    */
  def signature : ECDigitalSignature = signatures.head

  /**
    * The digital signatures inside of the scriptSig
    * @return
    */
  def signatures : Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(asm).head.hex))
  }
}





/**
 * Represents the empty script signature
 */
case object EmptyScriptSignature extends ScriptSignature {
  def signatures = List()
  def hex = ""
}

object ScriptSignature extends Factory[ScriptSignature] with BitcoinSLogger {

  private case class NonStandardScriptSignatureImpl(hex : String) extends NonStandardScriptSignature

  private case class P2PKScriptSignatureImpl(hex : String) extends P2PKScriptSignature

  private case class MultiSignatureScriptSignatureImpl(hex : String) extends MultiSignatureScriptSignature

  private case class P2SHScriptSignatureImpl(hex : String) extends P2SHScriptSignature

  private case class P2PKHScriptSignatureImpl(hex : String) extends P2PKHScriptSignature

  /**
    * Builds a script signature from a digital signature and a public key
    * this is a pay to public key hash script sig
    *
    * @param signature
    * @param pubKey
    * @return
    */
  def factory(signature : ECDigitalSignature, pubKey : ECPublicKey) : ScriptSignature = {
    val signatureBytesToPushOntoStack = BytesToPushOntoStack(signature.bytes.size)
    val pubKeyBytesToPushOntoStack = BytesToPushOntoStack(pubKey.bytes.size)
    val asm : Seq[ScriptToken] = Seq(signatureBytesToPushOntoStack, ScriptConstant(signature.hex),
      pubKeyBytesToPushOntoStack, ScriptConstant(pubKey.hex))
    fromAsm(asm)
  }

  /**
    * Returns an empty script signature
    *
    * @return
    */
  def empty : ScriptSignature = EmptyScriptSignature

  def fromBytes(bytes : Seq[Byte]) : ScriptSignature = RawScriptSignatureParser.read(bytes)

  /**
    * Creates a scriptSignature from the list of script tokens
    *
    * @param tokens
    * @return
    */
  def fromAsm(tokens : Seq[ScriptToken]) : ScriptSignature = {
    val scriptSigHex = tokens.map(_.hex).mkString
    tokens match {
      case Nil => EmptyScriptSignature
      case _  if (tokens.size > 1 && isRedeemScript(tokens.last)) =>
        P2SHScriptSignatureImpl(scriptSigHex)
      case _ if (isMultiSignatureScriptSignature(tokens)) =>
        //the head of the asm does not neccessarily have to be an OP_0 if the NULLDUMMY script
        //flag is not set. It can be any script number operation
        MultiSignatureScriptSignatureImpl(scriptSigHex)
      case List(w : BytesToPushOntoStack, x : ScriptConstant, y : BytesToPushOntoStack,
      z : ScriptConstant) => P2PKHScriptSignatureImpl(scriptSigHex)
      case List(w : BytesToPushOntoStack, x : ScriptConstant) => P2PKScriptSignatureImpl(scriptSigHex)
      case _ => NonStandardScriptSignatureImpl(scriptSigHex)
    }
  }


  /**
    * Creates a script signature from the given tokens and scriptPubKey
    *
    * @param tokens the script signature's tokens
    * @param scriptPubKey the scriptPubKey which the script signature is trying to spend
    * @return
    */
  def fromScriptPubKey(tokens : Seq[ScriptToken], scriptPubKey : ScriptPubKey) : ScriptSignature = {
    val scriptSigHex = tokens.map(_.hex).mkString
    scriptPubKey match {
      case s : P2SHScriptPubKey => P2SHScriptSignatureImpl(scriptSigHex)
      case s : P2PKHScriptPubKey => P2PKHScriptSignatureImpl(scriptSigHex)
      case s : P2PKScriptPubKey => P2PKScriptSignatureImpl(scriptSigHex)
      case s : MultiSignatureScriptPubKey => MultiSignatureScriptSignatureImpl(scriptSigHex)
      case s : NonStandardScriptPubKey => NonStandardScriptSignatureImpl(scriptSigHex)
      case EmptyScriptPubKey if (tokens.size == 0) => EmptyScriptSignature
      case EmptyScriptPubKey => NonStandardScriptSignatureImpl(scriptSigHex)
    }
  }




  /**
    * Detects if the given script token is a redeem script
    *
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
    *
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
    *
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

  def apply(signature : ECDigitalSignature, pubKey : ECPublicKey) : ScriptSignature = factory(signature,pubKey)

  def apply(tokens : Seq[ScriptToken], scriptPubKey : ScriptPubKey) : ScriptSignature = fromScriptPubKey(tokens, scriptPubKey)
}

