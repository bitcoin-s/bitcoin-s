package org.scalacoin.crypto

import org.scalacoin.config.TestNet3
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction.{Transaction, TransactionInput}
import org.scalacoin.script.ScriptProgram
import org.scalacoin.script.crypto._
import org.scalacoin.script.flag.ScriptVerifyDerSig
import org.scalacoin.util.{BitcoinSLogger, BitcoinSUtil}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 2/16/16.
 * Responsible for checkign digital signatures on inputs against their respective
 * public keys
 */
trait TransactionSignatureChecker extends BitcoinSLogger {


  /**
   * Checks the signatures inside of a script program
   * @param program the program whose transaction's input at the program's input index need to be checked against the scriptPubkey
   * @return a boolean indicating if the signatures in tx are valid or not
   */
  def checkSignature(program : ScriptProgram) : Boolean = {
    require(program.script.size > 0 && CryptoSignatureEvaluationFactory.fromHex(program.script.head.hex).isDefined,
      "The program script must contain atleast one operation and that operation must be in the CryptoOperationFactory" +
      "\nGiven operation: " + program.script.headOption)

    checkSignature(program.transaction,program.inputIndex,program.scriptPubKey, program.flags.contains(ScriptVerifyDerSig))
  }

  /**
   * Checks the signature of a scriptSig in the spending transaction against the
   * given scriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param pubKey
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                     pubKey: ECPublicKey, requireStrictDEREncoding : Boolean) : Boolean = {
    val input = spendingTransaction.inputs(inputIndex)
    val signature = input.scriptSignature.signatures.head
    if (requireStrictDEREncoding && !DERSignatureUtil.isStrictDEREncoding(signature)) {
      logger.warn("Signature was not stricly encoded der: " + signature.hex)
      false
    } else {
      val hashType = input.scriptSignature.hashType(signature)
      val hashForSignature = TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
      logger.info("Hash for signature: " + BitcoinSUtil.encodeHex(hashForSignature))
      val isValid = pubKey.verify(hashForSignature,signature)
      isValid
    }

  }

  /**
   * Checks the signatures on a given input against the scriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @return
   */
  def checkSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey: ScriptPubKey, requireStrictDEREncoding : Boolean) : Boolean = {
    val input = spendingTransaction.inputs(inputIndex)
    val scriptSig = input.scriptSignature
    scriptSig match {
      case p2pkhScriptSig : P2PKHScriptSignature =>
        checkP2PKHScriptSignature(spendingTransaction,inputIndex,scriptPubKey, p2pkhScriptSig,requireStrictDEREncoding)
      case multiSignatureScript : MultiSignatureScriptSignature =>
        checkMultiSignatureScriptSig(spendingTransaction,inputIndex,scriptPubKey,multiSignatureScript,requireStrictDEREncoding)
      case p2shSignatureScript : P2SHScriptSignature =>
        checkP2SHScriptSignature(spendingTransaction,inputIndex,scriptPubKey, p2shSignatureScript,requireStrictDEREncoding)
      case p2pkScriptSignature : P2PKScriptSignature =>
        throw new RuntimeException("This is an old script signature type that is not supported by wallets anymore")
      case EmptyScriptSignature => checkEmptyScriptSig(spendingTransaction,inputIndex,scriptPubKey)
      case x : NonStandardScriptSignature => false
    }
  }


  /**
   * Checks a pay-to-pubkey-hash scriptSignature against the given scriptPubKey, transaction, and inputIndex
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param p2pkhScriptSig
   * @return
   */
  private def checkP2PKHScriptSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                p2pkhScriptSig : P2PKHScriptSignature,requireStrictDEREncoding : Boolean) : Boolean = {
    val signature = p2pkhScriptSig.signatures.head
    if (requireStrictDEREncoding && !DERSignatureUtil.isStrictDEREncoding(signature)) {
      logger.warn("Signature for p2pkh scriptSig was not strictly der encoded: " + signature.hex)
      false
    } else {
      val hashType = p2pkhScriptSig.hashType(signature)
      val hashForSignature : Seq[Byte] =
        TransactionSignatureSerializer.hashForSignature(spendingTransaction,inputIndex,scriptPubKey,hashType)
      p2pkhScriptSig.publicKeys.head.verify(hashForSignature,p2pkhScriptSig.signatures.head)
    }
  }

  /**
   * Checks the p2sh scriptsig against the given scriptPubKey
   * throws an exception if the given scriptPubKey isn't a P2SHScriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param p2shScriptSignature
   * @return
   */
  private def checkP2SHScriptSignature(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                       p2shScriptSignature : P2SHScriptSignature, requireStrictDEREncoding : Boolean) : Boolean = {



    scriptPubKey match {
      case x : P2SHScriptPubKey =>
        val redeemScript = p2shScriptSignature.redeemScript
        helper(spendingTransaction, inputIndex, redeemScript,
        p2shScriptSignature.signatures.toList, p2shScriptSignature.publicKeys.toList, requireStrictDEREncoding)
      case x : MultiSignatureScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a multisignature scriptPubKey properly - this is trivially false")
        false
      case x : P2PKHScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a p2pkh scriptPubKey properly - this is trivially false")
        false
      case x : P2PKScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a p2pk scriptPubKey properly - this is trivially false")
        false
      case x : NonStandardScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a nonstandard scriptPubKey properly - this is trivially false")
        false
      case x : ScriptPubKey =>
        logger.warn("Trying to check if a p2sScriptSignature spends a scriptPubKey properly - this is trivially false")
        false
    }
  }
  /**
   * Checks a multisignature script sig against the given scriptPubKey
   * throws and exception if the given scriptPubKey is not of type MultiSignatureScriptPubKey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param multiSignatureScript
   * @return
   */
  private def checkMultiSignatureScriptSig(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                                           multiSignatureScript : MultiSignatureScriptSignature, requireStrictDEREncoding : Boolean) : Boolean = {
    scriptPubKey match {
      case x : MultiSignatureScriptPubKey =>
        logger.info("multisig public keys: " + x.publicKeys)
        helper(spendingTransaction,inputIndex,scriptPubKey,multiSignatureScript.signatures.toList,
          x.publicKeys.toList,requireStrictDEREncoding)
/*        val result: Seq[Boolean] = for {
          (sig, pubKey) <- multiSignatureScript.signatures.zip(x.publicKeys)
        } yield
          {
            if (requireStrictDEREncoding && DERSignatureUtil.isStrictDEREncoding(sig)) {
              logger.warn("Signature for multiSignatureScriptSig was not strictly der encoded: " + sig.hex)
              false
            } else {
              val hashType = multiSignatureScript.hashType(sig)
              val hashForSig: Seq[Byte] =
                TransactionSignatureSerializer.hashForSignature(spendingTransaction, inputIndex, x, hashType)
              pubKey.verify(hashForSig, sig)
            }

          }
        !result.contains(false)*/
      case x : P2PKHScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2pkh scriptPubKey properly - this is trivially false")
        false
      case x : P2PKScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2pk scriptPubKey properly - this is trivially false")
        false
      case x : NonStandardScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a p2sh scriptPubKey properly - this is trivially false")
        false
      case x : P2SHScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a nonstandard scriptPubKey properly - this is trivially false")
        false
      case EmptyScriptPubKey =>
        logger.warn("Trying to check if a multisignature scriptSig spends a empty scriptPubKey properly - this is trivially false")
        false
    }
  }

  /**
   * Checks if the scriptPubKey correlated with the empty script signature is valid
   * there are two ways this can be true.
   * 1.) If the multisig scriptPubKey requires 0 signatures
   * 2.) We have the empty script pubkey
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @return
   */
  private def checkEmptyScriptSig(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey) : Boolean = {
    scriptPubKey match {
      case x : MultiSignatureScriptPubKey =>
       if (x.requiredSigs == 0) true else false
      case x : P2PKHScriptPubKey => false
      case x : P2PKScriptPubKey => false
      case x : NonStandardScriptPubKey => false
      case x : P2SHScriptPubKey => false
      case EmptyScriptPubKey => true
    }
  }
  /**
   * This is a helper function to check digital signatures against public keys
   * if the signature does not match this public key, check it against the next
   * public key in the sequence
   * @param spendingTransaction
   * @param inputIndex
   * @param scriptPubKey
   * @param sigs
   * @param pubKeys
   * @param requireStrictDEREncoding
   * @return
   */
  @tailrec
  private def helper(spendingTransaction : Transaction, inputIndex : Int, scriptPubKey : ScriptPubKey,
                     sigs : List[ECDigitalSignature], pubKeys : List[ECPublicKey], requireStrictDEREncoding : Boolean) : Boolean = {
    logger.info("public keys inside of helper: " + pubKeys)
    if (sigs.size > pubKeys.size) {
      //this is how bitcoin core treats this. If there are ever any more
      //signatures than public keys remaining we immediately return
      //false https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L955-L959
      logger.info("We have more sigs than we have public keys remaining")
      false
    }
    else if (!sigs.isEmpty && !pubKeys.isEmpty) {
      val sig = sigs.head
      if (requireStrictDEREncoding && !DERSignatureUtil.isStrictDEREncoding(sig)) {
        logger.warn("Signature for multi signature script was not strictly encoded: " + sig.hex)
        false
      } else {
        val pubKey = pubKeys.head
        val hashType = spendingTransaction.inputs(inputIndex).scriptSignature.hashType(sig)
        val hashForSig = TransactionSignatureSerializer.hashForSignature(spendingTransaction,
          inputIndex,scriptPubKey,hashType)
        val result = pubKey.verify(hashForSig, sig)
        result match {
          case true => helper(spendingTransaction,inputIndex, scriptPubKey, sigs.tail,pubKeys.tail,requireStrictDEREncoding)
          case false => helper(spendingTransaction,inputIndex, scriptPubKey, sigs,pubKeys.tail,requireStrictDEREncoding)
        }
      }
    } else if (sigs.isEmpty) {
      //means that we have checked all of the sigs against the public keys
      //validation succeeds
      true
    } else false
  }

}

object TransactionSignatureChecker extends TransactionSignatureChecker
