package org.bitcoins.core.protocol.script

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.script.CLTVScriptPubKey.CLTVScriptPubKeyImpl
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY, OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.locktime.{OP_CHECKSEQUENCEVERIFY, OP_CHECKLOCKTIMEVERIFY}
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.core.serializers.script.{RawScriptPubKeyParser, ScriptParser}
import org.bitcoins.core.util._

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends NetworkElement with BitcoinSLogger {

  /**
   * Representation of a scriptSignature in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
    *
    * @return
   */
  lazy val asm : Seq[ScriptToken] = ScriptParser.fromBytes(bytes)

}

/**
 * Represents a pay-to-pubkey hash script pubkey
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * Format: OP_DUP OP_HASH160 <PubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
 */
trait P2PKHScriptPubKey extends ScriptPubKey {
  def pubKeyHash : Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 3).bytes)
}


object P2PKHScriptPubKey extends Factory[P2PKHScriptPubKey] {

  private case class P2PKHScriptPubKeyImpl(hex : String) extends P2PKHScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): P2PKHScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    P2PKHScriptPubKey(asm)
  }

  def apply(pubKey : ECPublicKey) : P2PKHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_DUP, OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUALVERIFY, OP_CHECKSIG)
    P2PKHScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptPubKey = {
    require(isP2PKHScriptPubKey(asm), "Given asm was not a p2pkh scriptPubKey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    P2PKHScriptPubKeyImpl(hex)
  }

  def apply(asm :Seq[ScriptToken]) : P2PKHScriptPubKey = fromAsm(asm)
  /**
    * Checks if the given asm matches the pattern for [[P2PKHScriptPubKey]]
    *
    * @param asm
    * @return
    */
  def isP2PKHScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(OP_DUP, OP_HASH160, x : BytesToPushOntoStack, y : ScriptConstant, OP_EQUALVERIFY, OP_CHECKSIG) => true
    case _ => false
  }
}
/**
 * Represents a multisignature script public key
 * https://bitcoin.org/en/developer-guide#multisig
 * Format: <m> <A pubkey> [B pubkey] [C pubkey...] <n> OP_CHECKMULTISIG
 */
trait MultiSignatureScriptPubKey extends ScriptPubKey {

  /**
    * Returns the amount of required signatures for this multisignature script pubkey output
    *
    * @return
    */
  def requiredSigs : Long = {
    val asmWithoutPushOps = asm.filterNot(_.isInstanceOf[BytesToPushOntoStack])
    val opCheckMultiSigIndex = if (asm.indexOf(OP_CHECKMULTISIG) != -1) asmWithoutPushOps.indexOf(OP_CHECKMULTISIG) else asmWithoutPushOps.indexOf(OP_CHECKMULTISIGVERIFY)
    //magic number 2 represents the maxSig operation and the OP_CHECKMULTISIG operation at the end of the asm
    val numSigsRequired = asmWithoutPushOps(opCheckMultiSigIndex - maxSigs.toInt - 2)
    numSigsRequired match {
      case x : ScriptNumber => x.underlying
      case _ => throw new RuntimeException("The first element of the multisignature pubkey must be a script number operation\n" +
        "operation: " + numSigsRequired +
        "\nscriptPubKey: " + this)
    }
  }

  /**
   * The maximum amount of signatures for this multisignature script pubkey output
    *
    * @return
   */
  def maxSigs : Long = {
    if (checkMultiSigIndex == -1 || checkMultiSigIndex == 0) {
      //means that we do not have a max signature requirement
      0.toLong
    } else {
      asm(checkMultiSigIndex - 1) match {
        case x : ScriptNumber => x.underlying
        case _ => throw new RuntimeException("The element preceding a OP_CHECKMULTISIG operation in a  multisignature pubkey must be a script number operation")
      }
    }
  }


  /**
   * Gives the OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY index inside of asm
    *
    * @return the index of OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY
   */
  private def checkMultiSigIndex : Int = {
    if (asm.indexOf(OP_CHECKMULTISIG) != -1) asm.indexOf(OP_CHECKMULTISIG) else asm.indexOf(OP_CHECKMULTISIGVERIFY)
  }

  /**
   * Returns the public keys encoded into the scriptPubKey
    *
    * @return
   */
  def publicKeys : Seq[ECPublicKey] = {
    asm.filter(_.isInstanceOf[ScriptConstant]).slice(1, maxSigs.toInt + 1).map(key => ECPublicKey(key.hex))
  }
}

object MultiSignatureScriptPubKey extends Factory[MultiSignatureScriptPubKey] {

  private case class MultiSignatureScriptPubKeyImpl(hex : String) extends MultiSignatureScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): MultiSignatureScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    MultiSignatureScriptPubKey(asm)
  }

  def apply(requiredSigs : Int, pubKeys : Seq[ECPublicKey]): MultiSignatureScriptPubKey = {
    require(requiredSigs <= ScriptSettings.maxPublicKeysPerMultiSig, "We cannot have more required signatures than: " +
      ScriptSettings.maxPublicKeysPerMultiSig + " got: " + requiredSigs)
    require(pubKeys.length <= ScriptSettings.maxPublicKeysPerMultiSig, "We cannot have more public keys than " +
      ScriptSettings.maxPublicKeysPerMultiSig + " got: " + pubKeys.length)

    val required = ScriptNumberOperation.fromNumber(requiredSigs) match {
      case Some(scriptNumOp) => Seq(scriptNumOp)
      case None =>
        val scriptNum = ScriptNumber(requiredSigs)
        val pushOps = BitcoinScriptUtil.calculatePushOp(scriptNum.bytes)
        pushOps ++ Seq(scriptNum)
    }
    val possible = ScriptNumberOperation.fromNumber(pubKeys.length) match {
      case Some(scriptNumOp) => Seq(scriptNumOp)
      case None =>
        val scriptNum = ScriptNumber(pubKeys.length)
        val pushOps = BitcoinScriptUtil.calculatePushOp(scriptNum)
        pushOps ++ Seq(scriptNum)
    }
    val pubKeysWithPushOps : Seq[Seq[ScriptToken]] = for {
      pubKey <- pubKeys
      pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
      constant = ScriptConstant(pubKey.bytes)
    } yield pushOps ++ Seq(constant)
    val asm: Seq[ScriptToken] = required ++ pubKeysWithPushOps.flatten ++ possible ++ Seq(OP_CHECKMULTISIG)
    MultiSignatureScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptPubKey = {
    require(isMultiSignatureScriptPubKey(asm), "Given asm was not a MultSignatureScriptPubKey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    MultiSignatureScriptPubKeyImpl(hex)
  }

  def apply(asm :Seq[ScriptToken]) : MultiSignatureScriptPubKey = fromAsm(asm)

  /**
    * Determines if the given script tokens are a multisignature scriptPubKey
    *
    * @param asm the tokens to check
    * @return a boolean indicating if the given tokens are a multisignature scriptPubKey
    */
  def isMultiSignatureScriptPubKey(asm : Seq[ScriptToken]) : Boolean = {
    val isNotEmpty = asm.size > 0
    val containsMultiSigOp = asm.contains(OP_CHECKMULTISIG) || asm.contains(OP_CHECKMULTISIGVERIFY)
    //we need either the first or second asm operation to indicate how many signatures are required
    val hasRequiredSignaturesTry = Try {
      asm.headOption match {
        case None => false
        case Some(token) =>
          //this is for the case that we have more than 16 public keys, the
          //first operation will be a push op, the second operation being the actual number of keys
          if (token.isInstanceOf[BytesToPushOntoStack]) isValidPubKeyNumber(asm.tail.head)
          else isValidPubKeyNumber(token)
      }
    }
    //the second to last asm operation should be the maximum amount of public keys
    val hasMaximumSignaturesTry = Try {
      asm(asm.length - 2) match {
        case token: ScriptToken => isValidPubKeyNumber(token)
      }
    }

    val standardOps = asm.filter(op =>  op.isInstanceOf[ScriptNumber] || op == OP_CHECKMULTISIG ||
      op == OP_CHECKMULTISIGVERIFY || op.isInstanceOf[ScriptConstant] || op.isInstanceOf[BytesToPushOntoStack])
    (hasRequiredSignaturesTry, hasMaximumSignaturesTry) match {
      case (Success(hasRequiredSignatures), Success(hasMaximumSignatures)) =>
        val result = isNotEmpty && containsMultiSigOp && hasRequiredSignatures &&
          hasMaximumSignatures && standardOps.size == asm.size
        result
      case (Success(_), Failure(_)) => false
      case (Failure(_), Success(_)) => false
      case (Failure(_), Failure(_)) => false
    }
  }

  /**
    * Checks that the given script token is with the range of the maximum amount of
    * public keys we can have in a [[MultiSignatureScriptPubKey]]
    *
    * @param token
    * @return
    */
  private def isValidPubKeyNumber(token : ScriptToken): Boolean = token match {
    case constant : ScriptConstant =>
      constant.isInstanceOf[ScriptNumber] ||
        ScriptNumber(constant.bytes) <= ScriptNumber(ScriptSettings.maxPublicKeysPerMultiSig)
    case _ : ScriptToken => false
  }
}

/**
 * Represents a pay-to-scripthash public key
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * Format: OP_HASH160 <Hash160(redeemScript)> OP_EQUAL
 */
trait P2SHScriptPubKey extends ScriptPubKey {
  /** The hash of the script for which this scriptPubKey is being created from */
  def scriptHash : Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 2).bytes)
}

object P2SHScriptPubKey extends Factory[P2SHScriptPubKey] with BitcoinSLogger {

  private case class P2SHScriptPubKeyImpl(hex : String) extends P2SHScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): P2SHScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    P2SHScriptPubKey(asm)
  }

  def apply(scriptPubKey: ScriptPubKey) : P2SHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(scriptPubKey.bytes)
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUAL)
    P2SHScriptPubKey(asm)
  }

  /**
    * Checks if the given asm matches the pattern for [[P2SHScriptPubKey]]
    *
    * @param asm
    * @return
    */
  def isP2SHScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(OP_HASH160, x : BytesToPushOntoStack, y : ScriptConstant, OP_EQUAL) => true
    case _ => false
  }

  def fromAsm(asm: Seq[ScriptToken]): P2SHScriptPubKey = {
    require(isP2SHScriptPubKey(asm), "Given asm was not a p2sh scriptPubkey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    P2SHScriptPubKeyImpl(hex)
  }

  def apply(asm :Seq[ScriptToken]) : P2SHScriptPubKey = fromAsm(asm)
}

/**
 * Represents a pay to public key script public key
 * https://bitcoin.org/en/developer-guide#pubkey
 * Format: <pubkey> OP_CHECKSIG
 */
trait P2PKScriptPubKey extends ScriptPubKey {
  def publicKey : ECPublicKey = ECPublicKey(BitcoinScriptUtil.filterPushOps(asm).head.bytes)
}

object P2PKScriptPubKey extends Factory[P2PKScriptPubKey] {

  private case class P2PKScriptPubKeyImpl(hex : String) extends P2PKScriptPubKey

  override def fromBytes(bytes : Seq[Byte]) = {
    val asm = ScriptParser.fromBytes(bytes)
    P2PKScriptPubKey(asm)
  }

  def apply(pubKey : ECPublicKey): P2PKScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOps ++ Seq(ScriptConstant(pubKey.bytes), OP_CHECKSIG)
    P2PKScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptPubKey = {
    require(isP2PKScriptPubKey(asm), "Given asm was not a p2pk scriptPubKey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    P2PKScriptPubKeyImpl(hex)
  }

  def apply(asm :Seq[ScriptToken]) : P2PKScriptPubKey = fromAsm(asm)

  /**
    * Sees if the given asm matches the [[P2PKHScriptPubKey]] pattern
    *
    * @param asm
    * @return
    */
  def isP2PKScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(b : BytesToPushOntoStack, x : ScriptConstant, OP_CHECKSIG) => true
    case _ => false
  }

}

/**
  * Represents a scriptPubKey that contains OP_CHECKLOCKTIMEVERIFY.
  * Adds an absolute/defined locktime condition to any scriptPubKey.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki]]
  * Format: <locktime> OP_CLTV OP_DROP <scriptPubKey>
  */
trait CLTVScriptPubKey extends ScriptPubKey {
  /**
    * Determines the nested ScriptPubKey inside the CLTVScriptPubKey
    * @return
    */
  def scriptPubKeyAfterCLTV : ScriptPubKey = ScriptPubKey(asm.slice(4, asm.length))
}

object CLTVScriptPubKey extends Factory[CLTVScriptPubKey] {
  private case class CLTVScriptPubKeyImpl(hex : String) extends CLTVScriptPubKey

  override def fromBytes (bytes : Seq[Byte]) : CLTVScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    CLTVScriptPubKey(asm)
  }
  def fromAsm (asm : Seq[ScriptToken]) : CLTVScriptPubKey = {
    require(isCLTVScriptPubKey(asm), "Given asm was not a CLTVScriptPubKey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    CLTVScriptPubKeyImpl(hex)
  }

  def apply (asm: Seq[ScriptToken]) : CLTVScriptPubKey = fromAsm(asm)

  /**
    * Creates a P2PKH-formatted CLTVScriptPubKey
    * @param locktime block height or timestamp
    * @param pubKey public key corresponding to the CLTV output
    * @return
    */
  def apply(locktime : ScriptNumber, pubKey : ECPublicKey) : CLTVScriptPubKey = {
    val pushOpsLockTime = BitcoinScriptUtil.calculatePushOp(locktime.bytes)
    val pushOpsPubKey = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOpsLockTime ++ Seq(ScriptConstant(locktime.bytes)) ++ Seq(OP_CHECKLOCKTIMEVERIFY, OP_DROP, OP_DUP, OP_HASH160) ++
    pushOpsPubKey ++ Seq(ScriptConstant(pubKey.bytes)) ++ Seq(OP_CHECKSIG)
    CLTVScriptPubKey(asm)
  }

  def apply(locktime : ScriptNumber, scriptPubKey : ScriptPubKey) : CLTVScriptPubKey = {
    val pushOpsLockTime= BitcoinScriptUtil.calculatePushOp(locktime.bytes)
    val cltvAsm = pushOpsLockTime ++ Seq(ScriptConstant(locktime.bytes)) ++ Seq(OP_CHECKLOCKTIMEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = cltvAsm ++ scriptPubKeyAsm
    CLTVScriptPubKey(asm)
  }

  def isCLTVScriptPubKey(asm : Seq[ScriptToken]) : Boolean = asm.slice(0,4) match {
    case List(lockTimeBytesToPush : BytesToPushOntoStack, lockTime : ScriptConstant, OP_CHECKLOCKTIMEVERIFY, OP_DROP) => true
    case _ => false
  }
}

/**
  * Represents a scriptPubKey that contains OP_CHECKSEQUENCEVERIFY.
  * Adds a relative lockTime condition to any scriptPubKey.
  * https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
  * Format: <locktime> OP_CSV OP_DROP <scriptPubKey>
  */
trait CSVScriptPubKey extends ScriptPubKey {
  /**
    * Determines the nested ScriptPubKey inside the CSVScriptPubKey
    * @return
    */
  def scriptPubKeyAfterCSV : ScriptPubKey = ScriptPubKey(asm.slice(4, asm.length))
}

object CSVScriptPubKey extends Factory[CSVScriptPubKey] {
  private case class CSVScriptPubKeyImpl(hex : String) extends CSVScriptPubKey

  override def fromBytes(bytes : Seq[Byte]) : CSVScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    CSVScriptPubKey(asm)
  }

  def fromAsm (asm : Seq[ScriptToken]) : CSVScriptPubKey = {
    require(isCSVScriptPubKey(asm), "Given asm was not a CSVScriptPubKey, got: " + asm)
    val hex = asm.map(_.hex).mkString
    CSVScriptPubKeyImpl(hex)
  }

  def apply(asm : Seq[ScriptToken]) : CSVScriptPubKey = fromAsm(asm)

  /**
    * Creates P2PKH-formatted CSVScriptPubKey
    * @param relativeLockTime number of blocks OR timestamp to lock coins
    * @param pubKey public key corresponding to CSV output
    * @return
    */
  def apply(relativeLockTime : ScriptNumber, pubKey : ECPublicKey) : CSVScriptPubKey = {
    val pushOpsTimeStamp = BitcoinScriptUtil.calculatePushOp(relativeLockTime.bytes)
    val pushOpsPubKey = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOpsTimeStamp ++ Seq(ScriptConstant(relativeLockTime.bytes)) ++ Seq(OP_CHECKSEQUENCEVERIFY, OP_DROP, OP_DUP, OP_HASH160) ++
      pushOpsPubKey ++ Seq(ScriptConstant(pubKey.bytes)) ++ Seq(OP_CHECKSIG)
    CSVScriptPubKey(asm)
  }

  def apply(relativeLockTime : ScriptNumber, scriptPubKey : ScriptPubKey) : CSVScriptPubKey = {
    val pushOpsLockTime= BitcoinScriptUtil.calculatePushOp(relativeLockTime.bytes)
    val csvAsm = pushOpsLockTime ++ Seq(ScriptConstant(relativeLockTime.bytes)) ++ Seq(OP_CHECKSEQUENCEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = csvAsm ++ scriptPubKeyAsm
    CSVScriptPubKey(asm)
  }

  def isCSVScriptPubKey(asm : Seq[ScriptToken]) : Boolean = asm.slice(0,4) match {
    case List(lockTimeBytesToPush : BytesToPushOntoStack, lockTime : ScriptConstant, OP_CHECKSEQUENCEVERIFY, OP_DROP) => true
    case _ => false
  }
}

trait NonStandardScriptPubKey extends ScriptPubKey

object NonStandardScriptPubKey extends Factory[NonStandardScriptPubKey] {
  private case class NonStandardScriptPubKeyImpl(hex : String) extends NonStandardScriptPubKey

  override def fromBytes(bytes: Seq[Byte]): NonStandardScriptPubKey = {
    val asm = ScriptParser.fromBytes(bytes)
    NonStandardScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptPubKey = {
    val hex = asm.map(_.hex).mkString
    NonStandardScriptPubKeyImpl(hex)
  }

  def apply(asm : Seq[ScriptToken]) : NonStandardScriptPubKey = fromAsm(asm)
}

/**
 * Represents the empty script pub key
 */
case object EmptyScriptPubKey extends ScriptPubKey {
  def hex = ""
}

/**
  * Factory companion object used to create ScriptPubKey objects
  */
object ScriptPubKey extends Factory[ScriptPubKey] with BitcoinSLogger {
  def empty : ScriptPubKey = fromAsm(List())

  /**
    * Creates a scriptPubKey from its asm representation
    *
    * @param asm
    * @return
    */
  def fromAsm(asm : Seq[ScriptToken]) : ScriptPubKey = asm match {
    case Seq() => EmptyScriptPubKey
    case _ if P2PKHScriptPubKey.isP2PKHScriptPubKey(asm) => P2PKHScriptPubKey(asm)
    case _ if P2SHScriptPubKey.isP2SHScriptPubKey(asm) => P2SHScriptPubKey(asm)
    case _ if P2PKScriptPubKey.isP2PKScriptPubKey(asm) => P2PKScriptPubKey(asm)
    case _ if MultiSignatureScriptPubKey.isMultiSignatureScriptPubKey(asm) => MultiSignatureScriptPubKey(asm)
    case _ if CLTVScriptPubKey.isCLTVScriptPubKey(asm) => CLTVScriptPubKey(asm)
    case _ if CSVScriptPubKey.isCSVScriptPubKey(asm) => CSVScriptPubKey(asm)
    case _ => NonStandardScriptPubKey(asm)
  }

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

  def apply(asm : Seq[ScriptToken]) : ScriptPubKey = fromAsm(asm)

}