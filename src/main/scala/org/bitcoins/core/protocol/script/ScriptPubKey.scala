package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0.WitnessScriptPubKeyV0Impl
import org.bitcoins.core.script.{ScriptOperation, ScriptSettings}
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY, OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.locktime.{OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY}
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.core.serializers.script.{RawScriptPubKeyParser, ScriptParser}
import org.bitcoins.core.util._

import scala.util.{Failure, Success, Try}

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends NetworkElement with BitcoinSLogger {

  /** The size of the script, this is used for network serialization */
  def compactSizeUInt : CompactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)

  /**
    * Representation of a scriptPubKey in a parsed assembly format
    * this data structure can be run through the script interpreter to
    * see if a script evaluates to true
    * Note: The first byte(s) inside the byte array is the [[CompactSizeUInt]]
    * used to represent the size of the script serialization
    */
  lazy val asm : Seq[ScriptToken] = ScriptParser.fromBytes(bytes.splitAt(compactSizeUInt.size.toInt)._2)


}

/**
 * Represents a pay-to-pubkey hash script pubkey
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * Format: OP_DUP OP_HASH160 <PubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
 */
sealed trait P2PKHScriptPubKey extends ScriptPubKey {
  def pubKeyHash : Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 3).bytes)
}


object P2PKHScriptPubKey extends Factory[P2PKHScriptPubKey] {

  private case class P2PKHScriptPubKeyImpl(hex : String) extends P2PKHScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): P2PKHScriptPubKey = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    P2PKHScriptPubKey(asm)
  }

  def apply(pubKey : ECPublicKey) : P2PKHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHScriptPubKey(hash)
  }

  def apply(hash: Sha256Hash160Digest): P2PKHScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_DUP, OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUALVERIFY, OP_CHECKSIG)
    P2PKHScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptPubKey = {
    require(isP2PKHScriptPubKey(asm), "Given asm was not a p2pkh scriptPubKey, got: " + asm)

    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)

    P2PKHScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply(asm :Seq[ScriptToken]) : P2PKHScriptPubKey = fromAsm(asm)

  /** Checks if the given asm matches the pattern for [[P2PKHScriptPubKey]] */
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
sealed trait MultiSignatureScriptPubKey extends ScriptPubKey {

  /** Returns the amount of required signatures for this multisignature script pubkey output */
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

  /** The maximum amount of signatures for this multisignature script pubkey output */
  def maxSigs : Long = {
    if (checkMultiSigIndex == -1 || checkMultiSigIndex == 0) {
      //means that we do not have a max signature requirement
      0.toLong
    } else {
      asm(checkMultiSigIndex - 1) match {
        case x : ScriptNumber => x.underlying
        case _ => throw new RuntimeException("The element preceding a OP_CHECKMULTISIG operation in a  multisignature pubkey must be a script number operation, got: " + this)
      }
    }
  }


  /** Gives the OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY index inside of asm */
  private def checkMultiSigIndex : Int = {
    if (asm.indexOf(OP_CHECKMULTISIG) != -1) asm.indexOf(OP_CHECKMULTISIG) else asm.indexOf(OP_CHECKMULTISIGVERIFY)
  }

  /** Returns the public keys encoded into the scriptPubKey */
  def publicKeys : Seq[ECPublicKey] = {
    asm.filter(_.isInstanceOf[ScriptConstant]).slice(1, maxSigs.toInt + 1).map(key => ECPublicKey(key.hex))
  }
}

object MultiSignatureScriptPubKey extends Factory[MultiSignatureScriptPubKey] with BitcoinSLogger {

  private case class MultiSignatureScriptPubKeyImpl(hex : String) extends MultiSignatureScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): MultiSignatureScriptPubKey = {
    val s = RawScriptPubKeyParser.read(bytes)
    MultiSignatureScriptPubKey(s.asm)
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
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    val m = MultiSignatureScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
    m
  }

  def apply(asm :Seq[ScriptToken]) : MultiSignatureScriptPubKey = fromAsm(asm)

  /** Determines if the given script tokens are a multisignature scriptPubKey */
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
    * public keys we can have in a [[MultiSignatureScriptPubKey]] */
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
sealed trait P2SHScriptPubKey extends ScriptPubKey {
  /** The hash of the script for which this scriptPubKey is being created from */
  def scriptHash : Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 2).bytes)
}

object P2SHScriptPubKey extends Factory[P2SHScriptPubKey] with BitcoinSLogger {

  private case class P2SHScriptPubKeyImpl(hex : String) extends P2SHScriptPubKey

  override def fromBytes(bytes : Seq[Byte]): P2SHScriptPubKey = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    P2SHScriptPubKey(asm)
  }

  def apply(scriptPubKey: ScriptPubKey) : P2SHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(scriptPubKey.asm.flatMap(_.bytes))
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUAL)
    P2SHScriptPubKey(asm)
  }

  /** Checks if the given asm matches the pattern for [[P2SHScriptPubKey]] */
  def isP2SHScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(OP_HASH160, x : BytesToPushOntoStack, y : ScriptConstant, OP_EQUAL) => true
    case _ => false
  }

  def fromAsm(asm: Seq[ScriptToken]): P2SHScriptPubKey = {
    require(isP2SHScriptPubKey(asm), "Given asm was not a p2sh scriptPubkey, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    P2SHScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply(asm :Seq[ScriptToken]) : P2SHScriptPubKey = fromAsm(asm)
}

/**
 * Represents a pay to public key script public key
 * https://bitcoin.org/en/developer-guide#pubkey
 * Format: <pubkey> OP_CHECKSIG
 */
sealed trait P2PKScriptPubKey extends ScriptPubKey {
  def publicKey : ECPublicKey = ECPublicKey(BitcoinScriptUtil.filterPushOps(asm).head.bytes)
}

object P2PKScriptPubKey extends Factory[P2PKScriptPubKey] {

  private case class P2PKScriptPubKeyImpl(hex : String) extends P2PKScriptPubKey

  override def fromBytes(bytes : Seq[Byte]) = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    P2PKScriptPubKey(asm)
  }

  def apply(pubKey : ECPublicKey): P2PKScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOps ++ Seq(ScriptConstant(pubKey.bytes), OP_CHECKSIG)
    P2PKScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptPubKey = {
    require(isP2PKScriptPubKey(asm), "Given asm was not a p2pk scriptPubKey, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    P2PKScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply(asm :Seq[ScriptToken]) : P2PKScriptPubKey = fromAsm(asm)

  /** Sees if the given asm matches the [[P2PKHScriptPubKey]] pattern */
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
sealed trait CLTVScriptPubKey extends ScriptPubKey {

  /** Determines the nested ScriptPubKey inside the CLTVScriptPubKey */
  def scriptPubKeyAfterCLTV : ScriptPubKey = {
    val bool : Boolean = asm.head.isInstanceOf[ScriptNumberOperation]
    bool match {
      case true => ScriptPubKey(asm.slice(3, asm.length))
      case false => ScriptPubKey(asm.slice(4, asm.length))
    }
  }

  /** The absolute CLTV-LockTime value (i.e. the output will remain unspendable until this timestamp or block height */
  def locktime : ScriptNumber = {
    asm.head match {
      case scriptNumOp: ScriptNumberOperation => ScriptNumber(scriptNumOp.underlying)
      case pushBytes : BytesToPushOntoStack => ScriptNumber(asm(1).hex)
      case x @ (_ : ScriptConstant | _ : ScriptOperation) => throw new IllegalArgumentException("In a CLTVScriptPubKey, " +
        "the first asm must be either a ScriptNumberOperation (i.e. OP_5), or the BytesToPushOntoStack for the proceeding ScriptConstant.")
    }
  }
}

object CLTVScriptPubKey extends Factory[CLTVScriptPubKey] {
  private case class CLTVScriptPubKeyImpl(hex : String) extends CLTVScriptPubKey

  override def fromBytes (bytes : Seq[Byte]) : CLTVScriptPubKey = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    CLTVScriptPubKey(asm)
  }
  def fromAsm (asm : Seq[ScriptToken]) : CLTVScriptPubKey = {
    require(isCLTVScriptPubKey(asm), "Given asm was not a CLTVScriptPubKey, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    CLTVScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply (asm: Seq[ScriptToken]) : CLTVScriptPubKey = fromAsm(asm)

  def apply(locktime : ScriptNumber, scriptPubKey : ScriptPubKey) : CLTVScriptPubKey = {
    val scriptOp = BitcoinScriptUtil.minimalScriptNumberRepresentation(locktime)

    val scriptNum : Seq[ScriptToken] = if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
      Seq(scriptOp)
    } else {
      val pushOpsLockTime= BitcoinScriptUtil.calculatePushOp(locktime.bytes)
      pushOpsLockTime ++ Seq(ScriptConstant(locktime.bytes))
    }

    val cltvAsm = Seq(OP_CHECKLOCKTIMEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ cltvAsm ++ scriptPubKeyAsm
    CLTVScriptPubKey(asm)
  }

  def isCLTVScriptPubKey(asm : Seq[ScriptToken]) : Boolean = {
    if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKLOCKTIMEVERIFY)) return false
      asm.slice(0,4) match {
        case List(lockTimeBytesToPush : BytesToPushOntoStack, lockTime : ScriptConstant, OP_CHECKLOCKTIMEVERIFY, OP_DROP) => true
        case _ => false
      }
    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKLOCKTIMEVERIFY)) return false
      asm.slice(0,3) match {
        case List(scriptNumOp : ScriptNumberOperation, OP_CHECKLOCKTIMEVERIFY, OP_DROP) => true
        case _ => false
      }
    }
  }
}

/**
  * Represents a scriptPubKey that contains OP_CHECKSEQUENCEVERIFY.
  * Adds a relative lockTime condition to any scriptPubKey.
  * https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
  * Format: <locktime> OP_CSV OP_DROP <scriptPubKey>
  */
sealed trait CSVScriptPubKey extends ScriptPubKey {

  /** Determines the nested ScriptPubKey inside the CSVScriptPubKey */
  def scriptPubKeyAfterCSV : ScriptPubKey = {
    val bool : Boolean = asm.head.isInstanceOf[ScriptNumberOperation]
    bool match {
      case true => ScriptPubKey(asm.slice(3, asm.length))
      case false => ScriptPubKey(asm.slice(4, asm.length))
    }
  }

  /** The relative CSV-LockTime value (i.e. the amount of time the output should remain unspendable) */
  def locktime : ScriptNumber = {
    asm.head match {
      case scriptNumOp: ScriptNumberOperation => ScriptNumber(scriptNumOp.underlying)
      case pushBytes : BytesToPushOntoStack => ScriptNumber(asm(1).hex)
      case x @ (_ : ScriptConstant | _ : ScriptOperation) => throw new IllegalArgumentException("In a CSVScriptPubKey, " +
        "the first asm must be either a ScriptNumberOperation (i.e. OP_5), or the BytesToPushOntoStack for the proceeding ScriptConstant.")
    }
  }

}

object CSVScriptPubKey extends Factory[CSVScriptPubKey] {
  private case class CSVScriptPubKeyImpl(hex : String) extends CSVScriptPubKey

  override def fromBytes(bytes : Seq[Byte]) : CSVScriptPubKey = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    CSVScriptPubKey(asm)
  }

  def fromAsm (asm : Seq[ScriptToken]) : CSVScriptPubKey = {
    require(isCSVScriptPubKey(asm), "Given asm was not a CSVScriptPubKey, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    CSVScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply(asm : Seq[ScriptToken]) : CSVScriptPubKey = fromAsm(asm)

  def apply(relativeLockTime : ScriptNumber, scriptPubKey : ScriptPubKey) : CSVScriptPubKey = {
    val scriptOp = BitcoinScriptUtil.minimalScriptNumberRepresentation(relativeLockTime)

    val scriptNum : Seq[ScriptToken] = if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
      Seq(scriptOp)
    } else {
      val pushOpsLockTime= BitcoinScriptUtil.calculatePushOp(relativeLockTime.bytes)
      pushOpsLockTime ++ Seq(ScriptConstant(relativeLockTime.bytes))
    }

    val csvAsm = Seq(OP_CHECKSEQUENCEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ csvAsm ++ scriptPubKeyAsm
    CSVScriptPubKey(asm)
  }

  def isCSVScriptPubKey(asm : Seq[ScriptToken]) : Boolean = {
    if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKSEQUENCEVERIFY)) return false
      asm.slice(0,4) match {
        case List(lockTimeBytesToPush : BytesToPushOntoStack, lockTime : ScriptConstant, OP_CHECKSEQUENCEVERIFY, OP_DROP) => true
        case _ => false
      }
    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKSEQUENCEVERIFY)) return false
      asm.slice(0,3) match {
        case List(numberOp : ScriptNumberOperation, OP_CHECKSEQUENCEVERIFY, OP_DROP) => true
        case _ => false
      }
    }
  }

}

sealed trait NonStandardScriptPubKey extends ScriptPubKey

object NonStandardScriptPubKey extends Factory[NonStandardScriptPubKey] {
  private case class NonStandardScriptPubKeyImpl(hex : String) extends NonStandardScriptPubKey

  override def fromBytes(bytes: Seq[Byte]): NonStandardScriptPubKey = {
    val asm = RawScriptPubKeyParser.read(bytes).asm
    NonStandardScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptPubKey = {
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    NonStandardScriptPubKeyImpl(compactSizeUInt.hex + asmHex)
  }

  def apply(asm : Seq[ScriptToken]) : NonStandardScriptPubKey = fromAsm(asm)
}

/** Represents the empty ScriptPubKey */
case object EmptyScriptPubKey extends ScriptPubKey {
  def hex = "00"
}

/** Factory companion object used to create ScriptPubKey objects */
object ScriptPubKey extends Factory[ScriptPubKey] with BitcoinSLogger {
  def empty : ScriptPubKey = fromAsm(Nil)

  /** Creates a scriptPubKey from its asm representation */
  def fromAsm(asm : Seq[ScriptToken]) : ScriptPubKey = asm match {
    case Nil => EmptyScriptPubKey
    case _ if P2PKHScriptPubKey.isP2PKHScriptPubKey(asm) => P2PKHScriptPubKey(asm)
    case _ if P2SHScriptPubKey.isP2SHScriptPubKey(asm) => P2SHScriptPubKey(asm)
    case _ if P2PKScriptPubKey.isP2PKScriptPubKey(asm) => P2PKScriptPubKey(asm)
    case _ if MultiSignatureScriptPubKey.isMultiSignatureScriptPubKey(asm) => MultiSignatureScriptPubKey(asm)
    case _ if CLTVScriptPubKey.isCLTVScriptPubKey(asm) => CLTVScriptPubKey(asm)
    case _ if CSVScriptPubKey.isCSVScriptPubKey(asm) => CSVScriptPubKey(asm)
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => WitnessScriptPubKey(asm).get
    case _ => NonStandardScriptPubKey(asm)
  }

  def fromBytes(bytes : Seq[Byte]) : ScriptPubKey = RawScriptPubKeyParser.read(bytes)

  def apply(asm : Seq[ScriptToken]) : ScriptPubKey = fromAsm(asm)
}

/** This type represents a [[ScriptPubKey]] to evaluate a [[ScriptWitness]] */
sealed trait WitnessScriptPubKey extends ScriptPubKey {
  def witnessProgram: Seq[ScriptToken]
  def witnessVersion = WitnessVersion(asm.head.toLong)
}

object WitnessScriptPubKey {

  /** Witness scripts must begin with one of these operations, see BIP141 */
  val validFirstOps = Seq(OP_0,OP_1,OP_2,OP_3,OP_4,OP_5,OP_6, OP_7, OP_8,
    OP_9, OP_10, OP_11, OP_12, OP_13, OP_14, OP_15, OP_16)

  def apply(asm: Seq[ScriptToken]): Option[WitnessScriptPubKey] = fromAsm(asm)

  def fromAsm(asm: Seq[ScriptToken]): Option[WitnessScriptPubKey] = asm match {
    case _ if WitnessScriptPubKeyV0.isWitnessScriptPubKeyV0(asm) => Some(WitnessScriptPubKeyV0(asm))
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => Some(UnassignedWitnessScriptPubKey(asm))
    case _ => None
  }

  def isWitnessScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    val bytes = asm.flatMap(_.bytes)
    val firstOp = asm.headOption
    if (bytes.size < 4 || bytes.size > 42) false
    else if (!validFirstOps.contains(firstOp.getOrElse(OP_1NEGATE))) false
    else if (asm(1).toLong + 2 == bytes.size) true
    else false
  }
}

/** Represents a [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program]] */
sealed trait WitnessScriptPubKeyV0 extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object WitnessScriptPubKeyV0 {
  private case class WitnessScriptPubKeyV0Impl(hex: String) extends WitnessScriptPubKeyV0


  def apply(asm: Seq[ScriptToken]): WitnessScriptPubKeyV0 = {
    require(isWitnessScriptPubKeyV0(asm), "Given asm was not a WitnessScriptPubKeyV0, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    WitnessScriptPubKeyV0Impl(compactSizeUInt.hex + asmHex)
  }

  /** Mimics the function to determine if a [[ScriptPubKey]] contains a witness
    * A witness program is any valid [[ScriptPubKey]] that consists of a 1 byte push op and then a data push
    * between 2 and 40 bytes
    * [[https://github.com/bitcoin/bitcoin/blob/449f9b8debcceb61a92043bc7031528a53627c47/src/script/script.cpL215-L229]]
    * Returns None if it is not a witness program, else returns the script and script version
    * */
  def isWitnessScriptPubKeyV0(asm: Seq[ScriptToken]): Boolean = {
    WitnessScriptPubKey.isWitnessScriptPubKey(asm) && asm.headOption == Some(OP_0)
  }
}

/** Type to represent all [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]]s we have not used yet in the bitcoin protocol */
sealed trait UnassignedWitnessScriptPubKey extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object UnassignedWitnessScriptPubKey {
  private case class UnassignedWitnessScriptPubKeyImpl(hex: String) extends UnassignedWitnessScriptPubKey

  def apply(asm: Seq[ScriptToken]): UnassignedWitnessScriptPubKey = {
    require(WitnessScriptPubKey.isWitnessScriptPubKey(asm), "Given asm was not a valid witness script pubkey: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    UnassignedWitnessScriptPubKeyImpl(compactSizeUInt.hex ++ asmHex)
  }
}