package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.script.bitwise.{ OP_EQUAL, OP_EQUALVERIFY }
import org.bitcoins.core.script.constant.{ BytesToPushOntoStack, _ }
import org.bitcoins.core.script.control.{ OP_ELSE, OP_ENDIF, OP_IF, OP_RETURN }
import org.bitcoins.core.script.crypto.{ OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY, OP_CHECKSIG, OP_HASH160 }
import org.bitcoins.core.script.locktime.{ OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY }
import org.bitcoins.core.script.reserved.UndefinedOP_NOP
import org.bitcoins.core.script.stack.{ OP_DROP, OP_DUP }
import org.bitcoins.core.serializers.script.{ RawScriptPubKeyParser, ScriptParser }
import org.bitcoins.core.util._
import scodec.bits.ByteVector

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 12/26/15.
 */
sealed trait ScriptPubKey extends NetworkElement {

  /** The size of the script, this is used for network serialization */
  def compactSizeUInt: CompactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)

  /**
   * Representation of a scriptPubKey in a parsed assembly format
   * this data structure can be run through the script interpreter to
   * see if a script evaluates to true
   * Note: The first byte(s) inside the byte array is the [[CompactSizeUInt]]
   * used to represent the size of the script serialization
   */
  lazy val asm: Seq[ScriptToken] = ScriptParser.fromBytes(bytes.splitAt(compactSizeUInt.size.toInt)._2)

  /**
   * The byte representation of [[asm]], this does NOT have the bytes
   * for the [[org.bitcoins.core.protocol.CompactSizeUInt]] in the [[org.bitcoins.core.protocol.script.ScriptPubKey]]
   */
  lazy val asmBytes: ByteVector = BitcoinSUtil.toByteVector(asm)

}

/**
 * Represents a pay-to-pubkey hash script pubkey
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * Format: OP_DUP OP_HASH160 <PubKeyHash> OP_EQUALVERIFY OP_CHECKSIG
 */
sealed trait P2PKHScriptPubKey extends ScriptPubKey {
  def pubKeyHash: Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 3).bytes)
}

object P2PKHScriptPubKey extends ScriptFactory[P2PKHScriptPubKey] {
  private case class P2PKHScriptPubKeyImpl(bytes: ByteVector) extends P2PKHScriptPubKey {
    override def toString = "P2PKHScriptPubKeyImpl(" + hex + ")"
  }

  def apply(pubKey: ECPublicKey): P2PKHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHScriptPubKey(hash)
  }

  def apply(hash: Sha256Hash160Digest): P2PKHScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_DUP, OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUALVERIFY, OP_CHECKSIG)
    P2PKHScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptPubKey = {
    buildScript(asm, P2PKHScriptPubKeyImpl(_), isP2PKHScriptPubKey(_), "Given asm was not a p2pkh scriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2PKHScriptPubKey = fromAsm(asm)

  /** Checks if the given asm matches the pattern for [[P2PKHScriptPubKey]] */
  def isP2PKHScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(OP_DUP, OP_HASH160, x: BytesToPushOntoStack, y: ScriptConstant, OP_EQUALVERIFY, OP_CHECKSIG) => true
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
  def requiredSigs: Int = {
    val asmWithoutPushOps = asm.filterNot(_.isInstanceOf[BytesToPushOntoStack])
    val opCheckMultiSigIndex = if (asm.indexOf(OP_CHECKMULTISIG) != -1) asmWithoutPushOps.indexOf(OP_CHECKMULTISIG) else asmWithoutPushOps.indexOf(OP_CHECKMULTISIGVERIFY)
    //magic number 2 represents the maxSig operation and the OP_CHECKMULTISIG operation at the end of the asm
    val numSigsRequired = asmWithoutPushOps(opCheckMultiSigIndex - maxSigs.toInt - 2)
    numSigsRequired match {
      case x: ScriptNumber => x.toInt
      case c: ScriptConstant if ScriptNumber(c.hex).toLong <= Consensus.maxPublicKeysPerMultiSig =>
        ScriptNumber(c.hex).toInt
      case _ => throw new RuntimeException("The first element of the multisignature pubkey must be a script number operation\n" +
        "operation: " + numSigsRequired +
        "\nscriptPubKey: " + this)
    }
  }

  /** The maximum amount of signatures for this multisignature script pubkey output */
  def maxSigs: Int = {
    if (checkMultiSigIndex == -1 || checkMultiSigIndex == 0) {
      //means that we do not have a max signature requirement
      0
    } else {
      asm(checkMultiSigIndex - 1) match {
        case x: ScriptNumber => x.toInt
        case c: ScriptConstant if ScriptNumber(c.hex).toLong <= Consensus.maxPublicKeysPerMultiSig =>
          ScriptNumber(c.hex).toInt
        case x => throw new RuntimeException("The element preceding a OP_CHECKMULTISIG operation in a  multisignature pubkey must be a script number operation, got: " + x)
      }
    }
  }

  /** Gives the OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY index inside of asm */
  private def checkMultiSigIndex: Int = {
    if (asm.indexOf(OP_CHECKMULTISIG) != -1) asm.indexOf(OP_CHECKMULTISIG) else asm.indexOf(OP_CHECKMULTISIGVERIFY)
  }

  /** Returns the public keys encoded into the scriptPubKey */
  def publicKeys: Seq[ECPublicKey] = {
    asm.filter(_.isInstanceOf[ScriptConstant]).slice(1, maxSigs + 1).map(key => ECPublicKey(key.hex))
  }
}

object MultiSignatureScriptPubKey extends ScriptFactory[MultiSignatureScriptPubKey] {

  private case class MultiSignatureScriptPubKeyImpl(bytes: ByteVector) extends MultiSignatureScriptPubKey {
    override def toString = "MultiSignatureScriptPubKeyImpl(" + hex + ")"
  }

  def apply(requiredSigs: Int, pubKeys: Seq[ECPublicKey]): MultiSignatureScriptPubKey = {
    require(requiredSigs <= Consensus.maxPublicKeysPerMultiSig, "We cannot have more required signatures than: " +
      Consensus.maxPublicKeysPerMultiSig + " got: " + requiredSigs)
    require(pubKeys.length <= Consensus.maxPublicKeysPerMultiSig, "We cannot have more public keys than " +
      Consensus.maxPublicKeysPerMultiSig + " got: " + pubKeys.length)

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
    val pubKeysWithPushOps: Seq[Seq[ScriptToken]] = for {
      pubKey <- pubKeys
      pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
      constant = ScriptConstant(pubKey.bytes)
    } yield pushOps ++ Seq(constant)
    val asm: Seq[ScriptToken] = required ++ pubKeysWithPushOps.flatten ++ possible ++ Seq(OP_CHECKMULTISIG)
    MultiSignatureScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptPubKey = {
    buildScript(asm, MultiSignatureScriptPubKeyImpl(_), isMultiSignatureScriptPubKey(_), "Given asm was not a MultSignatureScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): MultiSignatureScriptPubKey = fromAsm(asm)

  /** Determines if the given script tokens are a multisignature scriptPubKey */
  def isMultiSignatureScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
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

    val standardOps = asm.filter(op => op.isInstanceOf[ScriptNumber] || op == OP_CHECKMULTISIG ||
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
   */
  private def isValidPubKeyNumber(token: ScriptToken): Boolean = token match {
    case constant: ScriptConstant =>
      constant.isInstanceOf[ScriptNumber] ||
        ScriptNumber(constant.bytes) <= ScriptNumber(Consensus.maxPublicKeysPerMultiSig)
    case _: ScriptToken => false
  }
}

/**
 * Represents a pay-to-scripthash public key
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * Format: OP_HASH160 <Hash160(redeemScript)> OP_EQUAL
 */
sealed trait P2SHScriptPubKey extends ScriptPubKey {
  /** The hash of the script for which this scriptPubKey is being created from */
  def scriptHash: Sha256Hash160Digest = Sha256Hash160Digest(asm(asm.length - 2).bytes)
}

object P2SHScriptPubKey extends ScriptFactory[P2SHScriptPubKey] {
  private case class P2SHScriptPubKeyImpl(bytes: ByteVector) extends P2SHScriptPubKey {
    override def toString = "P2SHScriptPubKeyImpl(" + hex + ")"
  }

  def apply(scriptPubKey: ScriptPubKey): P2SHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(scriptPubKey.asmBytes)
    P2SHScriptPubKey(hash)
  }

  def apply(hash: Sha256Hash160Digest): P2SHScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm = Seq(OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUAL)
    P2SHScriptPubKey(asm)
  }

  /** Checks if the given asm matches the pattern for [[P2SHScriptPubKey]] */
  def isP2SHScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(OP_HASH160, x: BytesToPushOntoStack, y: ScriptConstant, OP_EQUAL) => true
    case _ => false
  }

  def fromAsm(asm: Seq[ScriptToken]): P2SHScriptPubKey = {
    buildScript(asm, P2SHScriptPubKeyImpl(_), isP2SHScriptPubKey(_), "Given asm was not a p2sh scriptPubkey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2SHScriptPubKey = fromAsm(asm)
}

/**
 * Represents a pay to public key script public key
 * https://bitcoin.org/en/developer-guide#pubkey
 * Format: <pubkey> OP_CHECKSIG
 */
sealed trait P2PKScriptPubKey extends ScriptPubKey {
  def publicKey: ECPublicKey = ECPublicKey(BitcoinScriptUtil.filterPushOps(asm).head.bytes)
}

object P2PKScriptPubKey extends ScriptFactory[P2PKScriptPubKey] {

  private case class P2PKScriptPubKeyImpl(bytes: ByteVector) extends P2PKScriptPubKey {
    override def toString = "P2PKScriptPubKeyImpl(" + hex + ")"
  }

  def apply(pubKey: ECPublicKey): P2PKScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOps ++ Seq(ScriptConstant(pubKey.bytes), OP_CHECKSIG)
    P2PKScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptPubKey = {
    buildScript(asm, P2PKScriptPubKeyImpl(_), isP2PKScriptPubKey(_), "Given asm was not a p2pk scriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2PKScriptPubKey = fromAsm(asm)

  /** Sees if the given asm matches the [[P2PKHScriptPubKey]] pattern */
  def isP2PKScriptPubKey(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(b: BytesToPushOntoStack, x: ScriptConstant, OP_CHECKSIG) => true
    case _ => false
  }

}

sealed trait LockTimeScriptPubKey extends ScriptPubKey {
  /** Determines the nested ScriptPubKey inside the LockTimeScriptPubKey */
  def nestedScriptPubKey: ScriptPubKey = {
    val bool: Boolean = asm.head.isInstanceOf[ScriptNumberOperation]
    bool match {
      case true => ScriptPubKey(asm.slice(3, asm.length))
      case false => ScriptPubKey(asm.slice(4, asm.length))
    }
  }

  /** The relative locktime value (i.e. the amount of time the output should remain unspendable) */
  def locktime: ScriptNumber = {
    asm.head match {
      case scriptNumOp: ScriptNumberOperation => ScriptNumber(scriptNumOp.toLong)
      case _: BytesToPushOntoStack => ScriptNumber(asm(1).hex)
      case x @ (_: ScriptConstant | _: ScriptOperation) => throw new IllegalArgumentException("In a LockTimeScriptPubKey, " +
        "the first asm must be either a ScriptNumberOperation (i.e. OP_5), or the BytesToPushOntoStack for the proceeding ScriptConstant.")
    }
  }
}

object LockTimeScriptPubKey extends ScriptFactory[LockTimeScriptPubKey] {

  def fromAsm(asm: Seq[ScriptToken]): LockTimeScriptPubKey = {
    require(isValidLockTimeScriptPubKey(asm))
    if (asm.contains(OP_CHECKLOCKTIMEVERIFY)) CLTVScriptPubKey(asm)
    else if (asm.contains(OP_CHECKSEQUENCEVERIFY)) CSVScriptPubKey(asm)
    else throw new IllegalArgumentException("Given asm was not a LockTimeScriptPubKey, got: " + asm)
  }

  def isValidLockTimeScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    CLTVScriptPubKey.isCLTVScriptPubKey(asm) || CSVScriptPubKey.isCSVScriptPubKey(asm)
  }
}

/**
 * Represents a scriptPubKey that contains OP_CHECKLOCKTIMEVERIFY.
 * Adds an absolute/defined locktime condition to any scriptPubKey.
 * [[https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki]]
 * Format: <locktime> OP_CLTV OP_DROP <scriptPubKey>
 */
sealed trait CLTVScriptPubKey extends LockTimeScriptPubKey

object CLTVScriptPubKey extends ScriptFactory[CLTVScriptPubKey] {
  private case class CLTVScriptPubKeyImpl(bytes: ByteVector) extends CLTVScriptPubKey {
    override def toString = "CLTVScriptPubKeyImpl(" + hex + ")"
  }

  def fromAsm(asm: Seq[ScriptToken]): CLTVScriptPubKey = {
    buildScript(asm, CLTVScriptPubKeyImpl(_), isCLTVScriptPubKey(_), "Given asm was not a CLTVScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): CLTVScriptPubKey = fromAsm(asm)

  def apply(locktime: ScriptNumber, scriptPubKey: ScriptPubKey): CLTVScriptPubKey = {
    val scriptOp = BitcoinScriptUtil.minimalScriptNumberRepresentation(locktime)

    val scriptNum: Seq[ScriptToken] = if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
      Seq(scriptOp)
    } else {
      val pushOpsLockTime = BitcoinScriptUtil.calculatePushOp(locktime.bytes)
      pushOpsLockTime ++ Seq(ScriptConstant(locktime.bytes))
    }

    val cltvAsm = Seq(OP_CHECKLOCKTIMEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ cltvAsm ++ scriptPubKeyAsm
    CLTVScriptPubKey(asm)
  }

  def isCLTVScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKLOCKTIMEVERIFY)) return false
      asm.slice(0, 4) match {
        case List(lockTimeBytesToPush: BytesToPushOntoStack, lockTime: ScriptConstant, OP_CHECKLOCKTIMEVERIFY, OP_DROP) =>
          validScriptAfterLockTime(tailTokens)
        case _ => false
      }
    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKLOCKTIMEVERIFY)) return false
      asm.slice(0, 3) match {
        case List(scriptNumOp: ScriptNumberOperation, OP_CHECKLOCKTIMEVERIFY, OP_DROP) =>
          validScriptAfterLockTime(tailTokens)
        case _ => false
      }
    }
  }

  /**
   * We need this check because sometimes we can get very lucky in having a non valid
   * lock time script that has the first 4 bytes as a valid locktime script
   * and then the bytes after the first 4 bytes gets lucky and is parsed by our [[ScriptParser]]
   * A good way to see if this is _actually_ a valid script is by checking if we have any
   * [[UndefinedOP_NOP]] in the script, which means we definitely don't have a valid locktime script
   * See this example of what happened before we added this check:
   *
   * [[https://travis-ci.org/bitcoin-s/bitcoin-s-core/builds/201652191#L2526]]
   */
  def validScriptAfterLockTime(asm: Seq[ScriptToken]): Boolean = {
    !asm.exists(_.isInstanceOf[UndefinedOP_NOP])
  }
}

/**
 * Represents a scriptPubKey that contains OP_CHECKSEQUENCEVERIFY.
 * Adds a relative lockTime condition to any scriptPubKey.
 * https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
 * Format: <locktime> OP_CSV OP_DROP <scriptPubKey>
 */
sealed trait CSVScriptPubKey extends LockTimeScriptPubKey

object CSVScriptPubKey extends ScriptFactory[CSVScriptPubKey] {
  private case class CSVScriptPubKeyImpl(bytes: ByteVector) extends CSVScriptPubKey {
    override def toString = "CSVScriptPubKeyImpl(" + hex + ")"
  }

  def fromAsm(asm: Seq[ScriptToken]): CSVScriptPubKey = {
    buildScript(asm, CSVScriptPubKeyImpl(_), isCSVScriptPubKey(_), "Given asm was not a CSVScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): CSVScriptPubKey = fromAsm(asm)

  def apply(relativeLockTime: ScriptNumber, scriptPubKey: ScriptPubKey): CSVScriptPubKey = {
    val scriptOp = BitcoinScriptUtil.minimalScriptNumberRepresentation(relativeLockTime)

    val scriptNum: Seq[ScriptToken] = if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
      Seq(scriptOp)
    } else {
      val pushOpsLockTime = BitcoinScriptUtil.calculatePushOp(relativeLockTime.bytes)
      pushOpsLockTime ++ Seq(ScriptConstant(relativeLockTime.bytes))
    }

    val csvAsm = Seq(OP_CHECKSEQUENCEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ csvAsm ++ scriptPubKeyAsm
    CSVScriptPubKey(asm)
  }

  def isCSVScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKSEQUENCEVERIFY)) return false
      asm.slice(0, 4) match {
        case List(lockTimeBytesToPush: BytesToPushOntoStack, lockTime: ScriptConstant, OP_CHECKSEQUENCEVERIFY, OP_DROP) =>
          CLTVScriptPubKey.validScriptAfterLockTime(tailTokens)
        case _ => false
      }
    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (P2SHScriptPubKey.isP2SHScriptPubKey(tailTokens) || tailTokens.contains(OP_CHECKSEQUENCEVERIFY)) return false
      asm.slice(0, 3) match {
        case List(numberOp: ScriptNumberOperation, OP_CHECKSEQUENCEVERIFY, OP_DROP) =>
          CLTVScriptPubKey.validScriptAfterLockTime(tailTokens)
        case _ => false
      }
    }
  }

}

sealed trait NonStandardScriptPubKey extends ScriptPubKey

object NonStandardScriptPubKey extends ScriptFactory[NonStandardScriptPubKey] {
  private case class NonStandardScriptPubKeyImpl(bytes: ByteVector) extends NonStandardScriptPubKey {
    override def toString = "NonStandardScriptPubKeyImpl(" + hex + ")"
  }

  def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptPubKey = {
    //everything can be a NonStandardScriptPubkey, thus the trivially true function
    buildScript(asm, NonStandardScriptPubKeyImpl(_), { _ => true }, "")
  }

  def apply(asm: Seq[ScriptToken]): NonStandardScriptPubKey = fromAsm(asm)
}

/** Represents the empty ScriptPubKey */
case object EmptyScriptPubKey extends ScriptPubKey {
  override def bytes = ByteVector.low(1)
}

/** Factory companion object used to create ScriptPubKey objects */
object ScriptPubKey extends ScriptFactory[ScriptPubKey] {
  def empty: ScriptPubKey = fromAsm(Nil)

  /** Creates a scriptPubKey from its asm representation */
  def fromAsm(asm: Seq[ScriptToken]): ScriptPubKey = asm match {
    case Nil => EmptyScriptPubKey
    case _ if P2PKHScriptPubKey.isP2PKHScriptPubKey(asm) => P2PKHScriptPubKey(asm)
    case _ if P2SHScriptPubKey.isP2SHScriptPubKey(asm) => P2SHScriptPubKey(asm)
    case _ if P2PKScriptPubKey.isP2PKScriptPubKey(asm) => P2PKScriptPubKey(asm)
    case _ if MultiSignatureScriptPubKey.isMultiSignatureScriptPubKey(asm) => MultiSignatureScriptPubKey(asm)
    case _ if CLTVScriptPubKey.isCLTVScriptPubKey(asm) => CLTVScriptPubKey(asm)
    case _ if CSVScriptPubKey.isCSVScriptPubKey(asm) => CSVScriptPubKey(asm)
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => WitnessScriptPubKey(asm).get
    case _ if WitnessCommitment.isWitnessCommitment(asm) => WitnessCommitment(asm)
    case _ if EscrowTimeoutScriptPubKey.isValidEscrowTimeout(asm) => EscrowTimeoutScriptPubKey.fromAsm(asm)
    case _ => NonStandardScriptPubKey(asm)
  }

  def apply(asm: Seq[ScriptToken]): ScriptPubKey = fromAsm(asm)
}

/** This type represents a [[ScriptPubKey]] to evaluate a [[ScriptWitness]] */
sealed trait WitnessScriptPubKey extends ScriptPubKey {
  def witnessProgram: Seq[ScriptToken]
  def witnessVersion = WitnessVersion(asm.head)
}

object WitnessScriptPubKey {

  /** Witness scripts must begin with one of these operations, see BIP141 */
  val validWitVersions: Seq[ScriptNumberOperation] = Seq(OP_0, OP_1, OP_2, OP_3, OP_4, OP_5, OP_6, OP_7, OP_8,
    OP_9, OP_10, OP_11, OP_12, OP_13, OP_14, OP_15, OP_16)

  val unassignedWitVersions = validWitVersions.tail

  def apply(asm: Seq[ScriptToken]): Option[WitnessScriptPubKey] = fromAsm(asm)

  def fromAsm(asm: Seq[ScriptToken]): Option[WitnessScriptPubKey] = asm match {
    case _ if P2WPKHWitnessSPKV0.isValid(asm) => Some(P2WPKHWitnessSPKV0.fromAsm(asm))
    case _ if P2WSHWitnessSPKV0.isValid(asm) => Some(P2WSHWitnessSPKV0.fromAsm(asm))
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => Some(UnassignedWitnessScriptPubKey(asm))
    case _ => None
  }

  /**
   * Checks if the given asm is a valid [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]]
   * Mimics this function inside of Bitcoin Core
   * [[https://github.com/bitcoin/bitcoin/blob/14d01309bed59afb08651f2b701ff90371b15b20/src/script/script.cpp#L223-L237]]
   */
  def isWitnessScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    val bytes = BitcoinSUtil.toByteVector(asm)
    val firstOp = asm.headOption
    if (bytes.size < 4 || bytes.size > 42) false
    else if (!validWitVersions.contains(firstOp.getOrElse(OP_1NEGATE))) false
    else if (asm(1).toLong + 2 == bytes.size) true
    else false
  }
}

/** Represents a [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program]] */
sealed abstract class WitnessScriptPubKeyV0 extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object WitnessScriptPubKeyV0 {

  /**
   * Mimics the function to determine if a [[ScriptPubKey]] contains a witness
   * A witness program is any valid [[ScriptPubKey]] that consists of a 1 byte push op and then a data push
   * between 2 and 40 bytes
   * Verison 0 witness program need to have an OP_0 as the first operation
   * [[https://github.com/bitcoin/bitcoin/blob/449f9b8debcceb61a92043bc7031528a53627c47/src/script/script.cpp#L215-L229]]
   */
  def isValid(asm: Seq[ScriptToken]): Boolean = {
    WitnessScriptPubKey.isWitnessScriptPubKey(asm) && asm.headOption == Some(OP_0)
  }
}

/**
 * Represents the pay-to-witness-pubkeyhash script pubkey type as defined in BIP141
 * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#P2WPKH]]
 */
sealed abstract class P2WPKHWitnessSPKV0 extends WitnessScriptPubKeyV0 {
  def pubKeyHash: Sha256Hash160Digest = Sha256Hash160Digest(asm(3).bytes)
  override def toString = s"P2WPKHWitnessSPKV0($hex)"
}

object P2WPKHWitnessSPKV0 extends ScriptFactory[P2WPKHWitnessSPKV0] {
  private case class P2WPKHWitnessSPKV0Impl(bytes: ByteVector) extends P2WPKHWitnessSPKV0

  override def fromAsm(asm: Seq[ScriptToken]): P2WPKHWitnessSPKV0 = {
    buildScript(asm, P2WPKHWitnessSPKV0Impl(_), isValid(_), s"Given asm was not a P2WPKHWitnessSPKV0, got $asm")
  }

  def isValid(asm: Seq[ScriptToken]): Boolean = {
    val asmBytes = BitcoinSUtil.toByteVector(asm)
    WitnessScriptPubKeyV0.isValid(asm) &&
      asmBytes.size == 22
  }

  def fromHash(hash: Sha256Hash160Digest): P2WPKHWitnessSPKV0 = {
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }

  /** Creates a P2WPKH witness script pubkey */
  def apply(pubKey: ECPublicKey): P2WPKHWitnessSPKV0 = {
    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#restrictions-on-public-key-type
    require(pubKey.isCompressed, s"Public key must be compressed to be used in a segwit script, see BIP143")
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    fromHash(hash)
  }
}

/**
 * Reprents the pay-to-witness-scripthash script pubkey type as defined in BIP141
 * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#p2wsh]]
 */
sealed abstract class P2WSHWitnessSPKV0 extends WitnessScriptPubKeyV0 {
  def scriptHash: Sha256Digest = Sha256Digest(asm(3).bytes)
  override def toString = s"P2WSHWitnessSPKV0($hex)"
}

object P2WSHWitnessSPKV0 extends ScriptFactory[P2WSHWitnessSPKV0] {
  private case class P2WSHWitnessSPKV0Impl(bytes: ByteVector) extends P2WSHWitnessSPKV0

  override def fromAsm(asm: Seq[ScriptToken]): P2WSHWitnessSPKV0 = {
    buildScript(asm, P2WSHWitnessSPKV0Impl(_), isValid(_), s"Given asm was not a P2WSHWitnessSPKV0, got $asm")
  }

  def isValid(asm: Seq[ScriptToken]): Boolean = {
    val asmBytes = BitcoinSUtil.toByteVector(asm)
    WitnessScriptPubKeyV0.isValid(asm) &&
      asmBytes.size == 34
  }

  def fromHash(hash: Sha256Digest): P2WSHWitnessSPKV0 = {
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }

  def apply(spk: ScriptPubKey): P2WSHWitnessSPKV0 = {
    require(BitcoinScriptUtil.isOnlyCompressedPubKey(spk), s"Public key must be compressed to be used in a segwit script, see BIP143")
    val hash = CryptoUtil.sha256(spk.asmBytes)
    fromHash(hash)
  }
}

/** Type to represent all [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]]s we have not used yet in the bitcoin protocol */
sealed trait UnassignedWitnessScriptPubKey extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object UnassignedWitnessScriptPubKey extends ScriptFactory[UnassignedWitnessScriptPubKey] {
  private case class UnassignedWitnessScriptPubKeyImpl(bytes: ByteVector) extends UnassignedWitnessScriptPubKey {
    override def toString = "UnassignedWitnessScriptPubKeyImpl(" + hex + ")"
  }

  override def fromAsm(asm: Seq[ScriptToken]): UnassignedWitnessScriptPubKey = {
    buildScript(asm, UnassignedWitnessScriptPubKeyImpl(_), WitnessScriptPubKey.isWitnessScriptPubKey(_),
      "Given asm was not a valid witness script pubkey: " + asm)
  }
  def apply(asm: Seq[ScriptToken]): UnassignedWitnessScriptPubKey = fromAsm(asm)
}

/**
 * This trait represents the witness commitment found in the coinbase transaction
 * This is needed to commit to the wtxids of all of the witness transactions, since the merkle tree
 * does not commit to the witnesses for all [[org.bitcoins.core.protocol.transaction.WitnessTransaction]]
 * See BIP141 for more info
 * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure]]
 */
sealed trait WitnessCommitment extends ScriptPubKey {
  /** The commitment to the [[WitnessTransaction]]s in the [[Block]] */
  def witnessRootHash: DoubleSha256Digest = DoubleSha256Digest(asm(2).bytes.splitAt(4)._2)
}

object WitnessCommitment extends ScriptFactory[WitnessCommitment] {
  private case class WitnessCommitmentImpl(bytes: ByteVector) extends WitnessCommitment {
    override def toString = "WitnessCommitmentImpl(" + hex + ")"
  }

  /** Every witness commitment must start with this header, see BIP141 for details */
  private val commitmentHeader = "aa21a9ed"

  def apply(asm: Seq[ScriptToken]): WitnessCommitment = fromAsm(asm)

  override def fromAsm(asm: Seq[ScriptToken]): WitnessCommitment = {
    buildScript(asm, WitnessCommitmentImpl(_), isWitnessCommitment(_), "Given asm was not a valid witness commitment, got: " + asm)
  }

  def apply(hash: DoubleSha256Digest): WitnessCommitment = {
    WitnessCommitment(Seq(OP_RETURN, BytesToPushOntoStack(36), ScriptConstant(commitmentHeader + hash.hex)))
  }
  /**
   * This determines if the given asm has the correct witness structure according to BIP141
   * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure]]
   */
  def isWitnessCommitment(asm: Seq[ScriptToken]): Boolean = {
    if (asm.size < 3) false
    else {
      val minCommitmentSize = 38
      val asmBytes = BitcoinSUtil.toByteVector(asm)
      val Seq(opReturn, pushOp, constant) = asm.take(3)
      opReturn == OP_RETURN && pushOp == BytesToPushOntoStack(36) &&
        constant.hex.take(8) == commitmentHeader && asmBytes.size >= minCommitmentSize
    }
  }
}

/**
 * Represents a [[ScriptPubKey]] that either times out allowing Alice to spend from the scriptpubkey
 * or allows a federation to spend from the escrow
 * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki#contracts-with-expiration-deadlines]]
 * Format: OP_IF <multsig scriptpubkey> OP_ELSE <locktime scriptPubKey> OP_ENDIF
 */
sealed trait EscrowTimeoutScriptPubKey extends ScriptPubKey {
  /** The [[MultiSignatureScriptPubKey]] that can be used to spend funds */
  def escrow: MultiSignatureScriptPubKey = {
    val escrowAsm = asm.slice(1, opElseIndex)
    MultiSignatureScriptPubKey(escrowAsm)
  }

  /** The [[CSVScriptPubKey]] that you can spend funds from after a certain timeout */
  def timeout: LockTimeScriptPubKey = {
    val timeoutAsm = asm.slice(opElseIndex + 1, asm.length - 1)
    LockTimeScriptPubKey.fromAsm(timeoutAsm)
  }

  private def opElseIndex: Int = {
    val idx = asm.indexOf(OP_ELSE)
    require(idx != -1, "CSVEscrowWithTimeout has to contain OP_ELSE asm token")
    idx
  }
}

object EscrowTimeoutScriptPubKey extends ScriptFactory[EscrowTimeoutScriptPubKey] {
  private case class EscrowTimeoutScriptPubKeyImpl(bytes: ByteVector) extends EscrowTimeoutScriptPubKey {
    override def toString = "EscrowTimeoutScriptPubKeyImpl(" + hex + ")"
  }

  override def fromAsm(asm: Seq[ScriptToken]): EscrowTimeoutScriptPubKey = {
    buildScript(asm, EscrowTimeoutScriptPubKeyImpl(_), isValidEscrowTimeout(_),
      "Given asm was not a valid CSVEscrowTimeout, got: " + asm)
  }

  def isValidEscrowTimeout(asm: Seq[ScriptToken]): Boolean = {
    val opElseIndex = asm.indexOf(OP_ELSE)
    val correctControlStructure = asm.headOption.contains(OP_IF) && asm.last == OP_ENDIF && opElseIndex != -1
    if (correctControlStructure) {
      val escrowAsm = asm.slice(1, opElseIndex)
      val escrow = Try(MultiSignatureScriptPubKey(escrowAsm))
      val timeoutAsm = asm.slice(opElseIndex + 1, asm.length - 1)
      val timeout = Try(LockTimeScriptPubKey.fromAsm(timeoutAsm))
      escrow.isSuccess && timeout.isSuccess
    } else false
  }

  def apply(escrow: MultiSignatureScriptPubKey, timeout: LockTimeScriptPubKey): EscrowTimeoutScriptPubKey = {
    fromAsm(Seq(OP_IF) ++ escrow.asm ++ Seq(OP_ELSE) ++ timeout.asm ++ Seq(OP_ENDIF))
  }
}

