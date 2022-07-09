package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant._
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._
import org.bitcoins.core.wallet.utxo.ConditionalPath
import org.bitcoins.crypto.{ECDigitalSignature, ECPublicKey, ECPublicKeyBytes}

import scala.annotation.tailrec
import scala.util.Try

/** Created by chris on 12/26/15.
  *
  * We only give standard types to ScriptSignatures that are Policy
  * compliant. This is because if we wanted to be closer to Consensus
  * compliant then it would be near impossible to type things.
  *
  * For example almost anything could be a ConditionalScriptSignature
  * since in consensus logic almost any ScriptToken is interpreted as True,
  * while under Policy only OP_TRUE is True.
  */
sealed abstract class ScriptSignature extends Script {

  /** The digital signatures contained inside of the script signature
    * p2pkh script signatures only have one sig
    * p2pk script signatures only have one sigs
    * p2sh script signatures can have m sigs
    * multisignature scripts can have m sigs
    */
  def signatures: Seq[ECDigitalSignature]

}

sealed trait NonStandardScriptSignature extends ScriptSignature {
  def signatures: Seq[ECDigitalSignature] = Nil
  override def toString = s"NonStandardScriptSignature($asm)"
}

object NonStandardScriptSignature
    extends ScriptFactory[NonStandardScriptSignature] {

  private case class NonStandardScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends NonStandardScriptSignature

  def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = NonStandardScriptSignatureImpl(_),
                errorMsg = "")
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    true
  }
}

/** A script signature to be used in tests for signing EmptyScriptPubKey.
  * This script pushes a true onto the stack, causing a successful spend.
  */
case object TrivialTrueScriptSignature extends ScriptSignature {
  override lazy val signatures: Seq[ECDigitalSignature] = Nil

  override lazy val asm: Vector[ScriptToken] =
    Vector(OP_TRUE)

  def isValid(asm: Seq[ScriptToken]): Boolean = {
    asm == this.asm
  }
}

/** P2PKH script signatures have only one public key
  * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
  * P2PKH scriptSigs follow this format
  * <sig> <pubkey>
  */
sealed trait P2PKHScriptSignature extends ScriptSignature {

  /** P2PKH scriptSigs only have one signature */
  def signature: ECDigitalSignature = signatures.head

  /** Gives us the public key inside of a p2pkh script signature */
  def publicKey: ECPublicKeyBytes = ECPublicKeyBytes(asm.last.bytes)

  override def signatures: Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(asm(1).hex))
  }

  override def toString = s"P2PKHScriptSignature($publicKey, $signature)"

}

object P2PKHScriptSignature extends ScriptFactory[P2PKHScriptSignature] {

  private case class P2PKHScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends P2PKHScriptSignature

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptSignature = {
    buildScript(
      asm = asm.toVector,
      constructor = P2PKHScriptSignatureImpl(_),
      errorMsg = s"Given asm was not a P2PKHScriptSignature, got: $asm"
    )
  }

  /** Builds a P2PKH ScriptSig from a signature and raw ECPublicKeyBytes (may be invalid). */
  private[core] def apply(
      signature: ECDigitalSignature,
      pubKeyBytes: ECPublicKeyBytes): P2PKHScriptSignature = {
    val signatureBytesToPushOntoStack =
      BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val pubKeyBytesToPushOntoStack =
      BitcoinScriptUtil.calculatePushOp(pubKeyBytes.bytes)
    val asm: Seq[ScriptToken] =
      signatureBytesToPushOntoStack ++ Seq(ScriptConstant(signature.bytes)) ++
        pubKeyBytesToPushOntoStack ++ Seq(ScriptConstant(pubKeyBytes.bytes))
    fromAsm(asm)
  }

  /** Builds a script signature from a digital signature and a public key
    * this is a pay to public key hash script sig
    */
  def apply(
      signature: ECDigitalSignature,
      pubKey: ECPublicKey): P2PKHScriptSignature = {
    P2PKHScriptSignature(signature, pubKey.toPublicKeyBytes())
  }

  /** Determines if the given asm matches a [[P2PKHScriptSignature]] */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean =
    asm match {
      case Seq(_: BytesToPushOntoStack,
               maybeSig: ScriptConstant,
               _: BytesToPushOntoStack,
               maybeKey: ScriptConstant) =>
        if (
          (maybeKey.bytes.length == 33 || maybeKey.bytes.length == 65) &&
          (maybeSig.bytes.length > 68 && maybeSig.bytes.length < 73)
        ) true
        else !P2SHScriptSignature.isRedeemScript(maybeKey)
      case _ => false
    }
}

/** Represents a pay-to-script-hash script signature
  * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
  * P2SH scriptSigs have the following format
  * <sig> [sig] [sig...] <redeemScript>
  */
sealed trait P2SHScriptSignature extends ScriptSignature {

  /** The redeemScript represents the conditions that must be satisfied to spend the output */
  def redeemScript: ScriptPubKey = {
    val scriptSig = scriptSignatureNoRedeemScript
    if (asm.isEmpty) {
      EmptyScriptPubKey
    } else if (
      scriptSig == EmptyScriptSignature &&
      WitnessScriptPubKey.isValidAsm(asm.tail)
    ) {
      //if we have an EmptyScriptSignature, we need to check if the rest of the asm
      //is a Witness script. It is not necessarily a witness script, since this code
      //path might be used for signing a normal p2sh spk in TransactionSignatureSerializer
      WitnessScriptPubKey(asm.tail)
    } else {
      ScriptPubKey.fromAsmBytes(asm.last.bytes)
    }

  }

  /** Returns the script signature of this p2shScriptSig with no serialized redeemScript */
  def scriptSignatureNoRedeemScript: ScriptSignature = {
    //witness scriptPubKeys always have EmptyScriptSigs
    if (WitnessScriptPubKey.isValidAsm(asm)) {
      EmptyScriptSignature
    } else {
      val asmWithoutRedeemScriptAndPushOp: Try[Seq[ScriptToken]] = Try {
        asm(asm.size - 2) match {
          case _: BytesToPushOntoStack => asm.dropRight(2)
          case _                       => asm.dropRight(3)
        }
      }
      val script =
        asmWithoutRedeemScriptAndPushOp.getOrElse(EmptyScriptSignature.asm)
      ScriptSignature.fromAsm(script)
    }
  }

  /** Returns the public keys for the p2sh scriptSignature */
  def publicKeys: Seq[ECPublicKeyBytes] = {
    val pubKeys: Seq[ScriptToken] = redeemScript.asm
      .filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
    pubKeys.map(k => ECPublicKeyBytes(k.bytes))
  }

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    val sigs = {
      scriptSignatureNoRedeemScript.asm
        .filter(_.isInstanceOf[ScriptConstant])
        .filterNot(_.isInstanceOf[ScriptNumberOperation])
        .filterNot(_.hex.length < 100)
    }
    sigs.map(s => ECDigitalSignature(s.hex))
  }

  override def toString =
    s"P2SHScriptSignature($redeemScript, $scriptSignatureNoRedeemScript)"
}

object P2SHScriptSignature extends ScriptFactory[P2SHScriptSignature] {

  private case class P2SHScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends P2SHScriptSignature

  def apply(
      scriptSig: ScriptSignature,
      redeemScript: ScriptPubKey): P2SHScriptSignature = {
    //we need to calculate the size of the redeemScript and add the corresponding push op
    val serializedRedeemScript = ScriptConstant(redeemScript.asmBytes)
    val pushOps = BitcoinScriptUtil.calculatePushOp(serializedRedeemScript)
    val asm: Seq[ScriptToken] =
      scriptSig.asm ++ pushOps ++ Seq(serializedRedeemScript)
    fromAsm(asm)
  }

  def apply(witnessScriptPubKey: WitnessScriptPubKey): P2SHScriptSignature = {
    P2SHScriptSignature(EmptyScriptSignature, witnessScriptPubKey)
  }

  override def buildScript(
      asm: Vector[ScriptToken],
      constructor: Vector[ScriptToken] => P2SHScriptSignature,
      errorMsg: String): P2SHScriptSignature = {
    //everything can be a P2SHScriptSignature, thus passing the trivially true function
    //the most important thing to note is we cannot have a P2SHScriptSignature unless
    //we have a P2SHScriptPubKey
    //previously P2SHScriptSignature's redeem script had to be standard scriptPubKey's, this
    //was removed in 0.11 or 0.12 of Bitcoin Core
    constructor(asm)
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2SHScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = P2SHScriptSignatureImpl(_),
                errorMsg =
                  s"Given asm tokens are not a p2sh scriptSig, got: $asm")
  }

  /** Tests if the given asm tokens are a [[P2SHScriptSignature]] */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    //as noted above, technically _anything_ can be a p2sh scriptsig
    //this applies basic checks to see if it's a standard redeemScript
    //rather than a non standard redeeScript.

    //this will return false if the redeemScript is not a
    //supported scriptpubkey type in our library
    asm.size > 1 && isRedeemScript(asm.last)
  }

  /** Detects if the given script token is a redeem script */
  def isRedeemScript(token: ScriptToken): Boolean = {
    token match {
      case _: ScriptNumberOperation | _: ScriptOperation =>
        //more cheap checks, we can't have a redeemScript
        //if the token is OP_0/OP_1/OP_CHECKSIG etc
        false
      case constant: ScriptConstant =>
        val redeemScript: ScriptPubKey = parseRedeemScript(constant)
        isStandardNonP2SH(redeemScript, isRecursiveCall = false)
    }
  }

  @tailrec
  private def isStandardNonP2SH(
      spk: ScriptPubKey,
      isRecursiveCall: Boolean): Boolean = {
    spk match {
      case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2PKScriptPubKey | _: P2PKWithTimeoutScriptPubKey |
          _: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0 |
          _: TaprootScriptPubKey |
          _: ConditionalScriptPubKey => // Conditional SPKs are not recursively checked
        true
      case EmptyScriptPubKey => isRecursiveCall // Fine if nested
      case locktime: LockTimeScriptPubKey =>
        if (Try(locktime.locktime).isSuccess) {
          isStandardNonP2SH(locktime.nestedScriptPubKey, isRecursiveCall = true)
        } else {
          false
        }
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: P2SHScriptPubKey =>
        false
    }
  }

  /** Parses a redeem script from the given script token */
  def parseRedeemScript(scriptToken: ScriptToken): ScriptPubKey = {
    val asm = ScriptParser.fromBytes(scriptToken.bytes)
    val redeemScript: ScriptPubKey = ScriptPubKey.fromAsm(asm)
    redeemScript
  }
}

/** Represents a multisignature script signature
  * https://bitcoin.org/en/developer-guide#multisig
  * Multisig script sigs have the following format
  * OP_0 <A sig> [B sig] [C sig...]
  */
sealed trait MultiSignatureScriptSignature extends ScriptSignature {

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    asm.tail
      .filter(_.isInstanceOf[ScriptConstant])
      .map(sig => ECDigitalSignature(sig.hex))
  }

  override def toString =
    s"MultiSignatureScriptSignature(${signatures.mkString(", ")})"
}

object MultiSignatureScriptSignature
    extends ScriptFactory[MultiSignatureScriptSignature] {

  private case class MultiSignatureScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends MultiSignatureScriptSignature

  def apply(
      signatures: Seq[ECDigitalSignature]): MultiSignatureScriptSignature = {
    val sigsPushOpsPairs: Seq[Seq[ScriptToken]] = for {
      sig <- signatures
      constant = ScriptConstant(sig.bytes)
      pushOps = BitcoinScriptUtil.calculatePushOp(sig.bytes)
    } yield pushOps ++ Seq(constant)
    val sigsWithPushOps = sigsPushOpsPairs.flatten
    //OP_0 is for the dummy input required by OP_CHECKMULTISIG
    val asm = OP_0 +: sigsWithPushOps
    MultiSignatureScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptSignature = {
    buildScript(
      asm = asm.toVector,
      constructor = MultiSignatureScriptSignatureImpl(_),
      errorMsg =
        s"The given asm tokens were not a multisignature script sig: $asm"
    )
  }

  /** Checks if the given script tokens are a multisignature script sig
    * format: OP_0 <A sig> [B sig] [C sig...]
    *
    * @param asm the asm to check if it falls in the multisignature script sig format
    * @return boolean indicating if the scriptsignature is a multisignature script signature
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean =
    asm.isEmpty match {
      case true => false
      //case false if (asm.size == 1) => false
      case false =>
        val firstTokenIsScriptNumberOperation =
          asm.head.isInstanceOf[ScriptNumberOperation]
        if (firstTokenIsScriptNumberOperation) {
          //avoid doing this computation unless we think it need to be done
          //fail fast
          isPushOpsOrScriptConstants(asm.tail)
        } else {
          false
        }
    }

  /** Iterates through the given given script tokens and return false if
    * one of the elements is NOT [[ScriptConstant]] or a push operation
    */
  private def isPushOpsOrScriptConstants(asm: Seq[ScriptToken]): Boolean = {
    asm.forall(token =>
      token.isInstanceOf[ScriptConstant] ||
        StackPushOperationFactory.isPushOperation(token))
  }
}

/** Represents a pay to public key script signature
  * https://bitcoin.org/en/developer-guide#pubkey
  * Signature script: <sig>
  */
sealed trait P2PKScriptSignature extends ScriptSignature {

  /** PubKey scriptSignatures only have one signature */
  def signature: ECDigitalSignature = signatures.head

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(asm).head.hex))
  }

  override def toString = s"P2PKScriptSignature($signature)"
}

object P2PKScriptSignature extends ScriptFactory[P2PKScriptSignature] {

  private case class P2PKScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends P2PKScriptSignature

  def apply(signature: ECDigitalSignature): P2PKScriptSignature = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val signatureConstant = ScriptConstant(signature.bytes)
    val asm = pushOps ++ Seq(signatureConstant)
    P2PKScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptSignature = {
    buildScript(asm.toVector,
                P2PKScriptSignatureImpl(_),
                "The given asm tokens were not a p2pk script sig: " + asm)
  }

  /** P2PK scriptSigs always have the pattern [pushop, digitalSignature] */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean =
    asm match {
      case Seq(_: BytesToPushOntoStack, _: ScriptConstant) => true
      case _                                               => false
    }
}

object P2PKWithTimeoutScriptSignature
    extends ScriptFactory[ConditionalScriptSignature] {

  override def fromAsm(asm: Seq[ScriptToken]): ConditionalScriptSignature = {
    buildScript(
      asm.toVector,
      ConditionalScriptSignature.fromAsm,
      s"The given asm tokens were not a P2PKWithTimeoutScriptSignature, got $asm"
    )
  }

  def apply(
      beforeTimeout: Boolean,
      signature: ECDigitalSignature): ConditionalScriptSignature = {
    ConditionalScriptSignature(P2PKScriptSignature(signature), beforeTimeout)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    P2PKScriptSignature.isValidAsm(
      asm.dropRight(1)) && ConditionalScriptSignature
      .isValidAsm(asm)
  }
}

/** Parent type for all lock time script signatures, these spend [[LockTimeScriptPubKey]] */
sealed trait LockTimeScriptSignature extends ScriptSignature {
  def scriptSig: ScriptSignature = ScriptSignature(hex)

  override def signatures: Seq[ECDigitalSignature] = scriptSig.signatures
}

sealed trait CLTVScriptSignature extends LockTimeScriptSignature {
  override def toString: String = s"CLTVScriptSignature($scriptSig)"
}

/** Note that extend [[org.bitcoins.core.protocol.script.ScriptFactory]] here
  * but technically ANYTHING can be a [[CLTVScriptSignature]] since the
  * [[CLTVScriptPubKey]] does not manipulate the stack
  */
object CLTVScriptSignature extends ScriptFactory[CLTVScriptSignature] {

  private case class CLTVScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends CLTVScriptSignature

  override def fromAsm(asm: Seq[ScriptToken]): CLTVScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = CLTVScriptSignatureImpl(_),
                errorMsg = s"Given asm was not a CLTVScriptSignature $asm")
  }

  override def fromHex(hex: String): CLTVScriptSignature = {
    CLTVScriptSignature(BytesUtil.decodeHex(hex))
  }

  def apply(scriptSig: ScriptSignature): CLTVScriptSignature = {
    fromHex(scriptSig.hex)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    true
  }
}

sealed trait CSVScriptSignature extends LockTimeScriptSignature {
  override def toString = s"CSVScriptSignature($scriptSig)"
}

object CSVScriptSignature extends ScriptFactory[CSVScriptSignature] {

  private case class CSVScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends CSVScriptSignature

  override def fromAsm(asm: Seq[ScriptToken]): CSVScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = CSVScriptSignatureImpl(_),
                errorMsg = s"Given asm was not a CLTVScriptSignature $asm")
  }

  override def fromHex(hex: String): CSVScriptSignature = {
    CSVScriptSignature(BytesUtil.decodeHex(hex))
  }

  def apply(scriptSig: ScriptSignature): CSVScriptSignature = {
    fromHex(scriptSig.hex)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = true
}

/** ScriptSignature for both OP_IF and OP_NOTIF ScriptPubKeys */
sealed trait ConditionalScriptSignature extends ScriptSignature {
  require(ConditionalScriptSignature.isValidAsm(asm),
          "ConditionalScriptSignature must end in true or false")

  def isTrue: Boolean = {
    BitcoinScriptUtil.castToBool(asm.last)
  }

  def isFalse: Boolean = {
    !isTrue
  }

  /** The ScriptSignature for the nested case being spent */
  def nestedScriptSig: ScriptSignature = {
    ScriptSignature.fromAsm(asm.dropRight(1))
  }

  override def signatures: Seq[ECDigitalSignature] = {
    nestedScriptSig.signatures
  }
}

object ConditionalScriptSignature
    extends ScriptFactory[ConditionalScriptSignature] {

  private case class ConditionalScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends ConditionalScriptSignature {

    override def toString: String =
      s"ConditionalScriptSignature($isTrue, $nestedScriptSig)"
  }

  override def fromAsm(asm: Seq[ScriptToken]): ConditionalScriptSignature = {
    buildScript(asm.toVector,
                ConditionalScriptSignatureImpl.apply,
                s"Given asm was not a ConditionalScriptSignature: $asm")
  }

  def apply(
      nestedScriptSig: ScriptSignature,
      condition: Boolean): ConditionalScriptSignature = {
    val conditionAsm = if (condition) {
      OP_TRUE
    } else {
      OP_FALSE
    }

    fromAsm(nestedScriptSig.asm.:+(conditionAsm))
  }

  @scala.annotation.tailrec
  def fromNestedScriptSig(
      nestedScriptSig: ScriptSignature,
      conditionalPath: ConditionalPath): ConditionalScriptSignature = {
    conditionalPath match {
      case ConditionalPath.NoCondition =>
        throw new IllegalArgumentException("ConditionalPath cannot be empty")
      case ConditionalPath.nonNestedTrue =>
        ConditionalScriptSignature(nestedScriptSig, condition = true)
      case ConditionalPath.nonNestedFalse =>
        ConditionalScriptSignature(nestedScriptSig, condition = false)
      case ConditionalPath.ConditionTrue(nextCondition) =>
        ConditionalScriptSignature.fromNestedScriptSig(
          ConditionalScriptSignature(nestedScriptSig, condition = true),
          nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        ConditionalScriptSignature.fromNestedScriptSig(
          ConditionalScriptSignature(nestedScriptSig, condition = false),
          nextCondition)
    }
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    asm.lastOption.exists(Vector(OP_0, OP_FALSE, OP_1, OP_TRUE).contains)
  }
}

/** Represents the empty script signature */
case object EmptyScriptSignature extends ScriptSignature {
  override def asm: Seq[ScriptToken] = Vector.empty
  def signatures = Vector.empty
}

object ScriptSignature extends ScriptFactory[ScriptSignature] {

  /** Returns an empty script signature */
  def empty: ScriptSignature = EmptyScriptSignature

  /** Creates a scriptSignature from the list of script tokens */
  override def fromAsm(tokens: Seq[ScriptToken]): ScriptSignature =
    tokens match {
      case Nil => EmptyScriptSignature
      case _ if TrivialTrueScriptSignature.isValid(tokens) =>
        TrivialTrueScriptSignature
      case _ if P2SHScriptSignature.isValidAsm(tokens) =>
        P2SHScriptSignature.fromAsm(tokens)
      case _ if ConditionalScriptSignature.isValidAsm(tokens) =>
        ConditionalScriptSignature.fromAsm(tokens)
      case _ if MultiSignatureScriptSignature.isValidAsm(tokens) =>
        MultiSignatureScriptSignature.fromAsm(tokens)
      case _ if P2PKHScriptSignature.isValidAsm(tokens) =>
        P2PKHScriptSignature.fromAsm(tokens)
      case _ if P2PKScriptSignature.isValidAsm(tokens) =>
        P2PKScriptSignature.fromAsm(tokens)
      case _ => NonStandardScriptSignature.fromAsm(tokens)
    }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = true
}
