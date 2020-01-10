package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{
  ECDigitalSignature,
  ECPublicKey,
  EmptyDigitalSignature
}
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinScriptUtil}
import scodec.bits.ByteVector

/**
  * Created by chris on 11/10/16.
  * The witness used to evaluate a [[RawScriptPubKey]] inside of Bitcoin
  * [[https://github.com/bitcoin/bitcoin/blob/57b34599b2deb179ff1bd97ffeab91ec9f904d85/src/script/script.h#L648-L660]]
  */
sealed abstract class ScriptWitness extends NetworkElement {

  /** The byte vectors that are placed on to the stack when evaluating a witness program */
  val stack: Seq[ByteVector]

}

case object EmptyScriptWitness extends ScriptWitness {
  override val stack: Seq[ByteVector] = Vector.empty

  override val bytes: ByteVector = ByteVector.low(1)
}

sealed abstract class ScriptWitnessV0 extends ScriptWitness {
  override val bytes: ByteVector = RawScriptWitnessParser.write(this)
}

/**
  * Represents a [[org.bitcoins.core.protocol.script.ScriptWitness]] that is needed to spend a
  * [[P2WPKHWitnessV0]] scriptPubKey
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#p2wpkh-nested-in-bip16-p2sh]]
  * Format: <pubKey> <signature>
  */
sealed abstract class P2WPKHWitnessV0 extends ScriptWitnessV0 {
  def pubKey: ECPublicKey = ECPublicKey(stack.head)

  def signature: ECDigitalSignature = stack(1) match {
    case ByteVector.empty  => EmptyDigitalSignature
    case bytes: ByteVector => ECDigitalSignature(bytes)
  }

  override def toString =
    s"P2WPKHWitnessV0(${stack.map(BitcoinSUtil.encodeHex(_)).toString})"
}

object P2WPKHWitnessV0 {
  private case class P2WPKHWitnessV0Impl(stack: Seq[ByteVector])
      extends P2WPKHWitnessV0

  private def apply(stack: Seq[ByteVector]): P2WPKHWitnessV0 =
    P2WPKHWitnessV0Impl(stack)

  def apply(pubKey: ECPublicKey): P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(pubKey, EmptyDigitalSignature)
  }

  def apply(
      publicKey: ECPublicKey,
      signature: ECDigitalSignature): P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(Seq(publicKey.bytes, signature.bytes))
  }

  def fromP2PKHScriptSig(scriptSig: ScriptSignature): P2WPKHWitnessV0 =
    scriptSig match {
      case p2pkh: P2PKHScriptSignature =>
        P2WPKHWitnessV0(p2pkh.publicKey, p2pkh.signature)
      case x @ (_: LockTimeScriptSignature | _: MultiSignatureScriptSignature |
          _: ConditionalScriptSignature | _: NonStandardScriptSignature |
          _: P2PKScriptSignature | _: P2SHScriptSignature |
          EmptyScriptSignature) =>
        throw new IllegalArgumentException(
          s"Expected P2PKHScriptSignature, got $x")
    }
}

/**
  * Represents a [[ScriptWitness]] that is needed to spend a
  * [[P2WSHWitnessV0]] scriptPubKey
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#p2wsh]]
  * Format: <redeem script> <scriptSig data1> <scriptSig data2> ... <scriptSig dataN>
  */
sealed abstract class P2WSHWitnessV0 extends ScriptWitnessV0 {
  lazy val redeemScript: RawScriptPubKey = {
    val cmpct = CompactSizeUInt.calc(stack.head)
    RawScriptPubKey.fromBytes(cmpct.bytes ++ stack.head)
  }

  lazy val signatures: Vector[ECDigitalSignature] = {
    // ECDigital signatures are between 71 and 73 bytes long
    // with a exponential decay on the probability of smaller sigs
    // [[https://en.bitcoin.it/wiki/Elliptic_Curve_Digital_Signature_Algorithm]]
    val relevantStack = stack.toVector.tail.filter(bytes =>
      bytes.length >= 67 && bytes.length <= 73)

    relevantStack.map(ECDigitalSignature.fromBytes)
  }

  override def toString =
    s"P2WSHWitnessV0(${stack.map(BitcoinSUtil.encodeHex(_)).toString})"
}

object P2WSHWitnessV0 {
  private case class P2WSHWitnessV0Impl(stack: Seq[ByteVector])
      extends P2WSHWitnessV0

  def apply(spk: RawScriptPubKey): P2WSHWitnessV0 = {
    P2WSHWitnessV0(spk, EmptyScriptSignature)
  }

  def apply(
      spk: RawScriptPubKey,
      scriptSig: ScriptSignature): P2WSHWitnessV0 = {
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    //remove pushops from scriptSig
    val minimalIf = BitcoinScriptUtil.minimalIfOp(scriptSig.asm)
    val noPushOps = BitcoinScriptUtil.filterPushOps(minimalIf)
    val minimalDummy = BitcoinScriptUtil.minimalDummy(noPushOps).reverse
    val stack: Seq[ByteVector] = spk.asmBytes +: minimalDummy.map(_.bytes)
    P2WSHWitnessV0(stack)
  }

  private def apply(stack: Seq[ByteVector]): P2WSHWitnessV0 = {
    P2WSHWitnessV0Impl(stack)
  }

  def apply(spk: RawScriptPubKey, stack: Seq[ByteVector]): P2WSHWitnessV0 = {
    val fullStack: Seq[ByteVector] = spk.asmBytes +: stack
    P2WSHWitnessV0(fullStack)
  }
}

object ScriptWitness {

  def apply(stack: Seq[ByteVector]): ScriptWitness = {
    //TODO: eventually only compressed public keys will be allowed in v0 scripts
    //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#restrictions-on-public-key-type
    val isPubKey = {
      stack.nonEmpty &&
      ECPublicKey.isFullyValid(stack.head) &&
      (stack.head.size == 33
      || stack.head.size == 65)
    }
    if (stack.isEmpty) {
      EmptyScriptWitness
    } else if (isPubKey && stack.size == 2) {
      val pubKey = ECPublicKey(stack.head)
      val sig = ECDigitalSignature(stack(1))
      P2WPKHWitnessV0(pubKey, sig)
    } else if (isPubKey && stack.size == 1) {
      val pubKey = ECPublicKey(stack.head)
      P2WPKHWitnessV0(pubKey)
    } else {
      //wont match a Vector if I don't convert to list
      val s = stack.toList
      s match {
        case Nil =>
          EmptyScriptWitness
        case h :: t =>
          val cmpct = CompactSizeUInt.calc(h)
          val spk = RawScriptPubKey(cmpct.bytes ++ h)
          P2WSHWitnessV0(spk, t)
      }
    }
  }
}
