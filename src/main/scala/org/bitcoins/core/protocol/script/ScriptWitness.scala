package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.{BitcoinSUtil, Factory}

/**
  * Created by chris on 11/10/16.
  * The witness used to evaluate a [[ScriptPubKey]] inside of Bitcoin
  * [[https://github.com/bitcoin/bitcoin/blob/57b34599b2deb179ff1bd97ffeab91ec9f904d85/src/script/script.h#L648-L660]]
  */
sealed trait ScriptWitness {

  /** The [[ScriptToken]]s that are placed on to the stack when evaluating a witness program */
  def stack : Seq[Seq[Byte]]

  override def toString = stack.map(BitcoinSUtil.encodeHex(_)).mkString
}

object ScriptWitness{
  private case class ScriptWitnessImpl(stack: Seq[Seq[Byte]]) extends ScriptWitness

  def apply(stack: Seq[Seq[Byte]]): ScriptWitness = ScriptWitnessImpl(stack)

  def fromHex(stack: Seq[Seq[String]]): ScriptWitness = ScriptWitness(stack.flatMap(_.map(BitcoinSUtil.decodeHex(_))))

  def apply(signature: ECDigitalSignature, publicKey: ECPublicKey): ScriptWitness = {
    val sigConstant = signature.bytes
    val pubKeyConstant = publicKey.bytes
    ScriptWitness(Seq(sigConstant, pubKeyConstant))
  }
}