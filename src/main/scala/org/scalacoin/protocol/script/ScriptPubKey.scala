package org.scalacoin.protocol.script

import org.scalacoin.protocol._
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptToken}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP

/**
 * Created by chris on 12/26/15.
 */
trait ScriptPubKey extends ScriptSignature {


  def reqSigs : Option[Int] = {
    addressType match {
      case P2PKH => Some(1)
      //TODO: Figure out how many signatures are actually required by the scriptPubKey
      case P2SH => None
      case NonStandard => None
    }
  }
  def addressType : AddressType = {
    asm match {
      case List(OP_DUP, OP_HASH160, ScriptConstantImpl(pubKeyHash), OP_EQUALVERIFY, OP_CHECKSIG) => P2PKH
      case List(OP_HASH160, ScriptConstantImpl(scriptHash), OP_EQUAL) => P2SH
      case _ => NonStandard
    }
  }

  //the addresses that the bitcoins correlated to the output
  def addresses : Seq[BitcoinAddress]

}

case class ScriptPubKeyImpl(asm : List[ScriptToken], hex : String, addresses : Seq[BitcoinAddress]) extends ScriptPubKey
