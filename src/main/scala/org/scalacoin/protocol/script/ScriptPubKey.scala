package org.scalacoin.protocol.script

import org.scalacoin.protocol.BitcoinAddress
import org.scalacoin.script.constant.ScriptToken

/**
 * Created by chris on 12/26/15.
 */
trait ScriptPubKey extends ScriptSignature {


  def reqSigs : Int
  def addressType : String
  def addresses : Seq[BitcoinAddress]

}

case class ScriptPubKeyImpl(asm : List[ScriptToken], hex : String, reqSigs : Int,
  addressType : String, addresses : Seq[BitcoinAddress]) extends ScriptPubKey
