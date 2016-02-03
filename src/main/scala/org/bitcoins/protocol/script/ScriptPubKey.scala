package org.bitcoins.protocol.script

import org.bitcoins.protocol.BitcoinAddress

/**
 * Created by chris on 12/26/15.
 */
trait ScriptPubKey extends ScriptSignature {

  def reqSigs : Int
  def addressType : String
  def addresses : Seq[BitcoinAddress]

}

case class ScriptPubKeyImpl(asm : String, hex : String, reqSigs : Int,
  addressType : String, addresses : Seq[BitcoinAddress]) extends ScriptPubKey
