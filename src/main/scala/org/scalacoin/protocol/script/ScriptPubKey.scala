package org.scalacoin.protocol.script

import org.scalacoin.protocol.BitcoinAddress

/**
 * Created by chris on 12/26/15.
 */
trait ScriptPubKey extends ScriptSignature {

  def reqSigs : Int
  def addressType : String
  def addresses : Seq[BitcoinAddress]

}

case class ScriptPubKeyImpl(reqSigs : Int, addressType : String, addresses : Seq[BitcoinAddress])
