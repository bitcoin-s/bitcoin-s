package org.scalacoin.protocol.transaction

import org.scalacoin.protocol.{NetworkVarInt, VarInt}

/**
 * Created by chris on 7/14/15.
 */


trait Transaction {
  def version : Long
  def txInCount : VarInt
  def txIn : Seq[TxIn]
  def txOutCount : VarInt
  def txOut : Seq[TxOut]
  def lockTime : Long
}

case class NetworkTx(serialization : String ) extends Transaction {
  require(!serialization.contains(" "), "Your network transaction contains whitespace")
  override def version = java.lang.Long.parseLong(serialization.slice(0,8),16)
  override def txInCount : VarInt = NetworkVarInt("FF")
  override def txIn : Seq[TxIn] = Seq()
  override def txOutCount : VarInt = NetworkVarInt("FF")
  override def txOut : TxOut = (1,NetworkVarInt("FF"), Seq())
  override def lockTime : Long = 0

}









