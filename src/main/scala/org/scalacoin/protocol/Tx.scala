package org.scalacoin.protocol


/**
 * Created by chris on 7/14/15.
 */


trait Tx {
  def version : Long
  def txInCount : VarInt
  def txIn : Seq[TxIn]
  def txOutCount : VarInt
  def txOut : TxOut
  def lockTime : Long
}

case class NetworkTx(serialization : String ) extends Tx {
  require(!serialization.contains(" "), "Your network transaction contains whitespace")
  override def version = java.lang.Long.parseLong(serialization.slice(0,8),16)
  override def txInCount : VarInt = NetworkVarInt("FF")
  override def txIn : Seq[TxIn] = Seq()
  override def txOutCount : VarInt = NetworkVarInt("FF")
  override def txOut : TxOut = TxOut(1,NetworkVarInt("FF"), Seq())
  override def lockTime : Long = 0

}


case class TxIn(prevousOutput : OutPoint, scriptLength : VarInt, scriptSignature : Seq[Char], sequence : Long)

case class OutPoint(hash : Seq[Char], index : Long)

case class TxOut(value : Long, pkScriptLength : VarInt, pkScript : Seq[Char])

