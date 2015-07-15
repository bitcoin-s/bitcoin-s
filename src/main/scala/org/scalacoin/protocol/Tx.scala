package org.scalacoin.protocol


/**
 * Created by chris on 7/14/15.
 */
case class Tx(version : Long, txInCount : VarInt, txIn : Seq[TxIn], txOutCount : VarInt, txOut : TxOut, lockTime : Long)


case class TxIn(prevousOutput : OutPoint, scriptLength : VarInt, scriptSignature : Seq[Char], sequence : Long)

case class OutPoint(hash : Seq[Char], index : Long)

case class TxOut(value : Long, pkScriptLength : VarInt, pkScript : Seq[Char])

