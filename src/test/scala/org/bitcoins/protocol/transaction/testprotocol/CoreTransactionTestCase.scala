package org.bitcoins.protocol.transaction.testprotocol

import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.script.flag.ScriptFlag

/**
  * Created by chris on 5/4/16.
  * Used to represent the test cases found inside of tx_valid.json & tx_invalid.json
  * from bitcoin core
  */
trait CoreTransactionTestCase {


  def outPoints : Seq[TransactionOutPoint] = creditingTxsInfo.map(_._1)

  def scriptPubKeys : Seq[ScriptPubKey] = creditingTxsInfo.map(_._2)

  def creditingTxsInfo : Seq[(TransactionOutPoint, ScriptPubKey)]

  def spendingTx : Transaction

  def flags : Seq[ScriptFlag]

  def raw : String
}


case class CoreTransactionTestCaseImpl(creditingTxsInfo : Seq[(TransactionOutPoint,ScriptPubKey)],
  spendingTx : Transaction, flags  : Seq[ScriptFlag], raw : String) extends CoreTransactionTestCase