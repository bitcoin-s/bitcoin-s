package org.bitcoins.core.protocol.transaction.testprotocol

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionOutPoint }
import org.bitcoins.core.script.flag.ScriptFlag

/**
 * Created by chris on 5/4/16.
 * Used to represent the test cases found inside of tx_valid.json & tx_invalid.json
 * from bitcoin core
 */
trait CoreTransactionTestCase {

  def outPoints: Seq[TransactionOutPoint] = creditingTxsInfo.map(_._1)

  def scriptPubKeys: Seq[ScriptPubKey] = creditingTxsInfo.map(_._2)

  def creditingTxsInfo: Seq[(TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])]

  def spendingTx: Transaction

  def flags: Seq[ScriptFlag]

  def raw: String

}

case class CoreTransactionTestCaseImpl(
  creditingTxsInfo: Seq[(TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])],
  spendingTx: Transaction, flags: Seq[ScriptFlag], raw: String) extends CoreTransactionTestCase