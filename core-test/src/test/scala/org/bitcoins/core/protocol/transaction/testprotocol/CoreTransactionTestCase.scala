package org.bitcoins.core.protocol.transaction.testprotocol

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagFactory}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.DoubleSha256Digest
import ujson._
import upickle.default._

/** Created by chris on 5/4/16.
  * Used to represent the test cases found inside of tx_valid.json & tx_invalid.json
  * from bitcoin core
  */
case class CoreTransactionTestCase(
    creditingTxsInfo: Seq[
      (TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])],
    spendingTx: Transaction,
    flags: Seq[ScriptFlag],
    raw: String)

object CoreTransactionTestCase {

  implicit val coreTransactionTestCaseR: Reader[
    Option[CoreTransactionTestCase]] =
    reader[Value].map { value =>
      val arr: Arr = value match {
        case array: Arr => array
        case _: Value =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[Value] = arr.value.toVector

      if (elements.size < 3) None
      else {
        val creditingTxsInfo: Seq[(
            TransactionOutPoint,
            ScriptPubKey,
            Option[CurrencyUnit])] =
          elements.head match {
            case array: Arr => parseOutPointsScriptPubKeysAmount(array)
            case _: Value =>
              throw new RuntimeException("Needs to be a js array")
          }
        val spendingTx: Transaction = Transaction(elements(1).str)
        val flags: Seq[ScriptFlag] =
          ScriptFlagFactory.fromList(elements(2).str)

        Some(
          CoreTransactionTestCase(creditingTxsInfo.reverse,
                                  spendingTx,
                                  flags,
                                  elements.toString))
      }
    }

  private def parseOutPointsScriptPubKeysAmount(array: Arr): Seq[
    (TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])] = {
    val result = array.value.map {
      case array: Arr =>
        val prevoutHashHex =
          BytesUtil.flipEndianness(array.value.head.str)
        val prevoutHash = DoubleSha256Digest(prevoutHashHex)

        val prevoutIndex = array.value(1).num.toLong match {
          case -1 => UInt32("ffffffff")
          case index
              if index >= UInt32.min.toLong && index <= UInt32.max.toLong =>
            UInt32(index)
        }

        val amount =
          if (array.arr.size == 4)
            Some(Satoshis(array.arr(3).num.toLong))
          else None

        val outPoint = TransactionOutPoint(prevoutHash, prevoutIndex)
        val scriptTokens: Seq[ScriptToken] =
          ScriptParser.fromString(array.arr(2).str)
        val scriptPubKey = ScriptPubKey.fromAsm(scriptTokens)
        (outPoint, scriptPubKey, amount)
      case _: Value =>
        throw new RuntimeException(
          "All tx outpoint/scriptpubkey info must be array elements")
    }
    result.toVector
  }
}
