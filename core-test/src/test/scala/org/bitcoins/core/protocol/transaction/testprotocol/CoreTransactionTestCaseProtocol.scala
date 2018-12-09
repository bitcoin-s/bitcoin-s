package org.bitcoins.core.protocol.transaction.testprotocol

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagFactory}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}
import spray.json.{DefaultJsonProtocol, JsArray, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/4/16.
  */
object CoreTransactionTestCaseProtocol extends DefaultJsonProtocol {
  private def logger = BitcoinSLogger.logger

  implicit object CoreTransactionTestCaseProtocol
      extends RootJsonFormat[Option[CoreTransactionTestCase]] {

    override def read(value: JsValue): Option[CoreTransactionTestCase] = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _: JsValue =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[JsValue] = jsArray.elements

      if (elements.size < 3) None
      else {
        val creditingTxsInfo: Seq[
          (TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])] =
          elements.head match {
            case array: JsArray => parseOutPointsScriptPubKeysAmount(array)
            case _: JsValue =>
              throw new RuntimeException("Needs to be a js array")
          }
        val spendingTx: Transaction = Transaction(elements(1).convertTo[String])
        val flags: Seq[ScriptFlag] =
          ScriptFlagFactory.fromList(elements(2).convertTo[String])

        Some(
          CoreTransactionTestCaseImpl(creditingTxsInfo.reverse,
                                      spendingTx,
                                      flags,
                                      elements.toString))
      }

    }

    override def write(testCase: Option[CoreTransactionTestCase]): JsValue = ???
  }

  /**
    * These are in the following format
    * [[prevout hash, prevout index, prevout scriptPubKey, amount], [input 2], ...]
    */
  def parseOutPointsScriptPubKeysAmount(array: JsArray): Seq[
    (TransactionOutPoint, ScriptPubKey, Option[CurrencyUnit])] = {
    val result = array.elements.map {
      case array: JsArray =>
        val prevoutHashHex =
          BitcoinSUtil.flipEndianness(array.elements.head.convertTo[String])
        val prevoutHash = DoubleSha256Digest(prevoutHashHex)

        val prevoutIndex = array.elements(1).convertTo[Long] match {
          case -1 => UInt32("ffffffff")
          case index
              if index >= UInt32.min.toLong && index <= UInt32.max.toLong =>
            UInt32(index)
        }

        val amount =
          if (array.elements.size == 4)
            Some(Satoshis(Int64(array.elements(3).convertTo[Long])))
          else None

        //val prevoutIndex = UInt32(array.elements(1).convertTo[Int])
        val outPoint = TransactionOutPoint(prevoutHash, prevoutIndex)
        val scriptTokens: Seq[ScriptToken] =
          ScriptParser.fromString(array.elements(2).convertTo[String])
        val scriptPubKey = ScriptPubKey.fromAsm(scriptTokens)
        (outPoint, scriptPubKey, amount)
      case _: JsValue =>
        throw new RuntimeException(
          "All tx outpoint/scriptpubkey info must be array elements")
    }
    result
  }
}
