package org.bitcoins.core.protocol.transaction.testprotocol

import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagFactory}
import org.bitcoins.core.util.BitcoinSLogger
import spray.json.{DefaultJsonProtocol, JsArray, JsValue, RootJsonFormat}

/**
  * Created by chris on 5/4/16.
  */
object CoreTransactionTestCaseProtocol extends DefaultJsonProtocol with BitcoinSLogger {

  implicit object CoreTransactionTestCaseProtocol extends RootJsonFormat[Option[CoreTransactionTestCase]] {

    override def read(value : JsValue) : Option[CoreTransactionTestCase] = {
      val jsArray : JsArray = value match {
        case array : JsArray => array
        case _ : JsValue => throw new RuntimeException("Core test case must be in the format of js array")
      }
      val elements : Vector[JsValue]  = jsArray.elements

      if (elements.size < 3) None
      else {
        val creditingTxsInfo : Seq[(TransactionOutPoint, ScriptPubKey)]= elements.head match {
          case array : JsArray => parseOutPointsAndScriptPubKeys(array)
          case _ : JsValue => throw new RuntimeException("Needs to be a js array")
        }
        val spendingTx : Transaction = Transaction(elements(1).convertTo[String])
        val flags : Seq[ScriptFlag] = ScriptFlagFactory.fromList(elements(2).convertTo[String])

        Some(CoreTransactionTestCaseImpl(creditingTxsInfo.reverse,spendingTx,flags, elements.toString))
      }

    }

    override def write(testCase : Option[CoreTransactionTestCase]) : JsValue = ???
  }


  /**
    * These are in the following format
    * [[prevout hash, prevout index, prevout scriptPubKey], [input 2], ...]
 *
    * @param array
    * @return
    */
  def parseOutPointsAndScriptPubKeys(array : JsArray) : Seq[(TransactionOutPoint,ScriptPubKey)] = {
    val result = array.elements.map {
      case array : JsArray =>
        val prevoutHash = array.elements.head.convertTo[String]
        val prevoutIndex = array.elements(1).convertTo[Int]
        val outPoint = TransactionOutPoint(prevoutHash,prevoutIndex)
        val scriptTokens : Seq[ScriptToken] = ScriptParser.fromString(array.elements(2).convertTo[String])
        val scriptPubKey = ScriptPubKey.fromAsm(scriptTokens)
        (outPoint,scriptPubKey)
      case _ : JsValue => throw new RuntimeException("All tx outpoint/scriptpubkey info must be array elements")
    }

    result
  }
}
