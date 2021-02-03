package org.bitcoins.core.protocol.script.testprotocol

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.crypto.DoubleSha256Digest
import spray.json._

/** Created by tom on 7/21/16.
  */
object SignatureHashTestCaseProtocol extends DefaultJsonProtocol {

  implicit object SignatureTestCaseProtocol
      extends RootJsonFormat[SignatureHashTestCase] {

    override def read(value: JsValue): SignatureHashTestCase = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _: JsValue =>
          throw new RuntimeException(
            "Script signature hash test case must be in jsarray format")
      }
      val elements: Vector[JsValue] = jsArray.elements
      val transaction: Transaction = Transaction(
        elements.head.convertTo[String])
      val asm = ScriptParser.fromHex(elements.apply(1).convertTo[String])
      val script: ScriptPubKey = ScriptPubKey(asm)
      val inputIndex: UInt32 = UInt32(elements(2).convertTo[Int])
      val hashTypeNum: Int32 = Int32(elements(3).convertTo[Int])
      val hashType: HashType = HashType(hashTypeNum)
      val hash: DoubleSha256Digest = DoubleSha256Digest(
        elements.last.convertTo[String])
      SignatureHashTestCaseImpl(transaction,
                                script,
                                inputIndex,
                                hashTypeNum,
                                hashType,
                                hash)
    }
    override def write(testCase: SignatureHashTestCase): JsValue = ???
  }
}
