package org.bitcoins.core.protocol.script.testprotocol

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.crypto.DoubleSha256Digest
import ujson._
import upickle.default._

case class SignatureHashTestCase(
    transaction: Transaction,
    script: ScriptPubKey,
    inputIndex: UInt32,
    hashTypeNum: Int32,
    hashType: HashType,
    hash: DoubleSha256Digest)

object SignatureHashTestCase {

  implicit val signatureHashTestCaseR: Reader[SignatureHashTestCase] =
    reader[Value].map { value =>
      val Arr: Arr = value match {
        case array: Arr => array
        case _: Value =>
          throw new RuntimeException(
            "Script signature hash test case must be in Arr format")
      }
      val elements: Vector[Value] = Arr.value.toVector
      val transaction: Transaction =
        Transaction(elements.head.str)
      val asm = ScriptParser.fromHex(elements.apply(1).str)
      val script: ScriptPubKey = ScriptPubKey(asm)
      val inputIndex: UInt32 = UInt32(elements(2).num.toInt)
      val hashTypeNum: Int32 = Int32(elements(3).num.toInt)
      val hashType: HashType = HashType(hashTypeNum)
      val hash: DoubleSha256Digest =
        DoubleSha256Digest(elements.last.str)
      SignatureHashTestCase(transaction,
                            script,
                            inputIndex,
                            hashTypeNum,
                            hashType,
                            hash)
    }
}
