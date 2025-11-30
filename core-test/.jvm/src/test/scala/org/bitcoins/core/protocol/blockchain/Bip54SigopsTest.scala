package org.bitcoins.core.protocol.blockchain
import org.bitcoins.commons.serializers.SerializerUtil
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import play.api.libs.json.{JsResult, JsValue, Json, Reads}

import scala.io.Source
import scala.util.Using

class Bip54SigopsTest extends BitcoinSJvmTest {

  behavior of "BIP54 sigops"

  import org.bitcoins.commons.serializers.JsonSerializers.transactionReads
  implicit object TransactionOutputReads extends Reads[TransactionOutput] {
    override def reads(json: JsValue): JsResult[TransactionOutput] =
      SerializerUtil.processJsString(TransactionOutput.fromHex)(json)
  }
  case class Bip54SigOpsTestCase(
      spent_outputs: Vector[TransactionOutput],
      tx: Transaction,
      valid: Boolean,
      comment: String)
  implicit val bip54SigOpsTestCaseReader: Reads[Bip54SigOpsTestCase] =
    Json.reads[Bip54SigOpsTestCase]
  it must "pass all bip54 sigops test vectors" in {
    val fileName =
      "/sigops.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[Vector[Bip54SigOpsTestCase]].get
    testCases.foreach { testCase =>
      withClue(testCase.comment) {
        if (testCase.valid) {
          assert(
            Policy.checkBip54SigOpLimit(testCase.tx, testCase.spent_outputs))
        } else {
          assert(
            !Policy.checkBip54SigOpLimit(testCase.tx, testCase.spent_outputs))
        }
      }
    }
    succeed
  }
}
