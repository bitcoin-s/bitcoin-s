package org.bitcoins.wallet

import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import play.api.libs.json._
import scodec.bits.ByteVector
import scala.io.Source

class ComputeContractIdTest extends BitcoinSWalletTest with EmptyFixture {

  lazy val json: JsValue = {
    val stream = {
      val classLoader = getClass().getClassLoader()
      classLoader.getResourceAsStream("contract_id_test.json")
    }
    val rawText = Source
      .fromInputStream(stream)
      .getLines()
      .mkString
    Json.parse(rawText)
  }

  case class ContractIdTestVector(
      fundTxId: DoubleSha256DigestBE,
      fundOutputIndex: Int,
      temporaryContractId: Sha256Digest,
      contractId: ByteVector)

  object ContractIdTestVector {

    implicit val reads: Reads[ContractIdTestVector] =
      Json.reads[ContractIdTestVector]
  }

  lazy val vectors: Vector[ContractIdTestVector] =
    json.validate[Vector[ContractIdTestVector]] match {
      case JsError(errors)     => fail(errors.head.toString)
      case JsSuccess(value, _) => value
    }

  "DLCUtil" must "compute contract ids correctly as in the JSON file" in { _ =>
    vectors.foreach { testVector =>
      assert(
        DLCUtil.computeContractId(
          testVector.fundTxId,
          testVector.fundOutputIndex,
          testVector.temporaryContractId) == testVector.contractId)
    }

    succeed
  }
}
