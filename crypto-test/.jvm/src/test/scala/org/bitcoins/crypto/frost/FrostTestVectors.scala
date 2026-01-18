package org.bitcoins.crypto.frost
import org.bitcoins.crypto.BitcoinSCryptoTest
import play.api.libs.json.*
import play.api.libs.functional.syntax.*

import scala.io.Source
import scala.util.Using

class FrostTestVectors extends BitcoinSCryptoTest {
  behavior of "FrostTestVectors"

  case class NonceGenTestVectors(test_cases: Vector[NonceGenTestVector])
  case class NonceGenTestVector(
      rand: String,
      secshare: Option[String],
      pubshare: Option[String],
      threshold_pubkey: Option[String],
      msg: Option[String],
      extra_in: Option[String],
      expected_secnonce: String,
      expected_pubnonce: String,
      comment: String
  )
  implicit val nonceGenTestVectorReads: Reads[NonceGenTestVector] = (
    (__ \ "rand_").read[String] and
      (__ \ "secshare").readNullable[String] and
      (__ \ "pubshare").readNullable[String] and
      (__ \ "threshold_pubkey").readNullable[String] and
      (__ \ "msg").readNullable[String] and
      (__ \ "extra_in").readNullable[String] and
      (__ \ "expected_secnonce").read[String] and
      (__ \ "expected_pubnonce").read[String] and
      (__ \ "comment").read[String]
  )(NonceGenTestVector.apply _)
  implicit val nonceGenTestVectorsReads: Reads[NonceGenTestVectors] =
    Json.reads[NonceGenTestVectors]
  it should "pass nonce_gen_vectors.json" in {
    val fileName = "/nonce_gen_vectors.json"
    val lines = Using(Source.fromURL(getClass.getResource(fileName))) {
      source => source.mkString
    }.get
    val json = Json.parse(lines)
    val testCases = json.validate[NonceGenTestVectors]
    println(
      s"Running ${testCases.map(_.test_cases.length)} test cases from $fileName")
  }
}
