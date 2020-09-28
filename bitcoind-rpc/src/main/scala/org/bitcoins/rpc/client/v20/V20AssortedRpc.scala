package org.bitcoins.rpc.client.v20

import java.nio.file.Path

import org.bitcoins.commons.jsonmodels.bitcoind.DumpTxOutSetResult
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json.{JsNumber, JsString, Json}

import scala.concurrent.Future

/**
  * Assorted Rpc calls for Bitcoin V20
  */
trait V20AssortedRpc { self: Client =>

  def dumpTxOutSet(path: Path): Future[DumpTxOutSetResult] = {
    bitcoindCall[DumpTxOutSetResult]("dumptxoutset",
                                     List(Json.toJson(path.toString)))
  }

  def generateToDescriptor(
      numBlocks: Int,
      descriptor: String,
      maxTries: Long = 1000000): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "generatetodescriptor",
      List(JsNumber(numBlocks), JsString(descriptor), JsNumber(maxTries)))
  }
}
