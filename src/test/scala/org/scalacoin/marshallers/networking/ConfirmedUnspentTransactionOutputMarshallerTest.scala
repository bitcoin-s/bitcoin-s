package org.scalacoin.marshallers.networking

import org.scalacoin.marshallers.networking.ConfirmedUnspentTransactionOutputMarshaller._
import org.scalacoin.protocol.networking.ConfirmedUnspentTransactionOutput
import org.scalatest.{MustMatchers, FlatSpec}
import org.scalacoin.marshallers.networking.{ConfirmedUnspentTransactionOutputMarshaller}
import spray.json._

/**
 * Created by Tom on 1/11/2016.
 */
class ConfirmedUnspentTransactionOutputMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "height" : 632518,
      |    "bestblock" : "000000000000116903d2e6b84e327e5df5ecc9b9fb1bffe6d5eae67781543bdc",
      |    "transactions" : 2976618,
      |    "txouts" : 13198249,
      |    "bytes_serialized" : 433488802,
      |    "hash_serialized" : "f6f810ea6f20ab8f82141bbb418ebdb82074afc51bf3a9be03ff6ce75175e91c",
      |    "total_amount" : 18390134.73359557
      |}
    """.stripMargin

  val json = str.parseJson

  "ConfirmedUnspentTransactionOutput" must "parse uxtos" in {
    val utxos : ConfirmedUnspentTransactionOutput = ConfirmedUnspentTransactionOutputFormatter.read(json)
    utxos.height must be (632518)
    utxos.bestBlock must be ("000000000000116903d2e6b84e327e5df5ecc9b9fb1bffe6d5eae67781543bdc")
    utxos.transactions must be (2976618)
    utxos.txOuts must be (13198249)
    utxos.bytesSerialized must be (433488802)
    utxos.hashSerialized must be ("f6f810ea6f20ab8f82141bbb418ebdb82074afc51bf3a9be03ff6ce75175e91c")
    utxos.totalAmount must be (18390134.73359557)
  }
}
