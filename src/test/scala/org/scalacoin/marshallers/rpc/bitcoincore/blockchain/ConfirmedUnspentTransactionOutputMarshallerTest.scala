package org.scalacoin.marshallers.rpc.bitcoincore.blockchain

import org.scalacoin.marshallers.rpc.bitcoincore.blockchain.ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter
import org.scalacoin.protocol.rpc.bitcoincore.blockchain.ConfirmedUnspentTransactionOutput
import org.scalatest.{FlatSpec, MustMatchers}
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
  val utxos : ConfirmedUnspentTransactionOutput = ConfirmedUnspentTransactionOutputFormatter.read(json)

  "ConfirmedUnspentTransactionOutput" must "parse uxtos" in {
    utxos.height must be (632518)
    utxos.bestBlock must be ("000000000000116903d2e6b84e327e5df5ecc9b9fb1bffe6d5eae67781543bdc")
    utxos.transactions must be (2976618)
    utxos.txOuts must be (13198249)
    utxos.bytesSerialized must be (433488802)
    utxos.hashSerialized must be ("f6f810ea6f20ab8f82141bbb418ebdb82074afc51bf3a9be03ff6ce75175e91c")
    utxos.totalAmount must be (18390134.73359557)
  }

  it must "write utxos" in {
    val writtenUtxos = ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.write(utxos)
    writtenUtxos.asJsObject.fields("height") must be (JsNumber(632518))
    writtenUtxos.asJsObject.fields("bestblock") must be (JsString("000000000000116903d2e6b84e327e5df5ecc9b9fb1bffe6d5eae67781543bdc"))
    writtenUtxos.asJsObject.fields("transactions") must be (JsNumber(2976618))
    writtenUtxos.asJsObject.fields("txouts") must be (JsNumber(13198249))
    writtenUtxos.asJsObject.fields("bytes_serialized") must be (JsNumber(433488802))
    writtenUtxos.asJsObject.fields("hash_serialized") must be (JsString("f6f810ea6f20ab8f82141bbb418ebdb82074afc51bf3a9be03ff6ce75175e91c"))
    writtenUtxos.asJsObject.fields("total_amount") must be (JsNumber(18390134.73359557))

  }
}
