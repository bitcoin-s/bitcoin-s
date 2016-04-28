package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.BitcoinAddress
import org.bitcoins.protocol.transaction.TransactionOutputMeta
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/12/2016.
 */
class TransactionOutputMetaMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |    "bestblock" : "000000000000078233dfa9376fe6bc3b68e2bbda04700b5e663a1d4c8b322e62",
      |    "confirmations" : 1,
      |    "value" : 0.75829574,
      |    "scriptPubKey" : {
      |        "asm" : "OP_HASH160 5a81f53ac1ecf0312a2a4df29a734b8f2c0d8c93 OP_EQUAL",
      |        "hex" : "a9145a81f53ac1ecf0312a2a4df29a734b8f2c0d8c9387",
      |        "reqSigs" : 1,
      |        "type" : "scripthash",
      |        "addresses" : [
      |            "2N1VnVBccBVPrWgPgLaszLk2UMwEHTXTAuG"
      |        ]
      |    },
      |    "version" : 1,
      |    "coinbase" : false
      |}
    """.stripMargin

  val json = str.parseJson

  "TransactionOutputMetaMarshaller" must "parse meta information for tx output" in {
    val meta : TransactionOutputMeta = TransactionOutputMetaMarshaller.TransactionOutputMetaFormatter.read(json)
    meta.bestBlock must be ("000000000000078233dfa9376fe6bc3b68e2bbda04700b5e663a1d4c8b322e62")
    meta.confirmations must be (1)
    meta.value.value must be (0.75829574)
    meta.scriptPubKey.asm must be ("OP_HASH160 5a81f53ac1ecf0312a2a4df29a734b8f2c0d8c93 OP_EQUAL")
    meta.scriptPubKey.hex must be ("a9145a81f53ac1ecf0312a2a4df29a734b8f2c0d8c9387")
    meta.scriptPubKey.reqSigs must be (1)
    meta.scriptPubKey.addressType must be ("scripthash")
    meta.scriptPubKey.addresses must be (Seq(BitcoinAddress("2N1VnVBccBVPrWgPgLaszLk2UMwEHTXTAuG")))
    meta.version must be (1)
    meta.coinbase must be (false)
  }
}
