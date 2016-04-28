package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.script.{ScriptPubKeyMarshaller, ScriptParser}
import org.bitcoins.protocol.{script, BitcoinAddress}
import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.protocol.transaction.TransactionOutputMeta
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant.{BytesToPushOntoStackImpl, ScriptConstantImpl, ScriptToken}
import org.bitcoins.script.crypto.OP_HASH160
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
  val meta : TransactionOutputMeta = TransactionOutputMetaMarshaller.TransactionOutputMetaFormatter.read(json)

  "TransactionOutputMetaMarshaller" must "parse meta information for tx output" in {
    meta.bestBlock must be ("000000000000078233dfa9376fe6bc3b68e2bbda04700b5e663a1d4c8b322e62")
    meta.confirmations must be (1)
    meta.value.value must be (0.75829574)
    //println(meta.scriptPubKey.asm)
    //println(ScriptPubKey("OP_HASH160 5a81f53ac1ecf0312a2a4df29a734b8f2c0d8c93 OP_EQUAL"))
    val expectedAsm : Seq[ScriptToken] = {
      Seq(OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl("5a81f53ac1ecf0312a2a4df29a734b8f2c0d8c93"), OP_EQUAL)
    }
    meta.scriptPubKey.asm must be (expectedAsm)
    meta.scriptPubKey.hex must be ("a9145a81f53ac1ecf0312a2a4df29a734b8f2c0d8c9387")
    meta.version must be (1)
    meta.coinbase must be (false)
  }
  it must "write meta info in tx output" in {
    val writtenTxMeta = TransactionOutputMetaMarshaller.TransactionOutputMetaFormatter.write(meta)
    writtenTxMeta.asJsObject.fields("bestblock") must be (JsString("000000000000078233dfa9376fe6bc3b68e2bbda04700b5e663a1d4c8b322e62"))
    writtenTxMeta.asJsObject.fields("confirmations") must be (JsNumber(1))
    writtenTxMeta.asJsObject.fields("value") must be (JsNumber(0.75829574))
    writtenTxMeta.asJsObject.fields("version") must be (JsNumber(1))
    writtenTxMeta.asJsObject.fields("coinbase") must be (JsBoolean(false))
  }
}
