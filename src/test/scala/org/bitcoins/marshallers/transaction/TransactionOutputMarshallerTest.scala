package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.{BitcoinAddress}
import org.bitcoins.protocol.transaction.TransactionOutput
import org.bitcoins.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.script.constant.{ScriptToken, BytesToPushOntoStackImpl, ScriptConstantImpl}
import org.bitcoins.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.stack.OP_DUP
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._
import DefaultJsonProtocol._
/**
 * Created by chris on 12/27/15.
 */
class TransactionOutputMarshallerTest extends FlatSpec with MustMatchers {
  val str =
    """
      |{
      |   "value" : 0.00020000,
      |   "n" : 1,
      |   "scriptPubKey" : {
      |     "asm" : "OP_DUP OP_HASH160 321908115d8a138942f98b0b53f86c9a1848501a OP_EQUALVERIFY OP_CHECKSIG",
      |     "hex" : "76a914321908115d8a138942f98b0b53f86c9a1848501a88ac",
      |     "reqSigs" : 1,
      |     "type" : "pubkeyhash",
      |     "addresses" : [
      |       "15Ztmp5Tx2o49JRPxC6UaZgbLqaHL6SD4d"
      |     ]
      |   }
      | }
    """.stripMargin
  val json = str.parseJson
  "TranasctionOutput" must "parse an output from json" in {
    val output : TransactionOutput = TransactionOutputMarshaller.TransactionOutputFormatter.read(json)
    output.value.value must be (0.0002)
    output.scriptPubKey.asm must be (List(OP_DUP, OP_HASH160,
      BytesToPushOntoStackImpl(20),
      ScriptConstantImpl("321908115d8a138942f98b0b53f86c9a1848501a"),
      OP_EQUALVERIFY, OP_CHECKSIG))
    output.scriptPubKey.hex must be ("76a914321908115d8a138942f98b0b53f86c9a1848501a88ac")
  }

  it must "write a transaction output" in {
    val output : TransactionOutput = TransactionOutputMarshaller.TransactionOutputFormatter.read(json)
    val writtenOutput = TransactionOutputMarshaller.TransactionOutputFormatter.write(output)

    writtenOutput.asJsObject.fields("value") must be (JsNumber(0.00020000))

   /* val expectedAsm : Seq[ScriptToken] = {
      Seq()
    }*/
  }

}
