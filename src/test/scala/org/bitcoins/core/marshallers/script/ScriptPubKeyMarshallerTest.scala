package org.bitcoins.core.marshallers.script



import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.stack.OP_DUP
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._
import DefaultJsonProtocol._
/**
 * Created by chris on 12/27/15.
 */
class ScriptPubKeyMarshallerTest extends FlatSpec with MustMatchers {

  val str =
    """
      |{
      | "asm" : "OP_DUP OP_HASH160 7ecaa33ef3cd6169517e43188ad3c034db091f5e OP_EQUALVERIFY OP_CHECKSIG",
      | "hex" : "76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac",
      | "reqSigs" : 1,
      | "type" : "pubkeyhash",
      | "addresses" : [
      |   "1CZQtge31s59Evu716oP3teYWjcGhX8oKn"
      | ]
      |}
    """.stripMargin
  val json = str.parseJson
  "ScriptPubKeyMarshaller" must "parse a script pub key " in {

    val scriptPubKey = ScriptPubKeyMarshaller.ScriptPubKeyFormatter.read(json)
    scriptPubKey.asm must be (List(OP_DUP, OP_HASH160, BytesToPushOntoStack(20), ScriptConstant("7ecaa33ef3cd6169517e43188ad3c034db091f5e"), OP_EQUALVERIFY, OP_CHECKSIG))
    scriptPubKey.hex must be ("76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac")
  }
}
