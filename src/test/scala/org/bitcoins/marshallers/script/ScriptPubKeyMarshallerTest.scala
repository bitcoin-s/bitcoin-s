package org.bitcoins.marshallers.script

import org.bitcoins.protocol.{BitcoinAddress}
import org.bitcoins.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.script.constant.{BytesToPushOntoStackImpl, ScriptConstantImpl}
import org.bitcoins.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.stack.OP_DUP
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
    scriptPubKey.asm must be (List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl("7ecaa33ef3cd6169517e43188ad3c034db091f5e"), OP_EQUALVERIFY, OP_CHECKSIG))
    scriptPubKey.hex must be ("76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac")
  }
}
