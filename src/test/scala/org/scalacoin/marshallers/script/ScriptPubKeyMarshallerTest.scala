package org.scalacoin.marshallers.script

import org.scalacoin.protocol.{P2PKH, BitcoinAddress}
import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
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
    scriptPubKey.asm must be (List(OP_DUP, OP_HASH160, ScriptConstantImpl("7ecaa33ef3cd6169517e43188ad3c034db091f5e"), OP_EQUALVERIFY, OP_CHECKSIG))
    scriptPubKey.hex must be ("76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac")
    scriptPubKey.reqSigs must be (Some(1))
    scriptPubKey.addressType must be (P2PKH)
    scriptPubKey.addresses must be (Seq(BitcoinAddress("1CZQtge31s59Evu716oP3teYWjcGhX8oKn")))
  }
}
