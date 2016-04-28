package org.bitcoins.marshallers.script

import org.bitcoins.protocol.script.ScriptSignature
import org.bitcoins.script.arithmetic.{OP_1ADD, OP_ADD}
import org.bitcoins.script.bitwise.OP_EQUAL
import org.bitcoins.script.constant._
import org.bitcoins.script.control.{OP_ENDIF, OP_IF}
import org.bitcoins.script.crypto.{OP_CHECKMULTISIG, OP_HASH160}
import org.bitcoins.script.reserved.{OP_NOP10, OP_NOP}
import org.bitcoins.script.stack.OP_PICK
import org.bitcoins.util.{BitcoinSUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/7/16.
 */
class ScriptParserTest extends FlatSpec with MustMatchers with ScriptParser with BitcoinSUtil {



  "ScriptParser" must "parse 0x00 to a OP_0" in {
    fromBytes(List(0.toByte)) must be (List(OP_0))
  }

  it must "parse the number 0 as an OP_0" in {
    fromString("0") must be (List(OP_0))
  }

  it must "parse a number larger than an integer into a ScriptNumberImpl" in {
    fromString("2147483648") must be (List(ScriptNumber(2147483648L)))
  }

  it must "parse a pay-to-pubkey-hash output script" in {
    val parsedOutput = ScriptParser.fromString(TestUtil.p2pkhOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash output script" in {
    val parsedOutput = fromString(TestUtil.p2shOutputScriptNotParsedAsm)
    parsedOutput must be (TestUtil.p2shOutputScriptAsm)
  }

  it must "parse a p2pkh output script from a byte array to script tokens" in {
    val bytes : List[Byte] = decodeHex(TestUtil.p2pkhOutputScript)
    fromBytes(bytes) must be (TestUtil.p2pkhOutputScriptAsm)
  }

   it must "parse a p2pkh input script from a byte array to script tokens" in {
     val bytes = decodeHex(TestUtil.p2pkhInputScript)
     fromBytes(bytes) must be (TestUtil.p2pkhInputScriptAsm)
   }

   it must "parse a p2sh input script from a byte array into script tokens" in {
     val bytes = decodeHex(TestUtil.rawP2shInputScript)
     fromBytes(bytes) must be (TestUtil.p2shInputScriptAsm)
   }

   it must "parse a p2sh outputscript from a byte array into script tokens" in {
     val bytes = decodeHex(TestUtil.p2shOutputScript)
     fromBytes(bytes) must be (TestUtil.p2shOutputScriptAsm)
   }

   it must "parse a script constant from 'Az' EQUAL" in {
     val str = "'Az' EQUAL"
     fromString(str) must equal (List(BytesToPushOntoStack(2).get, ScriptConstant("417a"), OP_EQUAL))
   }

   it must "parse a script number that has a leading zero" in {
     val str = "0x02 0x0100"
     fromString(str) must equal (List(BytesToPushOntoStack(2).get, ScriptConstant("0100")))
   }


   it must "parse an OP_PICK" in {
     val str = "PICK"
     fromString(str) must equal (List(OP_PICK))
   }

   it must "parse an OP_NOP" in {
     val str = "NOP"
     fromString(str) must equal (List(OP_NOP))
   }

   it must "parse a script that has a decimal and a hexadecimal number in it " in  {
     val str = "32767 0x02 0xff7f EQUAL"
     fromString(str) must equal (List(BytesToPushOntoStack(2).get, ScriptConstant("ff7f"),
       BytesToPushOntoStackImpl(2), ScriptConstant("ff7f"), OP_EQUAL))
   }
   it must "parse an OP_1" in {
     val str = "0x51"
     fromString(str) must equal (List(OP_1))
   }

   it must "parse an extremely long hex constant" in {
     val str = "0x4b 0x417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a " +
     "'Azzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' EQUAL"

     fromString(str) must equal (List(BytesToPushOntoStack(75).get,
       ScriptConstant("417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"),
       BytesToPushOntoStack(75).get,
       ScriptConstant("417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"), OP_EQUAL))
   }

  it must "parse an OP_IF OP_ENDIF block" in {
    val str = "1 0x01 0x80 IF 0 ENDIF"
    fromString(str) must be (List(OP_1, BytesToPushOntoStackImpl(1), ScriptConstant("80"), OP_IF, OP_0, OP_ENDIF))
  }


  it must "parse an OP_PUSHDATA1 correctly" in {
    val str = "'abcdefghijklmnopqrstuvwxyz' HASH160 0x4c 0x14 0xc286a1af0947f58d1ad787385b1c2c4a976f9e71 EQUAL"
    val expectedScript = List(BytesToPushOntoStack(26).get, ScriptConstantImpl("6162636465666768696a6b6c6d6e6f707172737475767778797a"),OP_HASH160,
      OP_PUSHDATA1, ScriptNumber(20), ScriptConstantImpl("c286a1af0947f58d1ad787385b1c2c4a976f9e71"), OP_EQUAL)
    fromString(str) must be (expectedScript)
  }

  it must "parse a OP_PUSHDATA1 correct from a scriptSig" in {
    //https://tbtc.blockr.io/api/v1/tx/raw/5d254a872c9197c683ea9111fb5c0e2e0f49280a89961c45b9fea76834d335fe
    val str = "4cf1" +
      "55210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae"
    fromString(str) must be (List(OP_PUSHDATA1, ScriptNumber(241),
      ScriptConstant("55210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae"))
    )
  }


  it must "parse bytes from a string" in {
    val str = "0xFF00"
    parseBytesFromString(str) must be (List(ScriptNumber(255)))
  }

  it must "parse an OP_PUSHDATA2 correctly" in {
    val str = "0x4d 0xFF00 " +
      "0x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 " +
      "0x4c 0xFF 0x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 " +
      "EQUAL"
    val expectedScript  =  List(OP_PUSHDATA2, ScriptNumber(255),
      ScriptConstant("111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "1111111"), OP_PUSHDATA1, ScriptNumber(255),
      ScriptConstant("111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
        "1111111"), OP_EQUAL)

    fromString(str) must be (expectedScript)
  }


  it must "parse a hex string to a list of script tokens, and then back again" in {
    //from this question
    //https://bitcoin.stackexchange.com/questions/37125/how-are-sighash-flags-encoded-into-a-signature
    val hex = "304402206e3729f021476102a06ea453cea0a26cb9c096cca641efc4229c1111ed3a96fd022037dce1456a93f53d3e868c789b1b750a48a4c1110cd5b7049779b5f4f3c8b6200103ff1104b46b2141df1948dd0df2223720a3a471ec57404cace47063843a699a0f"

    val scriptTokens : Seq[ScriptToken] = fromHex(hex)
    scriptTokens.map(_.hex).mkString must be (hex)
  }


  it must "parse a p2pkh scriptSig properly" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawScriptSig = "4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b"

    val expectedAsm = List(BytesToPushOntoStackImpl(71),
      ScriptConstantImpl("30440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf" +
        "225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff3572230501"),
      BytesToPushOntoStackImpl(33),
      ScriptConstantImpl("03dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b"))

    val scriptTokens : List[ScriptToken] = ScriptParser.fromHex(rawScriptSig)

    scriptTokens must be (expectedAsm)
  }

  it must "parse 1ADD to an OP_1ADD" in {
    ScriptParser.fromString("1ADD") must be (Seq(OP_1ADD))
  }

  it must "parse a OP_PUSHDATA operation that pushes zero bytes correctly" in {
    val str = "0x4c 0x00"
    ScriptParser.fromString(str) must be (List(OP_PUSHDATA1, ScriptNumber.zero))

    val str1 = "0x4d 0x00"
    ScriptParser.fromString(str1) must be (List(OP_PUSHDATA2, ScriptNumber.zero))

    val str2 = "0x4e 0x00"
    ScriptParser.fromString(str2) must be (List(OP_PUSHDATA4, ScriptNumber.zero))
  }

  it must "parse a large string constant found inside of script_valid.json" in {
    val str = "'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'"
    val parsed = fromString(str)
     parsed must be (List(OP_PUSHDATA2, ScriptNumber(520), ScriptConstant(
      "62626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262"
    )))
  }

}
