package org.bitcoins.core.serializers.script

import org.bitcoins.core.script.arithmetic.OP_1ADD
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{OP_ELSE, OP_ENDIF, OP_IF, OP_NOTIF}
import org.bitcoins.core.script.crypto.{
  OP_CHECKMULTISIG,
  OP_CHECKSIG,
  OP_HASH160
}
import org.bitcoins.core.script.locktime.OP_CHECKLOCKTIMEVERIFY
import org.bitcoins.core.script.reserved.OP_NOP
import org.bitcoins.core.script.splice.OP_SIZE
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP, OP_PICK, OP_SWAP}
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}
import scodec.bits.ByteVector

/**
  * Created by chris on 1/7/16.
  */
class ScriptParserTest extends BitcoinSUnitTest {

  "ScriptParser" must "parse 0x00 to a OP_0" in {
    ScriptParser.fromBytes(ByteVector(List(0.toByte))) must be(List(OP_0))
  }

  it must "parse the number 0 as an OP_0" in {
    ScriptParser.fromString("0") must be(List(OP_0))
  }

  it must "parse a number larger than an integer into a ScriptNumberImpl" in {
    ScriptParser.fromString("2147483648") must be(
      List(BytesToPushOntoStack(5), ScriptNumber(2147483648L)))
  }

  it must "parse a decimal number and correctly add its corresponding BytesToPushOntoStack" in {
    ScriptParser.fromString("127") must be(
      List(BytesToPushOntoStack(1), ScriptNumber(127)))
  }

  it must "parse a decimal number that is pushed onto the stack" in {
    val str = "NOP 0x01 1"
    ScriptParser.fromString(str) must be(
      List(OP_NOP, BytesToPushOntoStack(1), ScriptConstant("51")))
  }

  it must "parse a pay-to-pubkey-hash output script" in {
    val parsedOutput =
      ScriptParser.fromString(TestUtil.p2pkhOutputScriptNotParsedAsm)
    parsedOutput must be(TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a pay-to-script-hash output script" in {
    val parsedOutput =
      ScriptParser.fromString(TestUtil.p2shOutputScriptNotParsedAsm)
    parsedOutput must be(TestUtil.p2shOutputScriptAsm)
  }

  it must "parse a p2pkh output script from a byte array to script tokens" in {
    val bytes: ByteVector =
      BitcoinSUtil.decodeHex(TestUtil.p2pkhOutputScript).tail
    ScriptParser.fromBytes(bytes) must be(TestUtil.p2pkhOutputScriptAsm)
  }

  it must "parse a p2pkh input script from a byte array to script tokens" in {
    val bytes = BitcoinSUtil.decodeHex(TestUtil.p2pkhInputScript).tail
    ScriptParser.fromBytes(bytes) must be(TestUtil.p2pkhInputScriptAsm)
  }

  it must "parse a p2sh input script from a byte array into script tokens" in {
    val bytes = BitcoinSUtil.decodeHex(TestUtil.rawP2shInputScript).tail
    ScriptParser.fromBytes(bytes) must be(TestUtil.p2shInputScriptAsm)
  }

  it must "parse a p2sh outputscript from a byte array into script tokens" in {
    val bytes = BitcoinSUtil.decodeHex(TestUtil.p2shOutputScript).tail
    ScriptParser.fromBytes(bytes) must be(TestUtil.p2shOutputScriptAsm)
  }

  it must "parse a script constant from 'Az' EQUAL" in {
    val str = "'Az' EQUAL"
    ScriptParser.fromString(str) must equal(
      List(BytesToPushOntoStack(2), ScriptConstant("417a"), OP_EQUAL))
  }

  it must "parse a script number that has a leading zero" in {
    val str = "0x02 0x0100"
    ScriptParser.fromString(str) must equal(
      List(BytesToPushOntoStack(2), ScriptConstant("0100")))
  }

  it must "parse an OP_PICK" in {
    val str = "PICK"
    ScriptParser.fromString(str) must equal(List(OP_PICK))
  }

  it must "parse an OP_NOP" in {
    val str = "NOP"
    ScriptParser.fromString(str) must equal(List(OP_NOP))
  }

  it must "parse a script that has a decimal and a hexadecimal number in it " in {
    val str = "32767 0x02 0xff7f EQUAL"
    ScriptParser.fromString(str) must equal(
      List(BytesToPushOntoStack(2),
           ScriptConstant("ff7f"),
           BytesToPushOntoStack(2),
           ScriptConstant("ff7f"),
           OP_EQUAL))
  }
  it must "parse an OP_1" in {
    val str = "0x51"
    ScriptParser.fromString(str) must equal(List(OP_1))
  }

  it must "parse an extremely long hex constant" in {
    val str = "0x4b 0x417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a " +
      "'Azzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' EQUAL"

    ScriptParser.fromString(str) must equal(
      List(
        BytesToPushOntoStack(75),
        ScriptConstant(
          "417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"),
        BytesToPushOntoStack(75),
        ScriptConstant(
          "417a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"),
        OP_EQUAL
      ))
  }

  it must "parse an OP_IF OP_ENDIF block" in {
    val str = "1 0x01 0x80 IF 0 ENDIF"
    ScriptParser.fromString(str) must be(
      List(OP_1,
           BytesToPushOntoStack(1),
           ScriptConstant("80"),
           OP_IF,
           OP_0,
           OP_ENDIF))
  }

  it must "parse an OP_PUSHDATA1 correctly" in {
    val str =
      "'abcdefghijklmnopqrstuvwxyz' HASH160 0x4c 0x14 0xc286a1af0947f58d1ad787385b1c2c4a976f9e71 EQUAL"
    val expectedScript = List(
      BytesToPushOntoStack(26),
      ScriptConstant("6162636465666768696a6b6c6d6e6f707172737475767778797a"),
      OP_HASH160,
      OP_PUSHDATA1,
      ScriptConstant("14"),
      ScriptConstant("c286a1af0947f58d1ad787385b1c2c4a976f9e71"),
      OP_EQUAL
    )
    ScriptParser.fromString(str) must be(expectedScript)
  }

  it must "parse a OP_PUSHDATA1 correct from a scriptSig" in {
    //https://tbtc.blockr.io/api/v1/tx/raw/5d254a872c9197c683ea9111fb5c0e2e0f49280a89961c45b9fea76834d335fe
    val str = "4cf1" +
      "55210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae"
    ScriptParser.fromString(str) must be(
      List(
        OP_PUSHDATA1,
        ScriptConstant("f1"),
        ScriptConstant(
          "55210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae")
      ))
  }

  it must "parse bytes from a string" in {
    val str = "0xFF00"
    ScriptParser.parseBytesFromString(str) must be(List(ScriptNumber(255)))
  }

  it must "parse an OP_PUSHDATA2 correctly" in {
    val str = "0x4d 0xFF00 " +
      "0x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 " +
      "0x4c 0xFF 0x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 " +
      "EQUAL"
    val expectedScript = List(
      OP_PUSHDATA2,
      ScriptConstant("ff00"),
      ScriptConstant(
        "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "1111111"),
      OP_PUSHDATA1,
      ScriptConstant("ff"),
      ScriptConstant(
        "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" +
          "1111111"),
      OP_EQUAL
    )

    ScriptParser.fromString(str) must be(expectedScript)
  }

  it must "parse a hex string to a list of script tokens, and then back again" in {
    //from this question
    //https://bitcoin.stackexchange.com/questions/37125/how-are-sighash-flags-encoded-into-a-signature
    val hex =
      "304402206e3729f021476102a06ea453cea0a26cb9c096cca641efc4229c1111ed3a96fd022037dce1456a93f53d3e868c789b1b750a48a4c1110cd5b7049779b5f4f3c8b6200103ff1104b46b2141df1948dd0df2223720a3a471ec57404cace47063843a699a0f"

    val scriptTokens: Seq[ScriptToken] = ScriptParser.fromHex(hex)
    scriptTokens.map(_.hex).mkString must be(hex)
  }

  it must "parse a p2pkh scriptSig properly" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawScriptSig =
      "4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b"

    val expectedAsm = Vector(
      BytesToPushOntoStack(71),
      ScriptConstant(
        "30440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf" +
          "225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff3572230501"),
      BytesToPushOntoStack(33),
      ScriptConstant(
        "03dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b")
    )

    val scriptTokens: Vector[ScriptToken] = ScriptParser.fromHex(rawScriptSig)

    scriptTokens must be(expectedAsm)
  }

  it must "parse 1 as an OP_1" in {
    ScriptParser.fromString("1") must be(Seq(OP_1))
  }
  it must "parse 1ADD to an OP_1ADD" in {
    ScriptParser.fromString("1ADD") must be(Seq(OP_1ADD))
  }

  it must "parse a OP_PUSHDATA operation that pushes zero bytes correctly" in {
    val str = "0x4c 0x00"
    ScriptParser.fromString(str) must be(
      List(OP_PUSHDATA1, ScriptConstant("00")))

    val str1 = "0x4d 0x00"
    ScriptParser.fromString(str1) must be(
      List(OP_PUSHDATA2, ScriptConstant("00")))

    val str2 = "0x4e 0x00"
    ScriptParser.fromString(str2) must be(
      List(OP_PUSHDATA4, ScriptConstant("00")))
  }

  it must "parse a large string constant found inside of script_valid.json" in {
    val str =
      "'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'"
    val parsed = ScriptParser.fromString(str)
    parsed must be(
      List(
        OP_PUSHDATA2,
        ScriptConstant("0802"),
        ScriptConstant(
          "62626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262")
      ))
  }

  it must "parse a offered htlc" in {
    //https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#offered-htlc-outputs
    val witScriptHex =
      "76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6868"
    val asm = ScriptParser.fromHex(witScriptHex)

    /**
      * # To remote node with revocation key
      * OP_DUP OP_HASH160 <RIPEMD160(SHA256(revocationpubkey))> OP_EQUAL
      * OP_IF
      * OP_CHECKSIG
      * OP_ELSE
      * <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
      * OP_NOTIF
      * # To local node via HTLC-timeout transaction (timelocked).
      * OP_DROP 2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
      * OP_ELSE
      * # To remote node with preimage.
      * OP_HASH160 <RIPEMD160(payment_hash)> OP_EQUALVERIFY
      * OP_CHECKSIG
      * OP_ENDIF
      * OP_ENDIF
      */
    val expectedAsm = List(
      OP_DUP,
      OP_HASH160,
      BytesToPushOntoStack(20),
      ScriptConstant(
        ByteVector.fromValidHex("14011f7254d96b819c76986c277d115efce6f7b5")),
      OP_EQUAL,
      OP_IF,
      OP_CHECKSIG,
      OP_ELSE,
      BytesToPushOntoStack(33),
      ScriptConstant(ByteVector.fromValidHex(
        "0394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b")),
      OP_SWAP,
      OP_SIZE,
      BytesToPushOntoStack(1),
      ScriptConstant.fromHex("20"),
      OP_EQUAL,
      OP_NOTIF,
      OP_DROP,
      OP_2,
      OP_SWAP,
      BytesToPushOntoStack(33),
      ScriptConstant(ByteVector.fromValidHex(
        "030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e7")),
      OP_2,
      OP_CHECKMULTISIG,
      OP_ELSE,
      OP_HASH160,
      BytesToPushOntoStack(20),
      ScriptConstant(
        ByteVector.fromValidHex("b43e1b38138a41b37f7cd9a1d274bc63e3a9b5d1")),
      OP_EQUALVERIFY,
      OP_CHECKSIG,
      OP_ENDIF,
      OP_ENDIF
    )

    asm must be(expectedAsm)

  }

  it must "parse a received htlc" in {
    //https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#received-htlc-outputs
    /**
      * # To remote node with revocation key
      * OP_DUP OP_HASH160 <RIPEMD160(SHA256(revocationpubkey))> OP_EQUAL
      * OP_IF
      * OP_CHECKSIG
      * OP_ELSE
      * <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
      * OP_IF
      * # To local node via HTLC-success transaction.
      * OP_HASH160 <RIPEMD160(payment_hash)> OP_EQUALVERIFY
      * 2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
      * OP_ELSE
      * # To remote node after timeout.
      * OP_DROP <cltv_expiry> OP_CHECKLOCKTIMEVERIFY OP_DROP
      * OP_CHECKSIG
      * OP_ENDIF
      * OP_ENDIF
      */
    val witScriptHex =
      "76a91414011f7254d96b819c76986c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b7c8201208763a914b8bcb07f6344b42ab04250c86a6e8b75d3fdbbc688527c21030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae677502f401b175ac6868"

    val asm = ScriptParser.fromHex(witScriptHex)

    val expectedAsm = {
      List(
        OP_DUP,
        OP_HASH160,
        BytesToPushOntoStack(20),
        ScriptConstant(
          ByteVector.fromValidHex("14011f7254d96b819c76986c277d115efce6f7b5")),
        OP_EQUAL,
        OP_IF,
        OP_CHECKSIG,
        OP_ELSE,
        BytesToPushOntoStack(33),
        ScriptConstant(ByteVector.fromValidHex(
          "0394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b")),
        OP_SWAP,
        OP_SIZE,
        BytesToPushOntoStack(1),
        ScriptConstant(ByteVector.fromValidHex("20")),
        OP_EQUAL,
        OP_IF,
        OP_HASH160,
        BytesToPushOntoStack(20),
        ScriptConstant(
          ByteVector.fromValidHex("b8bcb07f6344b42ab04250c86a6e8b75d3fdbbc6")),
        OP_EQUALVERIFY,
        OP_2,
        OP_SWAP,
        BytesToPushOntoStack(33),
        ScriptConstant(ByteVector.fromValidHex(
          "030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e7")),
        OP_2,
        OP_CHECKMULTISIG,
        OP_ELSE,
        OP_DROP,
        BytesToPushOntoStack(2),
        ScriptConstant(ByteVector.fromValidHex("f401")),
        OP_CHECKLOCKTIMEVERIFY,
        OP_DROP,
        OP_CHECKSIG,
        OP_ENDIF,
        OP_ENDIF
      )
    }

    asm must be(expectedAsm)
  }

}
