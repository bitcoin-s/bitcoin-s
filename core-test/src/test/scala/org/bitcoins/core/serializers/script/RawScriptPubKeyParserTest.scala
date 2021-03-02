package org.bitcoins.core.serializers.script

import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, ScriptPubKey}
import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkitcore.util.TestUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/** Created by chris on 1/12/16.
  */
class RawScriptPubKeyParserTest extends BitcoinSUnitTest {
  val encode = BytesUtil.encodeHex(_: ByteVector)
  "RawScriptPubKeyParser" must "read then write the scriptPubKey and get the original scriptPubKey" in {
    val scriptPubKey: ScriptPubKey =
      RawScriptPubKeyParser.read(TestUtil.rawScriptPubKey)
    encode(RawScriptPubKeyParser.write(scriptPubKey)) must be(
      TestUtil.rawScriptPubKey)
  }

  it must "read an EmptyScriptPubKey" in {
    assert(RawScriptPubKeyParser.read(ByteVector.empty) == EmptyScriptPubKey)
  }

  it must "read a raw scriptPubKey and give us the expected asm" in {
    val scriptPubKey = RawScriptPubKeyParser.read(TestUtil.rawP2PKHScriptPubKey)
    val expectedAsm: Seq[ScriptToken] =
      List(OP_DUP,
           OP_HASH160,
           BytesToPushOntoStack(20),
           ScriptConstant("31a420903c05a0a7de2de40c9f02ebedbacdc172"),
           OP_EQUALVERIFY,
           OP_CHECKSIG)
    scriptPubKey.asm must be(expectedAsm)

  }
  it must "read a raw scriptPubKey from an output" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    //output is index 1
    val rawScriptPubKey = "17a914af575bd77c5ce7eba3bd9ce6f89774713ae62c7987"
    val scriptPubKey = RawScriptPubKeyParser.read(rawScriptPubKey)
    encode(RawScriptPubKeyParser.write(scriptPubKey)) must be(rawScriptPubKey)
  }

  it must "read and write the scriptPubKey that pushes using a PUSHDATA1 that is negative when read as signed" in {
    val rawScriptPubKey =
      "0x4c 0xae 0x606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e207460 DROP DUP HASH160 0x14 0xbfd7436b6265aa9de506f8a994f881ff08cc2872 EQUALVERIFY CHECKSIG"
    val asm = ScriptParser.fromString(rawScriptPubKey)
    val scriptPubKey = ScriptPubKey.fromAsm(asm)
    val actualRawScriptPubKey = RawScriptPubKeyParser.write(scriptPubKey)
    //the actual hex representation is from a bitcoin core test case inside of tx_valid.json
    encode(actualRawScriptPubKey) must be(
      "ca4cae606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e2074607576a914bfd7436b6265aa9de506f8a994f881ff08cc287288ac")

  }

}
