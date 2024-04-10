package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script.{ScriptPubKey, TaprootScriptPubKey}
import org.bitcoins.crypto.{Sha256Digest, XOnlyPubKey}
import upickle.default._

case class ScriptTree(id: Int, spk: ScriptPubKey, leafVersion: Byte)

case class Given(internalPubkey: XOnlyPubKey, scriptTrees: Vector[ScriptTree])

case class Intermediary(
    leafHashes: Option[Vector[Sha256Digest]],
    merkleRootOpt: Option[Sha256Digest],
    tweak: Sha256Digest,
    tweakedPubkey: XOnlyPubKey)

case class Expected(
    scriptPubKey: TaprootScriptPubKey,
    bip350Address: Bech32mAddress)

case class TaprootWalletTestCase(
    `given`: Given,
    intermediary: Intermediary,
    expected: Expected)

case class TaprootWalletTestCases(tests: Vector[TaprootWalletTestCase])

object TaprootWalletTestCase {

  implicit val walletTestVectorReader: Reader[TaprootWalletTestCases] = {
    reader[ujson.Obj].map { obj =>
      val testCases = obj("scriptPubKey").arr.map { testCase =>
        val givenObj = testCase("given").obj
        val internalPubkey = XOnlyPubKey.fromHex(givenObj("internalPubkey").str)
        val scriptTree = parseScriptTree(givenObj("scriptTree"))

        val `given` = Given(internalPubkey, scriptTree)

        val intermediaryObj = testCase("intermediary").obj

        val intermediary = parseIntermediary(intermediaryObj)

        val expectedObj = testCase("expected").obj
        val spk =
          TaprootScriptPubKey.fromAsmHex(expectedObj("scriptPubKey").str)
        val bip350Address =
          Bech32mAddress.fromString(expectedObj("bip350Address").str)
        val expected = Expected(spk, bip350Address)

        TaprootWalletTestCase(`given`, intermediary, expected)

      }
      TaprootWalletTestCases(testCases.toVector)
    }
  }

  private def parseScriptTree(`given`: ujson.Value): Vector[ScriptTree] = {
    if (`given`.isNull) Vector.empty
    else if (`given`.objOpt.isDefined) {
      val givenObj = `given`.obj
      val id = givenObj.obj("id").num.toInt
      val script = ScriptPubKey.fromAsmHex(givenObj("script").str)
      val leafVersion = givenObj("leafVersion").num.toByte
      Vector(ScriptTree(id, script, leafVersion))
    } else {
      `given`.arr.map(parseScriptTree).flatten.toVector
    }
  }

  private def parseIntermediary(intermediaryObj: ujson.Obj): Intermediary = {
    val merkleRootOpt = {
      if (intermediaryObj("merkleRoot").isNull) None
      else Some(Sha256Digest.fromHex(intermediaryObj("merkleRoot").str))
    }
    val tweak = Sha256Digest.fromHex(intermediaryObj("tweak").str)
    val tweakedPubkey =
      XOnlyPubKey.fromHex(intermediaryObj("tweakedPubkey").str)

    val leafHashesOpt =
      if (intermediaryObj.value.exists(_._1 == "leafHashes")) {
        val leaves = intermediaryObj("leafHashes").arr.map(l =>
          Sha256Digest.fromHex(l.str))
        Some(leaves.toVector)
      } else {
        None
      }
    Intermediary(leafHashesOpt, merkleRootOpt, tweak, tweakedPubkey)
  }
}
