package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.Bech32mAddress
import org.bitcoins.core.protocol.script._
import org.bitcoins.crypto.{Sha256Digest, XOnlyPubKey}
import upickle.default._

case class Given(internalPubkey: XOnlyPubKey, treeOpt: Option[TapscriptTree]) {

  val leafHashes: Vector[Sha256Digest] = {
    treeOpt match {
      case None => Vector.empty
      case Some(t) =>
        t.leafs.map(_.sha256)
    }
  }

  def merkleRootOpt: Option[Sha256Digest] = {
    treeOpt.map(s => TaprootScriptPath.computeFullTreeMerkleRoot(s))
  }
}

case class Intermediary(
    leafHashes: Option[Vector[Sha256Digest]],
    merkleRootOpt: Option[Sha256Digest],
    tweak: Sha256Digest,
    tweakedPubkey: XOnlyPubKey
)

case class Expected(
    scriptPubKey: TaprootScriptPubKey,
    bip350Address: Bech32mAddress
)

case class TaprootWalletTestCase(
    `given`: Given,
    intermediary: Intermediary,
    expected: Expected
)

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

  private def parseScriptTree(`given`: ujson.Value): Option[TapscriptTree] = {
    if (`given`.isNull) None
    else if (`given`.objOpt.isDefined) {
      val givenObj = `given`.obj
      val script = ScriptPubKey.fromAsmHex(givenObj("script").str)
      val leafVersion = givenObj("leafVersion").num.toByte
      val leaf = TapLeaf(leafVersion, script)
      Some(leaf)
    } else {
      val arr = `given`.arr
      require(
        arr.length == 2,
        s"tapscript is a binary tre, not ${arr.length} tree"
      )
      val result: Vector[Option[TapscriptTree]] =
        arr.map(parseScriptTree).toVector
      val branch = TapBranch(result(0).get, result(1).get)
      Some(branch)
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
          Sha256Digest.fromHex(l.str)
        )
        Some(leaves.toVector)
      } else {
        None
      }
    Intermediary(leafHashesOpt, merkleRootOpt, tweak, tweakedPubkey)
  }
}
