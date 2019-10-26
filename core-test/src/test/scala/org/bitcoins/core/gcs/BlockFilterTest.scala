package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.testkit.util.BitcoinSUnitTest
import play.api.libs.json.{JsArray, Json}

import scala.io.Source

class BlockFilterTest extends BitcoinSUnitTest {
  behavior of "BlockFilter"

  // https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#appendix-c-test-vectors
  case class Bip158TestCase(
      blockHeight: Int,
      blockHash: DoubleSha256DigestBE,
      block: Block,
      prevOutputScripts: Vector[ScriptPubKey],
      prevHeader: DoubleSha256DigestBE,
      filter: GolombFilter,
      header: DoubleSha256DigestBE,
      notes: String
  ) {

    val clue: String = s"Test Notes: $notes"

    def runTest(): org.scalatest.Assertion = {
      val constructedFilter = BlockFilter(block, prevOutputScripts)

      assert(constructedFilter.encodedData.bytes == filter.encodedData.bytes,
             clue)

      val matcher = new BinarySearchFilterMatcher(filter)
      val constructedMatcher = new BinarySearchFilterMatcher(constructedFilter)

      assert(constructedMatcher.decodedHashes == matcher.decodedHashes, clue)

      val constructedHeader = constructedFilter.getHeader(prevHeader.flip)

      assert(constructedHeader.hash == header.flip, clue)
    }
  }

  object Bip158TestCase {

    //["Block Height,Block Hash,Block,[Prev Output Scripts for Block],Previous Basic Header,Basic Filter,Basic Header,Notes"]
    def fromJsArray(array: JsArray): Bip158TestCase = {
      val parseResult = for {
        height <- array(0).validate[Int]
        blockHash <- array(1).validate[String].map(DoubleSha256DigestBE.fromHex)

        block <- array(2).validate[String].map(Block.fromHex)

        scriptArray <- array(3).validate[JsArray]
        scripts = parseScripts(scriptArray)

        prevHeader <- array(4)
          .validate[String]
          .map(DoubleSha256DigestBE.fromHex)

        filter <- array(5)
          .validate[String]
          .map(BlockFilter.fromHex(_, blockHash.flip))

        header <- array(6).validate[String].map(DoubleSha256DigestBE.fromHex)

        notes <- array(7).validate[String]
      } yield Bip158TestCase(height,
                             blockHash,
                             block,
                             scripts,
                             prevHeader,
                             filter,
                             header,
                             notes)

      parseResult.get
    }

    private def parseScripts(array: JsArray): Vector[ScriptPubKey] = {
      val hexScripts = array.validate[Vector[String]].get

      hexScripts.map(ScriptPubKey.fromAsmHex)
    }
  }

  it must "pass bip 158 test vectors" in {
    val source = Source.fromURL(getClass.getResource("/testnet-19.json"))

    val vec: Vector[JsArray] =
      Json.parse(source.mkString).validate[Vector[JsArray]].get.tail
    val testCases = vec.map(Bip158TestCase.fromJsArray)

    testCases.foreach(_.runTest())
  }
}
