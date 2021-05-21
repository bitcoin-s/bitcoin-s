package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.dlc.compute.DLCAdaptorPointComputer.AdditionTrieNode
import org.bitcoins.crypto.{CryptoUtil, ECPublicKey}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.annotation.tailrec

class AdditionTrieNodeTest extends BitcoinSUnitTest {
  behavior of "AdditionTrieNode"

  val base: Int = 2
  val nonces: Int = 10

  @tailrec
  private def computeAllPrefixes(accum: Vector[Vector[Vector[Int]]] =
    Vector.empty): Vector[Vector[Int]] = {
    if (accum.length == nonces) {
      accum.flatten
    } else {
      val newPrefixes = if (accum.isEmpty) {
        0.until(base).toVector.map(Vector(_))
      } else {
        accum.last.flatMap { prefix =>
          0.until(base).toVector.map(digit => prefix.:+(digit))
        }
      }
      computeAllPrefixes(accum.:+(newPrefixes))
    }
  }

  val allPrefixes: Vector[Vector[Int]] = {
    computeAllPrefixes()
  }

  val preComputeTable: Vector[Vector[ECPublicKey]] =
    Vector.fill(nonces)(Vector.fill(base)(ECPublicKey.freshPublicKey))

  def newTrie(): AdditionTrieNode = {
    AdditionTrieNode.makeRoot(preComputeTable)
  }

  it should "correctly compute all elements" in {
    val trie = newTrie()
    allPrefixes.foreach { prefix =>
      val ptsToAdd = prefix.zipWithIndex.map {
        case (outcomeIndex, nonceIndex) =>
          preComputeTable(nonceIndex)(outcomeIndex)
      }
      val expected = CryptoUtil.combinePubKeys(ptsToAdd)

      assert(trie.computeSum(prefix) == expected)
    }
  }
}
