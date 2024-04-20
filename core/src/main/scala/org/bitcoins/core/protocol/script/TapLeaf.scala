package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{CryptoUtil, NetworkElement, Sha256Digest}
import scodec.bits.ByteVector

import scala.annotation.tailrec

sealed abstract class TapscriptTree extends NetworkElement {
  def leafs: Vector[TapLeaf]

  def merkleRoot: Sha256Digest = {
    TaprootScriptPath.computeFullTreeMerkleRoot(this)
  }
}

case class TapBranch(tree1: TapscriptTree, tree2: TapscriptTree)
    extends TapscriptTree {

  override val bytes: ByteVector = {
    tree1.bytes ++ tree2.bytes
  }

  override def leafs: Vector[TapLeaf] = {
    tree1.leafs ++ tree2.leafs
  }

  override def toString(): String = {
    s"{${tree1.toString},${tree2.toString}}"
  }
}

case class TapLeaf(leafVersion: Byte, spk: ScriptPubKey) extends TapscriptTree {

  override val bytes: ByteVector =
    ByteVector.fromInt(leafVersion, 1) ++ spk.bytes
  val sha256: Sha256Digest = CryptoUtil.tapLeafHash(bytes)
  override val leafs: Vector[TapLeaf] = Vector(this)

  override def toString(): String = {
    s"${spk.toString}"
  }
}

object TapLeaf {
  val leafVersion: Byte = 0xc0.toByte

  /** BIP342 specifies validity rules that apply for leaf version 0xc0, but
    * future proposals can introduce rules for other leaf versions.
    *
    * @see
    *   https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#rationale
    */
  val knownLeafVersions: Vector[Byte] = Vector(leafVersion, 0xc1.toByte)
}

object TapscriptTree {

  def buildTapscriptTree(leafs: Vector[TapLeaf]): TapscriptTree = {
    @tailrec
    def loop(subtree: Vector[TapscriptTree]): TapscriptTree = {
      if (subtree.length == 1) subtree.head
      else {
        val branches = subtree.grouped(2).map { x =>
          if (x.length == 2) {
            TapBranch(x(0), x(1))
          } else {
            x(0) // odd number of leafs
          }
        }
        loop(branches.toVector)
      }
    }
    loop(leafs)
  }
}
