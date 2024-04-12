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
}

case class TapLeaf(leafVersion: Int, spk: ScriptPubKey) extends TapscriptTree {

  override val bytes: ByteVector =
    ByteVector.fromInt(leafVersion, 1) ++ spk.bytes
  val sha256: Sha256Digest = CryptoUtil.tapLeafHash(bytes)
  override val leafs: Vector[TapLeaf] = Vector(this)
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
            x(0) //odd number of leafs
          }
        }
        loop(branches.toVector)
      }
    }
    loop(leafs)
  }
}
