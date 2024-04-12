package org.bitcoins.core.protocol.script

import org.bitcoins.crypto.{CryptoUtil, NetworkElement, Sha256Digest}
import scodec.bits.ByteVector

sealed abstract class TapscriptTree extends NetworkElement {
  def leafs: Vector[TapLeaf]
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
