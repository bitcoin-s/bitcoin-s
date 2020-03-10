package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.util.CryptoUtil
import scodec.bits.{BitVector, ByteVector}

/**
  * Represents a GCS encoded set with all parameters specified
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-coded-sets]]
  *
  * TODO: Replace ByteVector with a type for keys
  */
case class GolombFilter(
    key: SipHashKey,
    m: UInt64,
    p: UInt8,
    n: CompactSizeUInt,
    encodedData: BitVector)
    extends NetworkElement {
  lazy val f: UInt64 = n.num * m

  /** The hash of this serialized filter */
  lazy val hash: DoubleSha256Digest = {
    CryptoUtil.doubleSHA256(this.bytes)
  }

  lazy val hashBE: DoubleSha256DigestBE = {
    hash.flip
  }

  /** Given the previous FilterHeader, constructs the header corresponding to this */
  def getHeader(prevHeader: FilterHeader): FilterHeader = {
    FilterHeader(filterHash = this.hash, prevHeaderHash = prevHeader.hash)
  }

  /** Given the previous FilterHeader hash, constructs the header corresponding to this */
  def getHeader(prevHeaderHash: DoubleSha256Digest): FilterHeader = {
    FilterHeader(filterHash = this.hash, prevHeaderHash = prevHeaderHash)
  }

  override def bytes: ByteVector = {
    n.bytes ++ encodedData.bytes
  }

  def hashToRange(item: ByteVector): UInt64 = GCS.hashToRange(item, f, key)

}
