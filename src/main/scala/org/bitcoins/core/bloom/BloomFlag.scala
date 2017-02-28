package org.bitcoins.core.bloom

import org.bitcoins.core.util.Factory

/**
  * Created by chris on 8/3/16.
  * Specifies how to update a bloom filter
  * [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#filter-matching-algorithm]]
  */
sealed trait BloomFlag {
  def byte: Byte
}

/** The filtering node should not update the filter. */
case object BloomUpdateNone extends BloomFlag {
  def byte = 0.toByte
}

/**
  * If the filter matches any data element in a pubkey script,
  * the corresponding outpoint is added to the filter.
  */
case object BloomUpdateAll extends BloomFlag {
  def byte = 1.toByte
}

/**
  * If the filter matches any data element in a pubkey script and that
  * scriptPubKey is either a P2PKH or non-P2SH pay-to-multisig script,
  * the outpoint for this transaction is added to the filter.
  */
case object BloomUpdateP2PKOnly extends BloomFlag {
  def byte = 2.toByte
}


object BloomFlag extends Factory[BloomFlag] {
  private def flags = Seq(BloomUpdateNone, BloomUpdateAll, BloomUpdateP2PKOnly)
  def apply(byte: Byte): BloomFlag = {
    val flagOpt = flags.find(_.byte == byte)
    if (flagOpt.isDefined) flagOpt.get
    else throw new IllegalArgumentException("The given byte was not defined for BloomFlag, got: " + byte)
  }

  def fromBytes(bytes: Seq[Byte]): BloomFlag = BloomFlag(bytes.head)
}

