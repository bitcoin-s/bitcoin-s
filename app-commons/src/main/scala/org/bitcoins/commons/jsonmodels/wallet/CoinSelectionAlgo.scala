package org.bitcoins.commons.jsonmodels.wallet

/** Represents the various ways the wallet can do coin selection */
sealed abstract class CoinSelectionAlgo

object CoinSelectionAlgo {

  /** Randomly selects utxos until it has enough to fund the desired amount,
    * should only be used for research purposes */
  final case object RandomSelection extends CoinSelectionAlgo

  /** Greedily selects from walletUtxos starting with the largest outputs, skipping outputs with values
    * below their fees. Better for high fee environments than accumulateSmallestViable.
    */
  final case object AccumulateLargest extends CoinSelectionAlgo

  /**
    * Greedily selects from walletUtxos starting with the smallest outputs, skipping outputs with values
    * below their fees. Good for low fee environments to consolidate UTXOs.
    *
    * Has the potential privacy breach of connecting a ton of UTXOs to one address.
    */
  final case object AccumulateSmallestViable extends CoinSelectionAlgo

  /** Greedily selects from walletUtxos in order, skipping outputs with values below their fees */
  final case object StandardAccumulate extends CoinSelectionAlgo

  val all: Vector[CoinSelectionAlgo] =
    Vector(RandomSelection,
           AccumulateLargest,
           AccumulateSmallestViable,
           StandardAccumulate)

  def fromString(str: String): Option[CoinSelectionAlgo] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }
}
