package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.CoinSelectionAlgo._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee._

import scala.annotation.tailrec
import scala.util.control.Breaks._
import scala.util.{Random, Try}

/** Implements algorithms for selecting from a UTXO set to spend to an output set at a given fee rate. */
trait CoinSelector {

  private val BNB_MAX_TRIES = 100000

  /** Selects coins using the branch and bound algorithm.
    * TODO this isn't very scala-y, could be refactored.
    * @see https://github.com/bitcoindevkit/bdk/blob/6bae52e6f2843a371ec2a6e293ea7ab0ba96aff4/src/wallet/coin_selection.rs#L529
    */
  def branchAndBound(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      changeCost: CurrencyUnit,
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    // target is the total of the outputs we are trying to spend
    // plus the 10 overhead bytes of the transaction and size of the outputs
    val nonInputFees = feeRate * (outputs.map(_.byteSize).sum + 10)
    val target =
      outputs.foldLeft(CurrencyUnits.zero)(_ + _.value) + nonInputFees

    if (target <= CurrencyUnits.zero)
      throw new IllegalArgumentException(
        "Target amount must be greater than zero")

    // Filter dust coins
    val usableUtxos =
      walletUtxos
        .filter(_.effectiveValue(feeRate) > CurrencyUnits.zero)
        .sortBy(_.value)(Ordering[CurrencyUnit].reverse)

    var currentAvailValue =
      usableUtxos.foldLeft(CurrencyUnits.zero)(_ + _.effectiveValue(feeRate))

    // Note that currentSelection.size could be less than
    // usableUtxos.size, it just means that we still haven't decided if we should keep
    // certain usableUtxos or not.
    var currentSelection: Vector[Boolean] = Vector.empty
    var currentSelectionValue = CurrencyUnits.zero

    var bestSelection: Vector[Boolean] = Vector.empty
    var bestSelectionValue: Option[CurrencyUnit] = None

    breakable {
      0.until(BNB_MAX_TRIES).foreach { _ =>
        val backtrack =
          if (
            currentSelectionValue + currentAvailValue < target
            || currentSelectionValue > target + changeCost
          ) {
            true
          } else if (currentSelectionValue >= target) {
            // If we found a solution better than the previous one, or if there wasn't previous
            // solution, update the best solution
            if (
              bestSelectionValue.isEmpty || currentSelectionValue < bestSelectionValue.get
            ) {
              bestSelection = currentSelection
              bestSelectionValue = Some(currentSelectionValue)
            }

            // If we found a perfect match, break here
            if (currentSelectionValue == target) {
              break()
            }

            // Selected value is within range, there's no point in going forward. Start
            // backtracking
            true
          } else false

        if (backtrack) {
          while (currentSelection.lastOption.contains(false)) {
            currentSelection = currentSelection.dropRight(1)
            currentAvailValue += usableUtxos(currentSelection.size)
              .effectiveValue(feeRate)
          }

          if (currentSelection.lastOption.isEmpty) {
            // We have walked back to the first utxo and no branch is untraversed. All solutions searched
            // If best selection is empty, then there's no exact match
            if (bestSelectionValue.isEmpty) {
              throw new RuntimeException(
                "No solution found for the given parameters")
            } else break()
          }

          if (currentSelection.lastOption.contains(true)) {
            // Output was included on previous iterations, try excluding now.
            currentSelection = currentSelection.dropRight(1) :+ false
          }

          val utxo = usableUtxos(currentSelection.size - 1)
          currentSelectionValue -= utxo.effectiveValue(feeRate)
        } else {
          // Moving forwards, continuing down this branch
          val utxo = usableUtxos(currentSelection.size)

          // Remove this utxo from the curr_available_value utxo amount
          currentAvailValue -= utxo.effectiveValue(feeRate)

          // Inclusion branch first (Largest First Exploration)
          currentSelection = currentSelection :+ true
          currentSelectionValue += utxo.effectiveValue(feeRate)
        }
      }
    }

    if (bestSelection.isEmpty)
      throw new RuntimeException("Could not find a bnb solution")

    usableUtxos.zip(bestSelection).flatMap { case (utxo, keep) =>
      if (keep) Some(utxo) else None
    }
  }

  /** Randomly selects utxos until it has enough to fund the desired amount,
    * should only be used for research purposes
    */
  def randomSelection(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val randomUtxos = Random.shuffle(walletUtxos)

    accumulate(randomUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos starting with the largest outputs, skipping outputs with values
    * below their fees. Better for high fee environments than accumulateSmallestViable.
    */
  def accumulateLargest(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val sortedUtxos =
      walletUtxos.sortBy(_.prevOut.value).reverse

    accumulate(sortedUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos starting with the smallest outputs, skipping outputs with values
    * below their fees. Good for low fee environments to consolidate UTXOs.
    *
    * Has the potential privacy breach of connecting a ton of UTXOs to one address.
    */
  def accumulateSmallestViable(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val sortedUtxos = walletUtxos.sortBy(_.prevOut.value)

    accumulate(sortedUtxos, outputs, feeRate)
  }

  /** Greedily selects from walletUtxos in order, skipping outputs with values below their fees */
  def accumulate(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit): Vector[CoinSelectorUtxo] = {
    val totalValue = outputs.foldLeft(CurrencyUnits.zero) {
      case (totVal, output) => totVal + output.value
    }

    @tailrec
    def addUtxos(
        alreadyAdded: Vector[CoinSelectorUtxo],
        valueSoFar: CurrencyUnit,
        feeSoFar: CurrencyUnit,
        utxosLeft: Vector[CoinSelectorUtxo]): Vector[CoinSelectorUtxo] = {
      if (valueSoFar > totalValue + feeSoFar) {
        alreadyAdded
      } else if (utxosLeft.isEmpty) {
        throw new RuntimeException(
          s"Not enough value in given outputs ($valueSoFar) to make transaction spending $totalValue plus fees $feeSoFar")
      } else {
        val nextUtxo = utxosLeft.head
        val effectiveValue = calcEffectiveValue(nextUtxo, feeRate)
        if (effectiveValue <= Satoshis.zero) {
          addUtxos(alreadyAdded, valueSoFar, feeSoFar, utxosLeft.tail)
        } else {
          val newAdded = alreadyAdded.:+(nextUtxo)
          val newValue = valueSoFar + nextUtxo.prevOut.value
          val utxoFee = calculateUtxoFee(nextUtxo, feeRate)

          addUtxos(newAdded, newValue, feeSoFar + utxoFee, utxosLeft.tail)
        }
      }
    }

    val nonInputFees = feeRate * (outputs.map(_.byteSize).sum + 10)

    addUtxos(alreadyAdded = Vector.empty,
             valueSoFar = CurrencyUnits.zero,
             feeSoFar = nonInputFees,
             utxosLeft = walletUtxos)
  }

  def calculateUtxoFee(
      utxo: CoinSelectorUtxo,
      feeRate: FeeUnit): CurrencyUnit = {
    val size = feeRate match {
      case _: SatoshisPerByte =>
        CoinSelector.approximateUtxoBytesSize(utxo)
      case _: SatoshisPerKiloByte =>
        CoinSelector.approximateUtxoBytesSize(utxo)
      case _: SatoshisPerVirtualByte =>
        CoinSelector.approximateUtxoVBytesSize(utxo)
      case _: SatoshisPerKW =>
        CoinSelector.approximateUtxoWeight(utxo)
    }

    feeRate * size
  }

  def calcEffectiveValue(
      utxo: CoinSelectorUtxo,
      feeRate: FeeUnit): CurrencyUnit = {
    val utxoFee = calculateUtxoFee(utxo, feeRate)
    utxo.prevOut.value - utxoFee
  }
}

object CoinSelector extends CoinSelector {

  /** Cribbed from [[https://github.com/bitcoinjs/coinselect/blob/master/utils.js]] */
  def approximateUtxoBytesSize(utxo: CoinSelectorUtxo): Long = {
    val inputBase = 32 + 4 + 1 + 4
    val scriptSize = utxo.redeemScriptOpt match {
      case Some(script) => script.bytes.length
      case None =>
        utxo.scriptWitnessOpt match {
          case Some(script) => script.bytes.length
          case None =>
            utxo.prevOut.scriptPubKey match {
              case _: NonWitnessScriptPubKey        => 107 // P2PKH
              case _: WitnessScriptPubKeyV0         => 107 // P2WPKH
              case _: TaprootScriptPubKey           => 64 // Single Schnorr signature
              case _: UnassignedWitnessScriptPubKey => 0 // unknown
            }
        }
    }

    inputBase + scriptSize
  }

  def approximateUtxoVBytesSize(utxo: CoinSelectorUtxo): Long = {
    val inputBase = 32 + 4 + 1 + 4
    val scriptSize = utxo.redeemScriptOpt match {
      case Some(script) => script.bytes.length
      case None =>
        utxo.scriptWitnessOpt match {
          case Some(script) => script.bytes.length
          case None =>
            utxo.prevOut.scriptPubKey match {
              case _: NonWitnessScriptPubKey        => 107 // P2PKH
              case _: WitnessScriptPubKeyV0         => 26 // P2WPKH
              case _: TaprootScriptPubKey           => 17 // Single Schnorr signature
              case _: UnassignedWitnessScriptPubKey => 0 // unknown
            }
        }
    }

    inputBase + scriptSize
  }

  def approximateUtxoWeight(utxo: CoinSelectorUtxo): Long = {
    val inputBase = (32 + 4 + 1 + 4) * 3
    val scriptSize = utxo.redeemScriptOpt match {
      case Some(script) => script.bytes.length * 3
      case None =>
        utxo.scriptWitnessOpt match {
          case Some(script) => script.bytes.length
          case None =>
            utxo.prevOut.scriptPubKey match {
              case _: NonWitnessScriptPubKey        => 107 // P2PKH
              case _: WitnessScriptPubKeyV0         => 107 // P2WPKH
              case _: TaprootScriptPubKey           => 64 // Single Schnorr signature
              case _: UnassignedWitnessScriptPubKey => 0 // unknown
            }
        }
    }

    inputBase + scriptSize
  }

  def selectByAlgo(
      coinSelectionAlgo: CoinSelectionAlgo,
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      changeCostOpt: Option[CurrencyUnit],
      longTermFeeRateOpt: Option[FeeUnit] = None): Vector[CoinSelectorUtxo] =
    coinSelectionAlgo match {
      case RandomSelection =>
        randomSelection(walletUtxos, outputs, feeRate)
      case AccumulateLargest =>
        accumulateLargest(walletUtxos, outputs, feeRate)
      case AccumulateSmallestViable =>
        accumulateSmallestViable(walletUtxos, outputs, feeRate)
      case StandardAccumulate =>
        accumulate(walletUtxos, outputs, feeRate)
      case BranchAndBound =>
        changeCostOpt match {
          case Some(changeCost) =>
            branchAndBound(walletUtxos, outputs, changeCost, feeRate)
          case None =>
            throw new IllegalArgumentException(
              "changeCostOpt must be defined for BranchAndBound")
        }
      case LeastWaste =>
        (longTermFeeRateOpt, changeCostOpt) match {
          case (Some(longTermFeeRate), Some(changeCost)) =>
            selectByLeastWaste(walletUtxos,
                               outputs,
                               feeRate,
                               changeCost,
                               longTermFeeRate)
          case (None, None) | (Some(_), None) | (None, Some(_)) =>
            throw new IllegalArgumentException(
              "longTermFeeRateOpt and changeCostOpt must be defined for LeastWaste")
        }
      case SelectedUtxos(outPoints) =>
        val result = walletUtxos.foldLeft(Vector.empty[CoinSelectorUtxo]) {
          (acc, utxo) =>
            val outPoint = (utxo.outPoint.txId, utxo.outPoint.vout.toInt)
            if (outPoints(outPoint)) acc :+ utxo else acc
        }
        if (result.toSet.size < outPoints.size) {
          outPoints.foreach { outPoint =>
            if (
              !result.exists(utxo =>
                utxo.outPoint.txId == outPoint._1 && utxo.outPoint.vout.toInt == outPoint._2)
            )
              throw new IllegalArgumentException(
                s"Unknown UTXO: ${outPoint._1}:${outPoint._2}")
          }
        } else if (result.size > outPoints.size) {
          throw new IllegalArgumentException(s"Found too many UTXOs")
        }
        result
    }

  private case class CoinSelectionResults(
      waste: CurrencyUnit,
      totalSpent: CurrencyUnit,
      selection: Vector[CoinSelectorUtxo])

  implicit
  private val coinSelectionResultsOrder: Ordering[CoinSelectionResults] = {
    case (a: CoinSelectionResults, b: CoinSelectionResults) =>
      if (a.waste == b.waste) {
        a.selection.size.compare(b.selection.size)
      } else a.waste.compare(b.waste)
  }

  def selectByLeastWaste(
      walletUtxos: Vector[CoinSelectorUtxo],
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      changeCost: CurrencyUnit,
      longTermFeeRate: FeeUnit
  ): Vector[CoinSelectorUtxo] = {
    val target = outputs.map(_.value).sum
    val results = CoinSelectionAlgo.independentAlgos.flatMap { algo =>
      // Skip failed selection attempts
      Try {
        val selection =
          selectByAlgo(algo,
                       walletUtxos,
                       outputs,
                       feeRate,
                       Some(changeCost),
                       Some(longTermFeeRate))

        val waste = calculateSelectionWaste(selection,
                                            Some(changeCost),
                                            target,
                                            feeRate,
                                            longTermFeeRate)

        val totalSpent = selection.map(_.prevOut.value).sum
        CoinSelectionResults(waste, totalSpent, selection)
      }.toOption
    }

    require(
      results.nonEmpty,
      s"Not enough value in given outputs to make transaction spending $target plus fees")

    results.min.selection
  }

  /** Compute the waste for this result given the cost of change
    * and the opportunity cost of spending these inputs now vs in the future.
    * If change exists, waste = changeCost + inputs * (effective_feerate - long_term_feerate)
    * If no change, waste = excess + inputs * (effective_feerate - long_term_feerate)
    * where excess = totalEffectiveValue - target
    * change_cost = effective_feerate * change_output_size + long_term_feerate * change_spend_size
    *
    * Copied from
    * @see https://github.com/achow101/bitcoin/blob/4f5ad43b1e05cd7b403f87aae4c4d42e5aea810b/src/wallet/coinselection.cpp#L345
    *
    * @param utxos The selected inputs
    * @param changeCostOpt The cost of creating change and spending it in the future. None if there is no change.
    * @param target The amount targeted by the coin selection algorithm.
    * @param longTermFeeRate The expected average fee rate used over the long term
    * @return The waste
    */
  def calculateSelectionWaste(
      utxos: Vector[CoinSelectorUtxo],
      changeCostOpt: Option[CurrencyUnit],
      target: CurrencyUnit,
      feeRate: FeeUnit,
      longTermFeeRate: FeeUnit): CurrencyUnit = {
    require(
      utxos.nonEmpty,
      "This function should not be called with empty inputs as that would mean the selection failed")

    val (waste, selectedEffectiveValue) =
      utxos.foldLeft((CurrencyUnits.zero, CurrencyUnits.zero)) {
        case ((waste, selectedEffectiveValue), utxo) =>
          val fee = calculateUtxoFee(utxo, feeRate)
          val longTermFee = calculateUtxoFee(utxo, longTermFeeRate)
          val effectiveValue = calcEffectiveValue(utxo, feeRate)

          val newWaste = waste + fee - longTermFee
          val newSelectedEffectiveValue =
            selectedEffectiveValue + effectiveValue

          (newWaste, newSelectedEffectiveValue)
      }

    changeCostOpt match {
      case Some(changeCost) =>
        // Consider the cost of making change and spending it in the future
        // If we aren't making change, the caller should've set changeCost to 0
        require(changeCost > Satoshis.zero,
                "Cannot have a change cost less than 1")
        waste + changeCost
      case None =>
        // When we are not making change (changeCost == 0), consider the excess we are throwing away to fees
        require(selectedEffectiveValue >= target)
        waste + selectedEffectiveValue - target
    }
  }
}
