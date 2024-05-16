package org.bitcoins.core.api.mempool

import org.bitcoins.core.currency.Satoshis

sealed abstract class MempoolAcceptResult {

  /** Raw base fees in satoshis. */
  def vsize: Long

  /** The feerate at which this transaction was considered. This includes any
    * fee delta added using prioritisetransaction (i.e. modified fees). If this
    * transaction was submitted as a package, this is the package feerate, which
    * may also include its descendants and/or ancestors (see
    * m_wtxids_fee_calculations below).
    */
  def baseFee: Satoshis
}

object MempoolAcceptResult {

  case class Valid(vsize: Long, baseFee: Satoshis) extends MempoolAcceptResult
  case class Invalid(vsize: Long, baseFee: Satoshis) extends MempoolAcceptResult

  /** A tx that we already have in our mempool */
  case class DuplicateEntry(vsize: Long, baseFee: Satoshis)
      extends MempoolAcceptResult

  /** Not validated. A same-txid-different-witness tx (see m_other_wtxid)
    * already exists in the mempool and was not replaced.
    */
  case class DifferentWitness(vsize: Long, baseFee: Satoshis)
      extends MempoolAcceptResult
}
