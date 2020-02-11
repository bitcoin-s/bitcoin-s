package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.{Int32, UInt32}

/**
  * Created by chris on 2/12/16.
  */
trait TransactionConstants {

  lazy val version = Int32.one
  lazy val validLockVersion = Int32.two
  lazy val lockTime = UInt32.zero
  lazy val sequence = UInt32.max
  lazy val disableRBFSequence = sequence - UInt32.one

  /**
    * If bit (1 << 31) of the sequence number is set,
    * then no consensus meaning is applied to the sequence number and can be included
    * in any block under all currently possible circumstances.
    * @return the mask that ben used with a bitwise and to indicate if the sequence number has any meaning
    */
  def locktimeDisabledFlag: UInt32 = UInt32(1L << 31)

  /**
    * If a transaction's input's sequence number encodes a relative lock-time, this mask is
    * applied to extract that lock-time from the sequence field.
    */
  def sequenceLockTimeMask: UInt32 = UInt32(0x0000ffff)

  def fullSequenceLockTimeMask: UInt32 =
    sequenceLockTimeTypeFlag | sequenceLockTimeMask

  /**
    * If the transaction input sequence number encodes a relative lock-time and this flag
    * is set, the relative lock-time has units of 512 seconds,
    * otherwise it specifies blocks with a granularity of 1.
    */
  def sequenceLockTimeTypeFlag: UInt32 = UInt32(1L << 22)

  /**
    * Threshold for nLockTime: below this value it is interpreted as block number,
    * otherwise as UNIX timestamp.
    */
  def locktimeThreshold: UInt32 = UInt32(500000000)
}

object TransactionConstants extends TransactionConstants
