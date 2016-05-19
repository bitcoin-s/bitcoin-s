package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.script.constant.ScriptNumber

/**
  * Created by chris on 2/12/16.
 */
trait TransactionConstants {

  lazy val version = 1
  lazy val lockTime = 0
  lazy val sequence = 4294967295L

  /**
    * If bit (1 << 31) of the sequence number is set,
    * then no consensus meaning is applied to the sequence number and can be included
    * in any block under all currently possible circumstances.
 *
    * @return the mask that ben used with a bitwise and to indicate if the sequence number has any meaning
    */
  def locktimeDisabledFlag = 1L << 31

  /**
    * If a transaction's input's sequence number encodes a relative lock-time, this mask is
    * applied to extract that lock-time from the sequence field.
    */
  def sequenceLockTimeMask = 0x0000ffff

  /**
    * If the transaction input sequence number encodes a relative lock-time and this flag
    * is set, the relative lock-time has units of 512 seconds,
    * otherwise it specifies blocks with a granularity of 1.
    */
  def sequenceLockTimeTypeFlag = (1L << 22)


  /**
    * Threshold for nLockTime: below this value it is interpreted as block number,
    * otherwise as UNIX timestamp.
 *
    * @return
    */
  def locktimeThreshold = 500000000
}

object TransactionConstants extends TransactionConstants
