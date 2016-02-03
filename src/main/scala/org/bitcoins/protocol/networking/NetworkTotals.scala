package org.bitcoins.protocol.networking

import scala.math.BigInt

/**
 * Created by Tom on 1/8/2016.
 */
trait NetworkTotals {
  def totalBytesRecv : Int
  def totalBytesSent : Int
  def timeInMilliSeconds : BigInt
}

case class NetworkTotalsImpl(totalBytesRecv : Int, totalBytesSent : Int, timeInMilliSeconds : BigInt) extends NetworkTotals