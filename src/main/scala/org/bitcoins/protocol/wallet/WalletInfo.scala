package org.bitcoins.protocol.wallet

/**
 * Created by Tom on 1/6/2016.
 */
trait WalletInfo {
  def walletVersion : Int
  def balance : Double
  def unconfirmedBalance : Double
  def immatureBalance : Double
  def txCount : Int
  def keyPoolOldest : Long
  def keyPoolSize : Int
}

case class WalletInfoImpl(walletVersion : Int, balance : Double, unconfirmedBalance : Double, immatureBalance : Double,
 txCount : Int, keyPoolOldest : Long, keyPoolSize : Int) extends WalletInfo