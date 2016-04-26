package org.scalacoin.rpc.bitcoincore.blockchain

import org.scalacoin.rpc.bitcoincore.blockchain.softforks.SoftForks

/**
 * Created by Tom on 1/11/2016.
 */
trait BlockchainInfo {
  def chain : String
  def blockCount : Int
  def headerCount : Int
  def bestBlockHash : String
  def difficulty : Double
  def verificationProgress : Double
  def chainWork : String
  //def softForks : Seq[SoftForks]
}

case class BlockChainInfoImpl(chain : String, blockCount: Int, headerCount: Int, bestBlockHash: String, difficulty: Double,
         verificationProgress : Double, chainWork : String /*,softForks : Seq[SoftForks]*/) extends BlockchainInfo

