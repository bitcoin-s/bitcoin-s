package org.scalacoin.protocol.rpc

import org.scalacoin.marshallers.blockchain.{ConfirmedUnspentTransactionOutputMarshaller, BlockchainInfoMarshaller, MemPoolInfoMarshaller}
import org.scalacoin.marshallers.mining.MiningInfoMarshaller
import org.scalacoin.protocol.blockchain.{ConfirmedUnspentTransactionOutput, BlockchainInfo, MemPoolInfo}
import org.scalacoin.protocol.mining.GetMiningInfo
import spray.json._
import scala.sys.process._

/**
 * Created by Tom on 1/14/2016.
 */
class ScalaRPCClient (client : String, network : String) {
  def sendCommand(command : String) : String = {
    val cmd = client + " " + network + " " + command
    val result = cmd.!!
    result
  }

  /**
   * The number of blocks in the local best block chain. For a new node with only the hardcoded genesis block,
   * this number will be 0
   * https://bitcoin.org/en/developer-reference#getblockcount
   * @return
   */
  def getBlockCount : Int = sendCommand("getblockcount").trim.toInt

  /**
   * The getmempoolinfo RPC returns information about the node's current transaction memory pool.
   * https://bitcoin.org/en/developer-reference#getmempoolinfo
   * @return
   */
  def getMemPoolInfo : MemPoolInfo = {
    val result : String = sendCommand("getmempoolinfo")
    MemPoolInfoMarshaller.MemPoolInfoFormatter.read(result.parseJson)
  }

  /**
   * Information about the current state of the local block chain.
   * https://bitcoin.org/en/developer-reference#getblockchaininfo
   * @return
   */
  def getBlockChainInfo : BlockchainInfo = {
    val result : String = sendCommand("getblockchaininfo")
    BlockchainInfoMarshaller.BlockchainInfoFormatter.read(result.parseJson)
  }

  /**
   * The gettxoutsetinfo RPC returns statistics about the confirmed unspent transaction output (UTXO) set.
   * Note that this call may take some time and that it only counts outputs from confirmed transactions—it does
   * not count outputs from the memory pool.
   * https://bitcoin.org/en/developer-reference#gettxoutsetinfo
   * @return
   */
  def getTxOutSetInfo : ConfirmedUnspentTransactionOutput = {
    val result : String = sendCommand("gettxoutsetinfo")
    ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.read(result.parseJson)
  }

  /**
   * The getmininginfo RPC returns various mining-related information.
   * https://bitcoin.org/en/developer-reference#getmininginfo
   * @return
   */
  def getMiningInfo : GetMiningInfo = {
    val result : String = sendCommand("getmininginfo")
    MiningInfoMarshaller.MiningInfoFormatter.read(result.parseJson)
  }


}
