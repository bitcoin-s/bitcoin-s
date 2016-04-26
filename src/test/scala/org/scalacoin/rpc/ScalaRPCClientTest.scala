package org.scalacoin.rpc

import org.scalacoin.marshallers.rpc.bitcoincore.blockchain.{ConfirmedUnspentTransactionOutputMarshaller, MemPoolInfoMarshaller, BlockchainInfoMarshaller}
import org.scalacoin.marshallers.rpc.bitcoincore.mining.MiningInfoMarshaller
import org.scalacoin.marshallers.rpc.bitcoincore.networking.NetworkMarshaller
import org.scalacoin.marshallers.rpc.bitcoincore.wallet.WalletMarshaller
import org.scalacoin.protocol.Address
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._

import scala.sys.process.Process

/**
  * Created by tom on 4/26/16.
  */
class ScalaRPCClientTest extends FlatSpec with MustMatchers {
  val client : String = "bitcoin-cli"
  val network : String = "-testnet"
  val test = new ScalaRPCClient(client, network)

  "sendCommand" must "send a command to the command line and return the output" in {
    test.getBlockCount must be (test.sendCommand("getblockcount").trim.toInt)
  }

  it must "parse and return networkinfo" in {
    val networkInfo = test.sendCommand("getnetworkinfo")
    val json = networkInfo.parseJson
    test.getNetworkInfo must be (NetworkMarshaller.NetworkInfoFormatter.read(json))
    test.getNetworkInfo.version must be (NetworkMarshaller.NetworkInfoFormatter.read(json).version)
  }

  it must "parse and return mininginfo" in {
    val miningInfo = test.sendCommand("getmininginfo")
    val json = miningInfo.parseJson
    test.getMiningInfo must be (MiningInfoMarshaller.MiningInfoFormatter.read(json))
  }

  it must "parse and return blockchaininfo" in {
    val blockchainInfo = test.sendCommand("getblockchaininfo")
    val json = blockchainInfo.parseJson
    test.getBlockChainInfo must be (BlockchainInfoMarshaller.BlockchainInfoFormatter.read(json))
  }

  it must "parse and return mempoolinfo" in {
    val mempoolInfo = test.sendCommand("getmempoolinfo")
    val json = mempoolInfo.parseJson
    test.getMemPoolInfo must be (MemPoolInfoMarshaller.MemPoolInfoFormatter.read(json))
  }

  it must "parse and return txoutset info" in {
    val txOutSetInfo = test.sendCommand("gettxoutsetinfo")
    val json = txOutSetInfo.parseJson
    test.getTxOutSetInfo must be {
      ConfirmedUnspentTransactionOutputMarshaller.ConfirmedUnspentTransactionOutputFormatter.read(json)
    }
  }

  it must "parse and return wallet info" in {
    val walletInfo = test.sendCommand("getwalletinfo")
    val json = walletInfo.parseJson
    test.getWalletInfo must be (WalletMarshaller.WalletFormatter.read(json))
  }

  it must "get difficuluty" in {
    val difficulty = test.sendCommand("getdifficulty")
    test.getDifficulty must be (difficulty.trim.toDouble)
  }

  it must "get new address" in {
    val address : Address = test.getNewAddress
  }

  it must "get raw change address" in {
    val rawchangeaddress : Address = test.getRawChangeAddress
  }

  it must "get the balance" in {
    val balance = test.sendCommand("getbalance")
    test.getBalance must be (balance.trim.toDouble)
  }

  it must "get best block hash" in {
    val bestBlockHash = test.getBestBlockHash
  }

  it must "add a 1-of-1 multisig address" in {
    val address = "mp1z2kUC5pDv2CkqvfVrxw2J9PVeqGeuQJ"
    val result = test.generateOneOfOneMultiSigAddress(1, address)
  }

}
