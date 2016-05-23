package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.util.BitcoinSUtil

/**
  * Created by chris on 5/22/16.
  * CChainParams defines various tweakable parameters of a given instance of the
  * Bitcoin system. There are three: the main network on which people trade goods
  * and services, the public test network which gets reset from time to time and
  * a regression test mode which is intended for private networks only. It has
  * minimal difficulty to ensure that blocks can be found instantly.
  * Mimics this C++ interface
  * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.h#L42
  */
sealed trait ChainParams {
  /**
    * Return the BIP70 network string (main, test or regtest)
    *
    * @return
    */
  def networkId : String

  /**
    * The gensis block in the blockchain
    *
    * @return
    */
  def genesisBlock : Block

  /**
    * Filter transactions that do not match well-defined patterns
    * inside of Policy
    *
    * @return
    */
  def requireStandardTransaction : Boolean

  /**
    * Takes in a Base58Type and returns its base58 prefix
    *
    * @param base58
    * @return
    */
  def base58Prefix(base58 : Base58Type) : Seq[Byte] = base58Prefixes(base58)

  /**
    * The mapping from a Base58Type to a String
    *
    * @return
    */
  def base58Prefixes : Map[Base58Type,Seq[Byte]]

  /**
    * The seeds used to bootstrap the network
    *
    * @return
    */
  def dnsSeeds : Seq[String]
}


/**
  * This is the main network parameters
  */
object MainNetChainParams extends ChainParams {
  override def networkId = "main"
  override def genesisBlock = ???
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] =
    Map(PubKeyAddress -> Seq(1.toByte, 0.toByte),
      ScriptAddress -> Seq(1.toByte, 5.toByte),
      SecretKey -> Seq(1.toByte, 128.toByte),
      ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
        BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
      ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
        BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4")))


  override def dnsSeeds = Seq("seed.bitcoin.sipa.be","dnsseed.bluematt.me","dnsseed.bitcoin.dashjr.org",
    "seed.bitcoinstats.com","bitseed.xf2.org","seed.bitcoin.jonasschnelli.ch")

}

object TestNetChainParams extends ChainParams {

  override def networkId = "test"
  override def genesisBlock : Block = ???
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] = Map(
    PubKeyAddress -> Seq(1.toByte, 111.toByte),
      ScriptAddress -> Seq(1.toByte, 196.toByte),
      SecretKey -> Seq(1.toByte, 239.toByte),
      ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
        BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
      ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
        BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94")))


  override def dnsSeeds = Seq("testnet-seed.bitcoin.petertodd.org",
    "testnet-seed.bluematt.me","testnet-seed.bitcoin.schildbach.de")
}


object RegTestNetChainParams extends ChainParams {
  override def networkId = "regtest"
  override def genesisBlock : Block = ???
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type, Seq[Byte]] = TestNetChainParams.base58Prefixes

  override def dnsSeeds = Seq()
}



sealed trait Base58Type
case object PubKeyAddress extends Base58Type
case object ScriptAddress extends Base58Type
case object SecretKey extends Base58Type
case object ExtPublicKey extends Base58Type
case object ExtSecretKey extends Base58Type