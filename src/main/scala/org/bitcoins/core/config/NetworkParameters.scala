package org.bitcoins.core.config


/**
 * Created by chris on 7/27/15.
  */
sealed abstract class NetworkParameters {
  def p2pkhNetworkByte : Byte
  def p2shNetworkByte : Byte
  def privateKey : Byte
  def port : Int
  def rpcPort: Int
  def name: String
  /** The seeds used to bootstrap the network */
  def dnsSeeds : Seq[String]

  /**
    * The message start string is designed to be unlikely to occur in normal data.
    * The characters are rarely used upper ASCII, not valid as UTF-8, and produce
    * a large 32-bit integer with any alignment.
    * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L108
    */
  def magicBytes : Seq[Byte]

  /** In bitcoin, the network recaculates the difficulty for the network every 2016 blocks */
  def difficultyChangeThreshold = 2016
}

trait MainNet extends NetworkParameters {
  override def p2pkhNetworkByte = 0x00
  override def p2shNetworkByte = 0x05
  override def privateKey = 0x80.toByte
  override def port = 8333
  override def rpcPort = 8332
  //mainnet doesn't need to be specified like testnet or regtest
  override def name = ""
  override def dnsSeeds = Seq("seed.bitcoin.sipa.be","dnsseed.bluematt.me","dnsseed.bitcoin.dashjr.org",
    "seed.bitcoinstats.com","bitseed.xf2.org","seed.bitcoin.jonasschnelli.ch")

  override def magicBytes = Seq(0xf9.toByte, 0xbe.toByte, 0xb4.toByte, 0xd9.toByte)
}

object MainNet extends MainNet

trait TestNet3 extends NetworkParameters {
  override def p2pkhNetworkByte = 0x6F
  override def p2shNetworkByte = 0xC4.toByte
  override def privateKey = 0xEF.toByte
  override def port = 18333
  override def rpcPort = 18332
  override def name = "testnet"
  override def dnsSeeds = Seq("testnet-seed.bitcoin.petertodd.org",
    "testnet-seed.bluematt.me","testnet-seed.bitcoin.schildbach.de")
  override def magicBytes = Seq(0x0b.toByte, 0x11.toByte, 0x09.toByte, 0x07.toByte)
}

object TestNet3 extends TestNet3

trait RegTest extends NetworkParameters {
  override def p2pkhNetworkByte = TestNet3.p2pkhNetworkByte
  override def p2shNetworkByte = TestNet3.p2shNetworkByte
  override def privateKey = TestNet3.privateKey
  override def port = 18444
  override def rpcPort = TestNet3.rpcPort
  override def name = "regtest"
  override def dnsSeeds = Nil
  override def magicBytes = Seq(0xfa.toByte, 0xbf.toByte, 0xb5.toByte, 0xda.toByte)
}

object RegTest extends RegTest

