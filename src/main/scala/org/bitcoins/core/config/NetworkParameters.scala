package org.bitcoins.core.config


/**
 * Created by chris on 7/27/15.
  */
trait NetworkParameters {
  def p2pkhNetworkByte : Byte
  def p2shNetworkByte : Byte
  def privateKey : Byte
  def port : Int
  /**
    * The seeds used to bootstrap the network
    *
    * @return
    */
  def dnsSeeds : Seq[String]
}

trait MainNet extends NetworkParameters {
  override def p2pkhNetworkByte = 0x00
  override def p2shNetworkByte = 0x05
  override def privateKey = 0x80.toByte
  override def port = 8333
  override def dnsSeeds = Seq("seed.bitcoin.sipa.be","dnsseed.bluematt.me","dnsseed.bitcoin.dashjr.org",
    "seed.bitcoinstats.com","bitseed.xf2.org","seed.bitcoin.jonasschnelli.ch")

}

object MainNet extends MainNet

trait TestNet3 extends NetworkParameters {
  override def p2pkhNetworkByte = 0x6F
  override def p2shNetworkByte = 0xC4.toByte
  override def privateKey = 0xEF.toByte
  override def port = 18333
  override def dnsSeeds = Seq("testnet-seed.bitcoin.petertodd.org",
    "testnet-seed.bluematt.me","testnet-seed.bitcoin.schildbach.de")
}

object TestNet3 extends TestNet3

trait RegTest extends NetworkParameters {
  override def p2pkhNetworkByte = TestNet3.p2pkhNetworkByte
  override def p2shNetworkByte = TestNet3.p2shNetworkByte
  override def privateKey = TestNet3.privateKey
  override def port = 18444
  override def dnsSeeds = Seq()

}

object RegTest extends RegTest

