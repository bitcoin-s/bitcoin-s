package org.bitcoins.core.config


/**
 * Created by chris on 7/27/15.
  */
trait NetworkParameters {
  def p2pkhNetworkByte : Byte
  def p2shNetworkByte : Byte
  def port : Int
}

trait MainNet extends NetworkParameters {
  override def p2pkhNetworkByte = 0x00
  override def p2shNetworkByte = 0x05
  override def port = 8333
}

object MainNet extends MainNet

trait TestNet3 extends NetworkParameters {
  override def p2pkhNetworkByte = 0x6F
  override def p2shNetworkByte = 196.toByte
  override def port = 18333
}

object TestNet3 extends TestNet3

trait RegTest extends NetworkParameters {
  override def p2pkhNetworkByte = TestNet3.p2pkhNetworkByte
  override def p2shNetworkByte = TestNet3.p2shNetworkByte
  override def port = 18444

}

object RegTest extends RegTest

