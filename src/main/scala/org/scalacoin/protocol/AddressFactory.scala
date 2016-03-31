package org.scalacoin.protocol

import org.scalacoin.util.{BitcoinSUtil, Factory}

/**
 * Created by chris on 6/4/15.
 */
trait AddressFactory extends Factory[Address] {


  /**
   * Factory method for creating addresses
   * Takes in a string to check if it is an address
   * if it is it creates the address
   * if not it throws a runtime exception
   * @param str
   * @return
   */
  def factory(str : String) : Address = {
    if (AssetAddress.validate(str)) AssetAddress(str)
    else if (BitcoinAddress.validate(str)) BitcoinAddress(str)
    else throw new RuntimeException("The address that you passed in is invalid")
  }


  def fromBytes(bytes : Seq[Byte]) : Address = factory(BitcoinSUtil.encodeBase58(bytes))

  override def fromHex(hex : String) : Address = throw new RuntimeException("We cannot create a bitcoin address from hex - bitcoin addresses are base 58 encoded")
}

object AddressFactory extends AddressFactory
