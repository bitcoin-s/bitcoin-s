package org.bitcoins.protocol

/**
 * Created by chris on 6/4/15.
 */
object AddressUtil {


  /**
   * Factory method for creating addresses
   * Takes in a string to check if it is an address
   * if it is it creates the address
   * if not it throws a runtime exception
   * @param str
   * @return
   */
  def address(str : String) : Address = {
    if (AssetAddress.validate(str)) AssetAddress(str)
    else if (BitcoinAddress.validate(str)) BitcoinAddress(str)
    else throw new RuntimeException("The address that you passed in is invalid")
  }

}
