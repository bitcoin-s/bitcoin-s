package org.bitcoins.util

/**
 * Created by chris on 7/26/15.
 */
trait ScalacoinUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)
}

object ScalacoinUtil extends ScalacoinUtil