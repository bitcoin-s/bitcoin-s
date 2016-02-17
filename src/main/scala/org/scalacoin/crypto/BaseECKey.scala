package org.scalacoin.crypto

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey {

  def hex : String
  def bytes : Seq[Byte]
}
