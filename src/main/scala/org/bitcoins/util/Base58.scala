package org.bitcoins.util

/**
  * Created by chris on 5/16/16.
  */
trait Base58 {

  /**
    * Takes the given sequence of bytes and converts it into a base58 string
    * @param bytes
    * @return the base58 string
    */
  def encode(bytes: Seq[Byte]): String = ???


  /**
    * Takes in a base58 string and converts it into a sequence of bytes
    * @param base58
    * @return the sequence of bytes representing the base58 string
    */
  def decode(base58: String): Seq[Byte] = ???
}

object Base58 extends Base58
