package org.bitcoins.core.gen

import org.scalacheck.Gen

/**
  * Created by chris on 6/20/16.
  */
trait StringGenerators {

  lazy val validHexChars = "0123456789abcdef".toCharArray

  /**
    * Generates a hex char
    *
    * @return
    */
  def hexChar : Gen[Char] = Gen.choose(0,validHexChars.length - 1).map(validHexChars(_))

  /**
    * Generates a random hex string
 *
    * @return
    */
  def hexString : Gen[String] = {
    val int = Gen.choose(0,100)
    val hexStringGen : Gen[List[Char]] = int.flatMap { i =>
      if (i % 2 == 0) Gen.listOfN(i, hexChar)
      else Gen.listOfN(i * 2, hexChar)
    }
    hexStringGen.map(_.mkString)
  }
}

object StringGenerators extends StringGenerators
