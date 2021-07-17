package org.bitcoins.testkitcore.gen

import org.scalacheck.Gen

import java.nio.charset.StandardCharsets

/** Created by chris on 6/20/16.
  */
trait StringGenerators {

  lazy val validHexChars = "0123456789abcdef".toCharArray

  /** Generates a hex char
    *
    * @return
    */
  def hexChar: Gen[Char] =
    Gen.choose(0, validHexChars.length - 1).map(validHexChars(_))

  /** Generates a random hex string
    *
    * @return
    */
  def hexString: Gen[String] = {
    val int = Gen.choose(0, 100)
    val hexStringGen: Gen[List[Char]] = int.flatMap { i =>
      if (i % 2 == 0) Gen.listOfN(i, hexChar)
      else Gen.listOfN(i * 2, hexChar)
    }
    hexStringGen.map(_.mkString)
  }

  def strChar: Gen[Char] = {
    val char: Gen[Gen[Char]] = for {
      randomNum <- Gen.choose(0, 4)
    } yield {
      if (randomNum == 0) Gen.numChar
      else if (randomNum == 1) Gen.alphaUpperChar
      else if (randomNum == 2) Gen.alphaLowerChar
      else if (randomNum == 3) Gen.alphaChar
      else Gen.alphaNumChar
    }
    char.flatMap(g => g)
  }

  def genString(size: Int): Gen[String] = {
    val l: Gen[Seq[Char]] = Gen.listOfN(size, strChar)
    l.map(_.mkString)
  }

  def genString: Gen[String] =
    for {
      randomNum <- Gen.choose(0, 100)
      randomString <- genString(randomNum)
    } yield randomString

  def genNonEmptyString: Gen[String] =
    for {
      randomNum <- Gen.choose(1, 100)
      randomString <- genString(randomNum)
    } yield randomString

  def genUTF8String: Gen[String] = {
    for {
      bytes <- NumberGenerator.bytes
      //this is done to get postgres tests working with utf8 strings
      //I don't think we want to keep this here, as this seems like an exploit that is possible
      //do we want to narrow the DLC spec or something? What is the point of having a
      //null character in a UTF8 string for our purposes any way?
      //To reproduce, use PG_ENABLED=1 and comment out the line below when running dlcOracleTest/test
      //see: https://stackoverflow.com/a/1348551/967713
      noZero = bytes.filterNot(_ == 0x0)
      str = new String(noZero.toArray, StandardCharsets.UTF_8)
    } yield str
  }
}

object StringGenerators extends StringGenerators
