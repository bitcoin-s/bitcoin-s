package org.bitcoins.testkitcore.gen

import org.scalacheck.Gen

import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}

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
      str <- Try(new String(bytes.toArray, StandardCharsets.UTF_8)) match {
        case Failure(_) =>
          genUTF8String
        case Success(value) =>
          Gen.const(value)
      }
    } yield str
  }
}

object StringGenerators extends StringGenerators
