package org.bitcoins.core.util.testprotocol

import org.bitcoins.core.protocol.Address
import org.bitcoins.core.util.testprotocol.ConfigParams._
import org.bitcoins.crypto.{ECPrivateKey, Sha256Hash160Digest}
import ujson._
import upickle.default._

case class Base58ValidTestCase(
    addressOrWIFPrivKey: Either[Address, String],
    hashOrPrivKey: Either[Sha256Hash160Digest, ECPrivateKey],
    configParams: ConfigParams)

object Base58ValidTestCase {

  implicit val base58ValidTestCaseR: Reader[Base58ValidTestCase] =
    reader[Value].map { value =>
      val jsArray: Arr = value match {
        case array: Arr => array
        case _: Value =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[Value] = jsArray.value.toVector
      val configParams: ConfigParams =
        upickle.default.read[ConfigParams](elements(2))

      def addressOrPrivateKey(
          elements: Vector[Value]): Either[Address, String] =
        if (configParams.isPrivKey) {
          Right(elements(0).str)
        } else {
          val addr = Address.fromString(elements(0).str)
          Left(addr)
        }

      def isHashOrPrivKey(
          elements: Vector[Value]): Either[Sha256Hash160Digest, ECPrivateKey] =
        configParams.addrTypeOrIsCompressed match {
          case a if a.isLeft =>
            Left(Sha256Hash160Digest(elements(1).str))
          case b if b.isRight =>
            Right(ECPrivateKey(elements(1).str))
          case _ =>
            sys.error(s"Should be left or right")
        }
      Base58ValidTestCase(addressOrPrivateKey(elements),
                          isHashOrPrivKey(elements),
                          configParams)
    }
}
