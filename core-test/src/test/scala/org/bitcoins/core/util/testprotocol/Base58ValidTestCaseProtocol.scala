package org.bitcoins.core.util.testprotocol

import org.bitcoins.core.protocol.Address
import org.bitcoins.crypto.{ECPrivateKey, Sha256Hash160Digest}
import spray.json._

/**
  * Created by tom on 6/14/16.
  */
object Base58ValidTestCaseProtocol extends DefaultJsonProtocol {
  import ConfigParamsProtocol._

  implicit object Base58ValidTestCaseFormatter
      extends RootJsonFormat[Base58ValidTestCase] {

    override def read(value: JsValue): Base58ValidTestCase = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _: JsValue =>
          throw new RuntimeException(
            "Core test case must be in the format of js array")
      }
      val elements: Vector[JsValue] = jsArray.elements
      val configParams: ConfigParams = elements(2).convertTo[ConfigParams]

      def addressOrPrivateKey(
          elements: Vector[JsValue]): Either[Address, String] =
        configParams.isPrivKey match {
          case false => Left(Address(elements(0).convertTo[String]))
          case true  => Right(elements(0).convertTo[String])
        }

      def isHashOrPrivKey(elements: Vector[JsValue]): Either[
        Sha256Hash160Digest,
        ECPrivateKey] =
        configParams.addrTypeOrIsCompressed match {
          case a if a.isLeft =>
            Left(Sha256Hash160Digest(elements(1).convertTo[String]))
          case b if b.isRight =>
            Right(ECPrivateKey(elements(1).convertTo[String]))
        }
      Base58ValidTestCaseImpl(addressOrPrivateKey(elements),
                              isHashOrPrivKey(elements),
                              configParams)
    }
    override def write(testCase: Base58ValidTestCase): JsValue = ???
  }
}
