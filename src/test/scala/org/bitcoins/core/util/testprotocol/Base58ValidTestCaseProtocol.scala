package org.bitcoins.core.util.testprotocol

import org.bitcoins.core.crypto.{ECPrivateKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.util.BitcoinSLogger
import spray.json._

/**
  * Created by tom on 6/14/16.
  */
object Base58ValidTestCaseProtocol extends DefaultJsonProtocol with BitcoinSLogger {
  import ConfigParamsProtocol._
  implicit object Base58ValidTestCaseFormatter extends RootJsonFormat[Base58ValidTestCase] {
    override def read(value: JsValue): Base58ValidTestCase = {
      val jsArray: JsArray = value match {
        case array: JsArray => array
        case _: JsValue => throw new RuntimeException("Core test case must be in the format of js array")
      }
      val elements: Vector[JsValue] = jsArray.elements
      val configParams : ConfigParams = elements(2).convertTo[ConfigParams]
      val addressOrPrivKey : Either[Address, String] = {
        if (!configParams.isPrivKey) {
          Left(Address(elements(0).convertTo[String]))
        }
        else Right(elements(0).convertTo[String])
      }
      val hashOrPrivKey : Either[Sha256Hash160Digest, ECPrivateKey] = {
        if (configParams.addrTypeOrIsCompressed.isLeft) {
          Left(Sha256Hash160Digest(elements(1).convertTo[String]))
        } else Right(ECPrivateKey(elements(1).convertTo[String]))
      }
      Base58ValidTestCaseImpl(addressOrPrivKey, hashOrPrivKey, configParams)
    }
    override def write (testCase : Base58ValidTestCase) : JsValue = ???
  }
}

