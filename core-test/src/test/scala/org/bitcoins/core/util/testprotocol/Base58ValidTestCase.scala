package org.bitcoins.core.util.testprotocol

import org.bitcoins.core.crypto.{ ECPrivateKey, Sha256Hash160Digest }
import org.bitcoins.core.protocol.Address

/**
 * Created by tom on 6/14/16.
 */
trait Base58ValidTestCase {
  def addressOrWIFPrivKey: Either[Address, String]
  def hashOrPrivKey: Either[Sha256Hash160Digest, ECPrivateKey]
  def configParams: ConfigParams
}

case class Base58ValidTestCaseImpl(addressOrWIFPrivKey: Either[Address, String], hashOrPrivKey: Either[Sha256Hash160Digest, ECPrivateKey],
  configParams: ConfigParams) extends Base58ValidTestCase