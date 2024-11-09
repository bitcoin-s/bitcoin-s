package org.bitcoins.crypto

abstract class DigitalSignature extends NetworkElement {
  def hashTypeOpt: Option[HashType]
  def appendHashType(hashType: HashType): DigitalSignature
}
