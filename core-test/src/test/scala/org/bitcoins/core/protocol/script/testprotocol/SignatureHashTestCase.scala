package org.bitcoins.core.protocol.script.testprotocol

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.{ Int32, UInt32 }
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType

/**
 * Created by tom on 7/21/16.
 */
trait SignatureHashTestCase {
  def transaction: Transaction
  def script: ScriptPubKey
  def inputIndex: UInt32
  def hashTypeNum: Int32
  def hashType: HashType
  def hash: DoubleSha256Digest
}

case class SignatureHashTestCaseImpl(transaction: Transaction, script: ScriptPubKey, inputIndex: UInt32, hashTypeNum: Int32, hashType: HashType,
  hash: DoubleSha256Digest) extends SignatureHashTestCase