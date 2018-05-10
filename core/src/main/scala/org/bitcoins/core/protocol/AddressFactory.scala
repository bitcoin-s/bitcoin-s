package org.bitcoins.core.protocol

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.script.ScriptPubKey

import scala.util.Try

abstract class AddressFactory[T] {

  def fromString(str: String): Try[T]

  def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[T]

}