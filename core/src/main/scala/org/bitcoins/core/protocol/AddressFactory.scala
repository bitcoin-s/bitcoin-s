package org.bitcoins.core.protocol

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.crypto.StringFactory

import scala.util.Try

abstract class AddressFactory[T <: Address] extends StringFactory[T] {

  /** Same as fromString, but throws the exception */
  def fromStringExn(str: String): T = fromString(str)


  @deprecated(s"Use fromScriptPubKeyT", since="2020-06-09")
  def fromScriptPubKey(spk: ScriptPubKey, np: NetworkParameters): Try[T] = {
    fromScriptPubKeyT(spk,np)
  }

  /**
   * Attempts to create a address from the given [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
   * and [[org.bitcoins.core.config.NetworkParameters NetworkParameters]]
   */
  def fromScriptPubKeyT(spk: ScriptPubKey, np: NetworkParameters): Try[T]

  def fromScriptPubKeyOpt(spk: ScriptPubKey, np: NetworkParameters): Option[T] = {
    fromScriptPubKeyT(spk,np).toOption
  }

  /** Checks if the given string is a valid address */
  def isValid(str: String): Boolean = fromStringT(str).isSuccess
}
