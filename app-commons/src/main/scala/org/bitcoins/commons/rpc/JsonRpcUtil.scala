package org.bitcoins.commons.rpc

import org.bitcoins.crypto.AesPassword
import ujson.{Arr, False, Null, Num, Obj, Str, True, Value}

object JsonRpcUtil {

  def nullToOpt(value: Value): Option[Value] =
    value match {
      case Null                      => None
      case Arr(arr) if arr.isEmpty   => None
      case Arr(arr) if arr.size == 1 => Some(arr.head)
      case _: Value                  => Some(value)
    }

  def jsToWalletNameAndPassword(
      js: Value): (Option[String], Option[AesPassword]) = {
    js match {
      case Arr(arr) =>
        arr.toList match {
          case walletNameJs :: passJs :: Nil =>
            (jsToStringOpt(walletNameJs), jsToAESPassword(passJs))
          case walletNameJs :: Nil =>
            (jsToStringOpt(walletNameJs), None)
          case Nil =>
            (None, None)
          case other =>
            throw new IllegalArgumentException(
              s"Bad number of arguments: ${other.length}. Expected: 2")
        }
      case _: Value =>
        throw new IllegalArgumentException(s"Expected json.Arr")
    }
  }

  def jsToStringOpt(js: Value): Option[String] = {
    js match {
      case Str(str) =>
        Some(str)
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }

  def jsToAESPassword(js: Value): Option[AesPassword] = {
    js match {
      case Str(str) =>
        Some(AesPassword.fromString(str))
      case Null =>
        None
      case Arr(_) | False | True | Num(_) | Obj(_) =>
        throw new IllegalArgumentException("password must be a string or null")
    }
  }
}
