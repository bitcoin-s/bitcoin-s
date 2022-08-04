package org.bitcoins.core.api.commons

/** A trait to indicate where an argument came from */
sealed trait ArgumentSource[+T]

object ArgumentSource {

  /** Means this argument was passed via the rpc */
  case class RpcArgument[T](arg: T) extends ArgumentSource[T]

  case object NoArgument extends ArgumentSource[Nothing]
}
