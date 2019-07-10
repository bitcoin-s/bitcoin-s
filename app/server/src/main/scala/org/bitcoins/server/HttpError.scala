package org.bitcoins.server

/** HTTP errors our server knows how to handle.
  * These gets picked up by the exceptions handler
  * in Main
  */
sealed abstract class HttpError extends Error

object HttpError {

  /** The RPC method was not found */
  final case class MethodNotFound(method: String) extends HttpError
}
