package org.bitcoins.rpc

import play.api.libs.json.Reads
import play.api.libs.json.{JsResult, JsValue}
import play.api.libs.json.JsError
import play.api.libs.json.JsSuccess

/** Represents failures that can happen when using the
  * `bitcoind` RPC interface.
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/rpc/protocol.h#L32 protcol.h]]
  *      for an enumeration of all error codes used
  */
sealed abstract class BitcoindException(private val message: String)
    extends Exception {
  override def getMessage(): String = s"Error $code: $message"
  val code: Int
}

/** Wallet errors from `bitcoind` RPC calls
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/rpc/protocol.h#L32 protcol.h]]
  *      for an enumeration of all error codes used
  */

object BitcoindException {
  import org.bitcoins.rpc.BitcoindP2PException._
  import org.bitcoins.rpc.BitcoindWalletException._

  implicit val reads: Reads[BitcoindException] = new Reads[BitcoindException] {

    def reads(json: JsValue): JsResult[BitcoindException] =
      for {
        code <- (json \ "code").validate[Int]
        message <- (json \ "message").validate[String]
        exception <- BitcoindException.fromCodeAndMessage(code, message) match {
          case None =>
            JsError(
              s"Could not construct bitcoind exception with code $code and message '$message'")
          case Some(value) => JsSuccess(value)
        }
      } yield exception
  }

  private val all: List[String => BitcoindException] = List(
    InvalidParams(_),
    InternalError(_),
    ParseError(_),
    MiscError(_),
    TypeError(_),
    InvalidAddressOrKey(_),
    OutOfMemory(_),
    InvalidParameter(_),
    DatabaseError(_),
    DeserializationError(_),
    VerifyError(_),
    VerifyRejected(_),
    VerifyAlreadyInChain(_),
    InWarmUp(_),
    MethodDeprecated(_),
    ForbiddenBySafeMode(_),
    InInitialDownload(_),
    NodeAlreadyAdded(_),
    NodeNotAdded(_),
    NodeNotConnected(_),
    InvalidIpOrSubnet(_),
    P2PDisabled(_),
    WalletError(_),
    InsufficientFunds(_),
    InvalidLabelName(_),
    KeypoolRanOut(_),
    UnlockNeeded(_),
    PassphraseIncorrect(_),
    WrongEncState(_),
    EncryptionFailed(_),
    AlreadyUnlocked(_),
    NotFound(_),
    NotSpecified(_)
  )

  /** Attempts to construct a BitcoindException from the given code and message */
  def fromCodeAndMessage(
      code: Int,
      message: String): Option[BitcoindException] = {

    val constructorOpt = all.find(func => func(message).code == code)

    constructorOpt.map(func => func(message))
  }

  final case class InvalidParams(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -32602
  }

  /** InternalError is only used for genuine errors in bitcoind
    * (for example datadir corruption)
    */
  final case class InternalError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -32603
  }

  final case class ParseError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -32700
  }

  /** `std::exception` thrown in command handling */
  final case class MiscError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -1
  }

  /** Unexpected type was passed as parameter */
  final case class TypeError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -3
  }

  /** Invalid address or key */
  final case class InvalidAddressOrKey(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -5
  }

  /** Ran out of memory during operation */
  final case class OutOfMemory(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -7
  }

  /** Invalid, missing or duplicate parameter */
  final case class InvalidParameter(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -8
  }

  /** Database error */
  final case class DatabaseError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -20
  }

  /** Error parsing or validating structure in raw format */
  final case class DeserializationError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -22
  }

  /** General error during transaction or block submission */
  final case class VerifyError(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -25
  }

  /** Transaction or block was rejected by network rules */
  final case class VerifyRejected(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -26
  }

  /** Transaction already in chain */
  final case class VerifyAlreadyInChain(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -27
  }

  /** Client still warming up */
  final case class InWarmUp(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -28
  }

  /** RPC method is deprecated */
  final case class MethodDeprecated(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -32
  }

  /** Server is in safe mode, and command is not allowed in safe mode */
  final case class ForbiddenBySafeMode(private val message: String)
      extends BitcoindException(message) {
    val code: Int = -2
  }
}

/**  P2P client errors
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/rpc/protocol.h#L32 protcol.h]]
  *      for an enumeration of all error codes used
  */
sealed abstract class BitcoindP2PException(private val message: String)
    extends BitcoindException(message)

object BitcoindP2PException {

  /** Bitcoin is not connected */
  final case class NotConnected(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -9
  }

  /** Still downloading initial blocks */
  final case class InInitialDownload(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -10
  }

  /** Node is already added */
  final case class NodeAlreadyAdded(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -23
  }

  /** Node has not been added before */
  final case class NodeNotAdded(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -24
  }

  /** Node to disconnect not found in connected nodes */
  final case class NodeNotConnected(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -29
  }

  /** Invalid IP/Subnet */
  final case class InvalidIpOrSubnet(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -30
  }

  /** No valid connection manager instance found */
  final case class P2PDisabled(private val message: String)
      extends BitcoindP2PException(message) {
    val code: Int = -31
  }
}

sealed abstract class BitcoindWalletException(private val message: String)
    extends BitcoindException(message)

object BitcoindWalletException {

  /** Unspecified problem with wallet (key not found etc.) */
  final case class WalletError(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -4
  }

  /** Not enough funds in wallet or account */
  final case class InsufficientFunds(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -6
  }

  /** Invalid label name */
  final case class InvalidLabelName(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -11
  }

  /** Keypool ran out, call keypoolrefill first */
  final case class KeypoolRanOut(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -12
  }

  /** Enter the wallet passphrase with walletpassphrase first */
  final case class UnlockNeeded(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -13
  }

  /** The wallet passphrase entered was incorrect */
  final case class PassphraseIncorrect(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -14
  }

  /** Command given in wrong wallet encryption state (encrypting an encrypted wallet etc.) */
  final case class WrongEncState(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -15
  }

  /** Failed to encrypt the wallet */
  final case class EncryptionFailed(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -16
  }

  /** Wallet is already unlocked */
  final case class AlreadyUnlocked(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -17
  }

  /** Invalid wallet specified */
  final case class NotFound(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -18
  }

  /** No wallet specified (error when there are multiple wallets loaded) */
  final case class NotSpecified(private val message: String)
      extends BitcoindWalletException(message) {
    val code: Int = -19
  }

}
