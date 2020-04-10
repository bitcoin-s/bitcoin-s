package org.bitcoins.bench.eclair

import java.util.concurrent.ConcurrentHashMap
import java.util.function.BiFunction

import org.bitcoins.commons.jsonmodels.eclair.{PaymentId, WebSocketEvent}
import org.bitcoins.commons.jsonmodels.eclair.WebSocketEvent.{
  PaymentFailed,
  PaymentReceived,
  PaymentSent
}
import org.bitcoins.core.crypto.Sha256Digest

import scala.concurrent.Promise

object PaymentLog {

  case class PaymentLogEntry(
      paymentHash: Option[Sha256Digest] = None,
      id: Option[PaymentId] = None,
      event: Option[WebSocketEvent] = None,
      paymentSentAt: Long,
      paymentIdReceivedAt: Long = 0,
      eventReceivedAt: Long = 0) {

    def withPaymentHash(paymentHash: Sha256Digest): PaymentLogEntry =
      copy(paymentHash = Some(paymentHash),
           paymentSentAt = System.currentTimeMillis())

    def withPaymentId(id: PaymentId): PaymentLogEntry =
      copy(id = Some(id), paymentIdReceivedAt = System.currentTimeMillis())

    def withEvent(e: WebSocketEvent): PaymentLogEntry =
      copy(event = Some(e), eventReceivedAt = System.currentTimeMillis())

    def completed: Boolean = event.isDefined

    def completedAt: Long = event match {
      case None => 0
      case Some(e) =>
        e match {
          case PaymentReceived(_, parts) =>
            parts.maxBy(_.timestamp).timestamp.toEpochMilli
          case PaymentFailed(_, _, _, timestamp) => timestamp.toEpochMilli
          case _: WebSocketEvent =>
            throw new RuntimeException("Can't extract a timestamp")
        }
    }

    def toCSV: String =
      s"""${paymentHash
        .map(_.hex)
        .getOrElse("")},${id.map(_.toString).getOrElse("")},${event
        .map(_.getClass.getName.split('$').last)
        .getOrElse("")},$paymentSentAt,$paymentIdReceivedAt,$eventReceivedAt,${paymentIdReceivedAt - paymentSentAt},${eventReceivedAt - paymentIdReceivedAt}"""
  }

  object PaymentLogEntry {

    def apply(paymentHash: Sha256Digest): PaymentLogEntry = {
      PaymentLogEntry(paymentSentAt = System.currentTimeMillis(),
                      paymentHash = Some(paymentHash))
    }
  }

  val paymentLog =
    new ConcurrentHashMap[Sha256Digest, PaymentLogEntry]()

  val promises =
    new ConcurrentHashMap[Sha256Digest, Promise[Unit]]()

  def logPaymentHash(paymentHash: Sha256Digest): PaymentLogEntry = {
    val entry =
      paymentLog.putIfAbsent(paymentHash, PaymentLogEntry(paymentHash))
    promises.putIfAbsent(paymentHash, Promise())
    entry
  }

  def logPaymentId(
      paymentHash: Sha256Digest,
      paymentId: PaymentId): PaymentLogEntry = {
    paymentLog.compute(
      paymentHash,
      new BiFunction[Sha256Digest, PaymentLogEntry, PaymentLogEntry] {
        override def apply(
            hash: Sha256Digest,
            old: PaymentLogEntry): PaymentLogEntry = {
          val log = if (old == null) {
            PaymentLogEntry(paymentSentAt = 0, paymentHash = Some(hash))
          } else {
            old
          }
          log.withPaymentId(paymentId)
        }
      }
    )
  }

  def logEvent(event: WebSocketEvent): PaymentLogEntry = {
    val hash: Sha256Digest = event match {
      case PaymentReceived(paymentHash, _) =>
        paymentHash
      case PaymentSent(_, paymentHash, _, _) => paymentHash
      case PaymentFailed(_, paymentHash, _, _) =>
        paymentHash
      case _: WebSocketEvent =>
        throw new RuntimeException("Can't extract payment hash")
    }

    val entry = paymentLog.compute(
      hash,
      new BiFunction[Sha256Digest, PaymentLogEntry, PaymentLogEntry] {
        override def apply(
            hash: Sha256Digest,
            old: PaymentLogEntry): PaymentLogEntry = {
          val log = if (old == null) {
            PaymentLogEntry(paymentSentAt = 0, paymentHash = Some(hash))
          } else {
            old
          }
          log.withEvent(event)
        }
      }
    )
    promises.compute(
      hash,
      new BiFunction[Sha256Digest, Promise[Unit], Promise[Unit]] {
        override def apply(
            hash: Sha256Digest,
            old: Promise[Unit]): Promise[Unit] = {
          val promise = if (old == null) {
            Promise[Unit]()
          } else {
            old
          }
          promise.success(())
          promise
        }
      }
    )
    entry
  }

}
