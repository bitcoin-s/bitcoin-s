package org.bitcoins.bench.eclair

import org.bitcoins.bench.eclair.PaymentLog.PaymentLogEntry

import scala.jdk.CollectionConverters._

object EclairBenchUtil {

  def paymentLogValues(): Vector[PaymentLogEntry] = {
    PaymentLog.paymentLog
      .values()
      .asScala
      .toVector
  }

  def convertStrings(strings: Vector[String]): java.util.List[String] = {
    strings.asJava
  }
}
