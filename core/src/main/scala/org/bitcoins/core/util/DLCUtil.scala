package org.bitcoins.core.util

import org.bitcoins.core.protocol.dlc.models.NumericContractDescriptor

import java.io.{File, FileWriter}
import java.nio.file.Path
import scala.util.Try

/** DLC Utils */
object DLCUtil {

  /** Writes the given payout curve defined in the contract descriptor as a CSV file
    * @param fileName CSV file name
    * @param contractDescriptor the numeric contract descriptor
    * @param outcomeLabel the label for the outcome column
    * @param payoutLabel the label for the payout column
    * @param emptyFirstColumn if true it creates an empty first column (useful for Apple Numbers)
    */
  def writePayoutCurveAsCSV(
      fileName: String,
      contractDescriptor: NumericContractDescriptor,
      outcomeLabel: String,
      payoutLabel: String,
      emptyFirstColumn: Boolean): Unit = DLCUtil.writePayoutCurveAsCSV(
    new File(fileName),
    contractDescriptor,
    outcomeLabel,
    payoutLabel,
    emptyFirstColumn)

  /** Writes the given payout curve defined in the contract descriptor as a CSV file
    * @param path CSV file path
    * @param contractDescriptor the numeric contract descriptor
    * @param outcomeLabel the label for the outcome column
    * @param payoutLabel the label for the payout column
    * @param emptyFirstColumn if true it creates an empty first column (useful for Apple Numbers)
    */
  def writePayoutCurveAsCSV(
      path: Path,
      contractDescriptor: NumericContractDescriptor,
      outcomeLabel: String,
      payoutLabel: String,
      emptyFirstColumn: Boolean): Unit = DLCUtil.writePayoutCurveAsCSV(
    path.toFile,
    contractDescriptor,
    outcomeLabel,
    payoutLabel,
    emptyFirstColumn)

  /** Writes the given payout curve defined in the contract descriptor as a CSV file
    * @param fileName CSV file
    * @param contractDescriptor the numeric contract descriptor
    * @param outcomeLabel the label for the outcome column
    * @param payoutLabel the label for the payout column
    * @param emptyFirstColumn if true it creates an empty first column (useful for Apple Numbers)
    */
  def writePayoutCurveAsCSV(
      file: File,
      contractDescriptor: NumericContractDescriptor,
      outcomeLabel: String,
      payoutLabel: String,
      emptyFirstColumn: Boolean): Unit = {
    val writer = new FileWriter(file)
    try {
      val firstColumn = if (emptyFirstColumn) "," else ""
      writer.write(s"$firstColumn$outcomeLabel,$payoutLabel\n")
      val maxValue = (Math.pow(2, contractDescriptor.numDigits) - 1).toLong
      0L.to(maxValue).foreach { outcome =>
        val payoutT = Try(
          contractDescriptor
            .outcomeValueFunc(outcome, contractDescriptor.roundingIntervals)
            .toLong)
        payoutT.foreach(payout =>
          writer.append(s"$firstColumn$outcome,$payout\n"))
      }
    } finally writer.close()
  }

}
