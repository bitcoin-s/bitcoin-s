package org.bitcoins.cli

import java.time.{ZoneId, ZonedDateTime}

import org.bitcoins.appCommons.JsonSerializers
import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ptlc.PTLCMessage._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import scopt._

/** scopt readers for parsing CLI params and options */
object CliReaders {

  implicit val npReads: Read[NetworkParameters] =
    new Read[NetworkParameters] {
      val arity: Int = 1

      val reads: String => NetworkParameters = str =>
        Networks.knownNetworks
          .find(_.toString.toLowerCase == str.toLowerCase)
          .getOrElse {
            val networks =
              Networks.knownNetworks
                .map(_.toString.toLowerCase)
                .mkString(", ")
            val msg =
              s"$str is not a valid network! Valid networks: $networks"
            sys.error(msg)
          }
    }

  implicit val bitcoinAddressReads: Read[BitcoinAddress] =
    new Read[BitcoinAddress] {
      val arity: Int = 1

      val reads: String => BitcoinAddress = BitcoinAddress.fromStringExn
    }

  implicit val bitcoinsReads: Read[Bitcoins] =
    new Read[Bitcoins] {
      val arity: Int = 1
      val reads: String => Bitcoins = str => Bitcoins(BigDecimal(str))
    }

  implicit val satoshisReads: Read[Satoshis] =
    new Read[Satoshis] {
      val arity: Int = 1

      val reads: String => Satoshis = str => Satoshis(BigInt(str))
    }

  implicit val satoshisPerVirtualByteReads: Read[SatoshisPerVirtualByte] =
    new Read[SatoshisPerVirtualByte] {
      val arity: Int = 1

      val reads: String => SatoshisPerVirtualByte = str =>
        SatoshisPerVirtualByte(Satoshis(BigInt(str)))
    }

  implicit val uInt32Reads: Read[UInt32] = new Read[UInt32] {
    val arity: Int = 1

    val reads: String => UInt32 = str => UInt32(BigInt(str))
  }

  implicit val blockStampReads: Read[BlockStamp] =
    new Read[BlockStamp] {
      val arity: Int = 1
      private val dateRe = """(\d4)-(\d2)-(\d2)""".r

      val reads: String => BlockStamp = str =>
        str match {
          case dateRe(year, month, day) =>
            val time = ZonedDateTime.of(year.toInt,
                                        month.toInt,
                                        day.toInt,
                                        0,
                                        0,
                                        0,
                                        0,
                                        ZoneId.of("UTC"))
            BlockTime(time)
          case _ => BlockStamp.fromString(str).get
        }
    }

  implicit val psbtReads: Read[PSBT] =
    new Read[PSBT] {
      val arity: Int = 1

      val reads: String => PSBT = PSBT.fromString
    }

  implicit val txReads: Read[Transaction] = new Read[Transaction] {
    val arity: Int = 1

    val reads: String => Transaction = Transaction.fromHex
  }

  implicit val sha256DigestBEReads: Read[Sha256DigestBE] =
    new Read[Sha256DigestBE] {
      val arity: Int = 1

      val reads: String => Sha256DigestBE = Sha256DigestBE.fromHex
    }

  implicit val ptlcInvoiceReads: Read[PTLCInvoice] = new Read[PTLCInvoice] {
    override def arity: Int = 1

    // this will be a JSON string
    override def reads: String => PTLCInvoice = str => {
      JsonSerializers.getPTLCInvoice(ujson.read(str))
    }
  }

  implicit val ptlcAcceptReads: Read[PTLCAccept] = new Read[PTLCAccept] {
    override def arity: Int = 1

    // this will be a JSON string
    override def reads: String => PTLCAccept = str => {
      JsonSerializers.getPTLCAccept(ujson.read(str))
    }
  }

  implicit val ptlcRefundSigReads: Read[PTLCRefundSignature] =
    new Read[PTLCRefundSignature] {
      override def arity: Int = 1

      // this will be a JSON string
      override def reads: String => PTLCRefundSignature = str => {
        JsonSerializers.getPTLCRefundSignature(ujson.read(str))
      }
    }
}
