package org.bitcoins.cli

import java.time.{ZoneId, ZonedDateTime}

import org.bitcoins.commons.jsonmodels.wallet.CoinSelectionAlgo
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.sbclient.{Exchange, TradingPair}
import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
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

  implicit val oracleInfoReads: Read[OracleInfo] = new Read[OracleInfo] {
    val arity: Int = 1
    val reads: String => OracleInfo = OracleInfo.fromHex
  }

  implicit val contractInfoReads: Read[ContractInfo] =
    new Read[ContractInfo] {
      val arity: Int = 1
      val reads: String => ContractInfo = ContractInfo.fromHex
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
          case _ => BlockStamp.fromString(str)
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

  implicit val outPointsRead: Read[TransactionOutPoint] =
    new Read[TransactionOutPoint] {
      val arity: Int = 1

      val reads: String => TransactionOutPoint = TransactionOutPoint.fromHex
    }

  implicit val coinSelectionAlgoReads: Read[CoinSelectionAlgo] =
    new Read[CoinSelectionAlgo] {
      val arity: Int = 1

      val reads: String => CoinSelectionAlgo =
        CoinSelectionAlgo.fromString(_).get
    }

  implicit val schnorrSigReads: Read[SchnorrDigitalSignature] =
    new Read[SchnorrDigitalSignature] {
      override def arity: Int = 1

      override def reads: String => SchnorrDigitalSignature =
        SchnorrDigitalSignature.fromHex
    }

  implicit val partialSigReads: Read[PartialSignature] =
    new Read[PartialSignature] {
      override def arity: Int = 1

      override def reads: String => PartialSignature =
        PartialSignature.fromHex
    }

  implicit val sha256DigestBEReads: Read[Sha256DigestBE] =
    new Read[Sha256DigestBE] {
      val arity: Int = 1

      val reads: String => Sha256DigestBE = Sha256DigestBE.fromHex
    }

  implicit val dlcOfferReads: Read[DLCOffer] = new Read[DLCOffer] {
    override def arity: Int = 1

    // this will be a JSON string
    override def reads: String => DLCOffer =
      str => {
        DLCOffer.fromJson(ujson.read(str))
      }
  }

  implicit val dlcAcceptReads: Read[DLCAccept] = new Read[DLCAccept] {
    override def arity: Int = 1

    // this will be a JSON string
    override def reads: String => DLCAccept =
      str => {
        DLCAccept.fromJson(ujson.read(str))
      }
  }

  implicit val dlcSignReads: Read[DLCSign] = new Read[DLCSign] {
    override def arity: Int = 1

    // this will be a JSON string
    override def reads: String => DLCSign =
      str => {
        DLCSign.fromJson(ujson.read(str))
      }
  }

  implicit val dlcMutualCloseSigReads: Read[DLCMutualCloseSig] =
    new Read[DLCMutualCloseSig] {
      override def arity: Int = 1

      override def reads: String => DLCMutualCloseSig =
        str => DLCMutualCloseSig.fromJson(ujson.read(str))
    }

  implicit val exchangeReads: Read[Exchange] = new Read[Exchange] {
    override def arity: Int = 1

    override def reads: String => Exchange = Exchange.fromString(_).get
  }

  implicit val tradingPairReads: Read[TradingPair] = new Read[TradingPair] {
    override def arity: Int = 1

    override def reads: String => TradingPair = TradingPair.fromString
  }
}
