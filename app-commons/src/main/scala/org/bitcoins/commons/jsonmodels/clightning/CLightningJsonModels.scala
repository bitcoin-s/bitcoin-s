package org.bitcoins.commons.jsonmodels.clightning

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel._
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.ln.node._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto._
import scodec.bits._

sealed abstract class CLightningJsonModel

object CLightningJsonModels {

  case class CLightningAddress(`type`: String, `address`: String, port: Int)
      extends CLightningJsonModel

  case class CLightningInfo(
      id: NodeId,
      alias: String,
      color: String,
      num_peers: Int,
      num_pending_channels: Int,
      num_active_channels: Int,
      num_inactive_channels: Int,
      version: String,
      `lightning-dir`: String,
      blockheight: Int,
      network: BitcoinNetwork,
      fees_collected_msat: String,
      address: Vector[CLightningAddress],
      binding: Vector[CLightningAddress]
  ) extends CLightningJsonModel

  case class NewAddressResult(
      bech32: Option[BitcoinAddress],
      `p2sh-segwit`: Option[BitcoinAddress]
  ) extends CLightningJsonModel {
    val address: BitcoinAddress = bech32.getOrElse(`p2sh-segwit`.get)
  }

  case class Output(
      txid: DoubleSha256DigestBE,
      output: UInt32,
      value: Satoshis,
      scriptpubkey: ScriptPubKey,
      status: OutputStatus,
      reserved: Boolean,
      address: Option[BitcoinAddress],
      redeemscript: Option[ScriptPubKey],
      blockheight: Option[Long],
      reserved_to_block: Option[Long]
  ) extends CLightningJsonModel {
    val outPoint: TransactionOutPoint = TransactionOutPoint(txid, output)
  }

  sealed abstract class OutputStatus extends CLightningJsonModel

  object OutputStatus extends StringFactory[OutputStatus] {
    case object Spent extends OutputStatus
    case object Unconfirmed extends OutputStatus
    case object Confirmed extends OutputStatus

    val all = Vector(Spent, Unconfirmed, Confirmed)

    override def fromStringOpt(string: String): Option[OutputStatus] =
      all.find(_.toString.toLowerCase == string.toLowerCase)

    override def fromString(string: String): OutputStatus = {
      fromStringOpt(string) match {
        case Some(value) => value
        case None =>
          sys.error(s"Could not find an OutputStatus for string $string")
      }
    }
  }

  case class ChannelFunds(
      peer_id: NodeId,
      our_amount_msat: MilliSatoshis,
      amount_msat: MilliSatoshis,
      funding_txid: DoubleSha256DigestBE,
      funding_output: UInt32,
      connected: Boolean,
      state: String,
      short_channel_id: Option[ShortChannelId]
  ) extends CLightningJsonModel

  case class ListFundsResult(
      outputs: Vector[Output],
      channels: Vector[ChannelFunds])
      extends CLightningJsonModel

  case class Channel(
      source: NodeId,
      destination: NodeId,
      short_channel_id: ShortChannelId,
      public: Boolean,
      satoshis: Satoshis,
      message_flags: Int,
      channel_flags: Int,
      active: Boolean,
      last_update: UInt64,
      base_fee_millisatoshi: MilliSatoshis,
      fee_per_millionth: Int,
      delay: Int
  ) extends CLightningJsonModel

  case class ListChannelsResult(channels: Vector[Channel])
      extends CLightningJsonModel

  case class ConnectResult(
      id: NodeId,
      features: ByteVector,
      direction: ConnectionDirection,
      address: CLightningAddress
  ) extends CLightningJsonModel

  sealed abstract class ConnectionDirection extends CLightningJsonModel

  object ConnectionDirection extends StringFactory[ConnectionDirection] {
    case object In extends ConnectionDirection
    case object Out extends ConnectionDirection

    val all = Vector(In, Out)

    override def fromStringOpt(string: String): Option[ConnectionDirection] =
      all.find(_.toString.toLowerCase == string.toLowerCase)

    override def fromString(string: String): ConnectionDirection = {
      fromStringOpt(string) match {
        case Some(value) => value
        case None =>
          sys.error(s"Could not find a ConnectionDirection for string $string")
      }
    }
  }

  case class CLightningPeerChannel(
      state: String,
      opener: LocalOrRemote,
      closer: Option[LocalOrRemote],
      status: Vector[String],
      to_us_msat: MilliSatoshis,
      total_msat: MilliSatoshis,
      fee_base_msat: MilliSatoshis,
      fee_proportional_millionths: Long,
      features: Vector[String]
  ) extends CLightningJsonModel

  case class CLightningPeer(
      id: NodeId,
      connected: Boolean,
      features: ByteVector,
      netaddr: Vector[String],
      channels: Vector[CLightningPeerChannel])
      extends CLightningJsonModel

  case class CLightningPeers(peers: Vector[CLightningPeer])
      extends CLightningJsonModel

  sealed abstract class LocalOrRemote extends CLightningJsonModel {
    def isLocal: Boolean
  }

  object LocalOrRemote extends StringFactory[LocalOrRemote] {

    case object Local extends LocalOrRemote {
      override val isLocal: Boolean = true
    }

    case object Remote extends LocalOrRemote {
      override val isLocal: Boolean = false
    }

    val all = Vector(Local, Remote)

    override def fromStringOpt(string: String): Option[LocalOrRemote] =
      all.find(_.toString.toLowerCase == string.toLowerCase)

    override def fromString(string: String): LocalOrRemote = {
      fromStringOpt(string) match {
        case Some(value) => value
        case None =>
          sys.error(s"Could not find an Opener for string $string")
      }
    }
  }

  case class WalletBalances(
      balance: CurrencyUnit,
      unconfirmedBalance: CurrencyUnit,
      confirmedBalance: CurrencyUnit
  ) extends CLightningJsonModel

  case class FundChannelResult(
      tx: Transaction,
      txid: DoubleSha256DigestBE,
      outnum: UInt32,
      channel_id: FundedChannelId,
      close_to: Option[ScriptPubKey]
  ) extends CLightningJsonModel

  case class CLightningInvoiceResult(
      bolt11: LnInvoice,
      payment_hash: Sha256Digest,
      payment_secret: PaymentSecret,
      expires_at: UInt64
  ) extends CLightningJsonModel

  case class CLightningLookupInvoiceResult(
      label: String,
      description: String,
      bolt11: LnInvoice,
      msatoshi: Option[MilliSatoshis],
      msatoshi_received: Option[MilliSatoshis],
      payment_hash: Sha256Digest,
      payment_preimage: Option[PaymentPreimage],
      pay_index: Long,
      status: InvoiceStatus,
      expires_at: UInt64
  ) extends CLightningJsonModel

  case class CLightningListInvoicesResult(
      invoices: Vector[CLightningLookupInvoiceResult])
      extends CLightningJsonModel

  case class CLightningPsbtResult(signed_psbt: PSBT) extends CLightningJsonModel

  sealed abstract class InvoiceStatus extends CLightningJsonModel {
    def paid: Boolean
  }

  object InvoiceStatus extends StringFactory[InvoiceStatus] {

    case object Paid extends InvoiceStatus {
      override val paid: Boolean = true
    }

    case object Unpaid extends InvoiceStatus {
      override val paid: Boolean = false
    }

    case object Expired extends InvoiceStatus {
      override val paid: Boolean = false
    }

    val all = Vector(Paid, Unpaid, Expired)

    override def fromStringOpt(string: String): Option[InvoiceStatus] =
      all.find(_.toString.toLowerCase == string.toLowerCase)

    override def fromString(string: String): InvoiceStatus = {
      fromStringOpt(string) match {
        case Some(value) => value
        case None =>
          sys.error(s"Could not find an InvoiceStatus for string $string")
      }
    }
  }

  case class CLightningPayResult(
      destination: Option[NodeId],
      payment_preimage: PaymentPreimage,
      payment_hash: Sha256Digest,
      created_at: BigDecimal,
      parts: Long,
      msatoshi: MilliSatoshis,
      msatoshi_sent: MilliSatoshis
  ) extends CLightningJsonModel

  case class InputReservation(
      txid: DoubleSha256DigestBE,
      vout: UInt32,
      was_reserved: Boolean,
      reserved: Boolean,
      reserved_to_block: Long
  ) extends CLightningJsonModel

  case class InputReservations(reservations: Vector[InputReservation])
      extends CLightningJsonModel

  case class CloseChannelResult(
      `type`: ClosedChannelType,
      tx: Option[Transaction],
      txid: Option[DoubleSha256DigestBE]
  ) extends CLightningJsonModel

  sealed abstract class ClosedChannelType extends CLightningJsonModel

  object ClosedChannelType extends StringFactory[ClosedChannelType] {
    case object Mutual extends ClosedChannelType
    case object Unilateral extends ClosedChannelType
    case object Unopened extends ClosedChannelType

    val all = Vector(Mutual, Unilateral, Unopened)

    override def fromStringOpt(string: String): Option[ClosedChannelType] =
      all.find(_.toString.toLowerCase == string.toLowerCase)

    override def fromString(string: String): ClosedChannelType = {
      fromStringOpt(string) match {
        case Some(value) => value
        case None =>
          sys.error(s"Could not find a ClosedChannelType for string $string")
      }
    }
  }

  case class WithdrawResult(
      tx: Transaction,
      txid: DoubleSha256DigestBE,
      psbt: PSBT
  ) extends CLightningJsonModel

  case class FundChannelStartResult(
      funding_address: BitcoinAddress,
      scriptpubkey: ScriptPubKey,
      close_to: Option[ScriptPubKey]
  ) extends CLightningJsonModel

  case class FundChannelCompleteResult(
      channel_id: FundedChannelId
  ) extends CLightningJsonModel

  case class FundChannelCancelResult(
      cancelled: String
  ) extends CLightningJsonModel

  case class CLightningTransaction(
      hash: DoubleSha256DigestBE,
      rawtx: Transaction,
      blockheight: Int,
      txindex: Int,
      locktime: Long,
      version: Int
  ) extends CLightningJsonModel

  case class ListTransactionsResults(
      transactions: Vector[CLightningTransaction]
  ) extends CLightningJsonModel

  case class SendCustomMessageResult(
      status: String
  ) extends CLightningJsonModel
}
