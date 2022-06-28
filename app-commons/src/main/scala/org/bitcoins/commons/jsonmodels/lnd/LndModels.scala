package org.bitcoins.commons.jsonmodels.lnd

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.LnTag.PaymentHashTag
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

sealed abstract class LndModel

case class AddInvoiceResult(
    rHash: PaymentHashTag,
    invoice: LnInvoice,
    addIndex: UInt64,
    paymentAddr: ByteVector)
    extends LndModel

case class UTXOResult(
    address: BitcoinAddress,
    amount: CurrencyUnit,
    spk: ScriptPubKey,
    outPointOpt: Option[TransactionOutPoint],
    confirmations: Long
) extends LndModel

case class WalletBalances(
    balance: CurrencyUnit,
    unconfirmedBalance: CurrencyUnit,
    confirmedBalance: CurrencyUnit
) extends LndModel

case class ChannelBalances(
    localBalance: CurrencyUnit,
    remoteBalance: CurrencyUnit,
    unsettledLocalBalance: CurrencyUnit,
    unsettledRemoteBalance: CurrencyUnit,
    pendingOpenLocalBalance: CurrencyUnit,
    pendingOpenRemoteBalance: CurrencyUnit
) extends LndModel

case class OutputDetails(
    address: BitcoinAddress,
    spk: ScriptPubKey,
    outputIndex: Long,
    amount: CurrencyUnit,
    isOurAddress: Boolean
) extends LndModel

case class TxDetails(
    txId: DoubleSha256DigestBE,
    amount: CurrencyUnit,
    numConfirmations: Int,
    blockHashOpt: Option[DoubleSha256DigestBE],
    blockHeight: Int,
    timeStamp: Long,
    totalFees: CurrencyUnit,
    outputDetails: Vector[OutputDetails],
    tx: Transaction,
    label: String
) extends LndModel

case class UTXOLease(
    id: ByteVector,
    outPoint: TransactionOutPoint,
    expiration: Long
) extends LndModel
