package org.bitcoins.core.protocol.ptlc

import org.bitcoins.core.crypto.{
  ECAdaptorSignature,
  ECPublicKey,
  Sha256DigestBE
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

sealed trait PTLCMessage {
  val invoiceId: Sha256DigestBE
}

object PTLCMessage {

  def calcInvoiceId(invoice: PTLCInvoice): Sha256DigestBE = {
    val dataToHash = invoice.adaptorPoint.bytes ++
      invoice.amount.satoshis.bytes ++
      invoice.pubkey.bytes ++
      invoice.finalAddress.scriptPubKey.bytes ++
      invoice.timeout.bytes

    CryptoUtil
      .sha256(dataToHash)
      .flip
  }

  case class PTLCInvoice(
      adaptorPoint: ECPublicKey,
      amount: CurrencyUnit,
      pubkey: ECPublicKey,
      finalAddress: BitcoinAddress,
      timeout: UInt32)
      extends PTLCMessage {
    val invoiceId: Sha256DigestBE = calcInvoiceId(this)
  }

  case class PTLCAccept(
      pubkey: ECPublicKey,
      unsignedTx: Transaction,
      adaptorSignature: ECAdaptorSignature,
      refundAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      invoiceId: Sha256DigestBE)
      extends PTLCMessage {
    lazy val changeSPK: ScriptPubKey = unsignedTx.outputs(1).scriptPubKey
  }

  case class PTLCRefundSignature(
      refundSignature: PartialSignature,
      invoiceId: Sha256DigestBE)
      extends PTLCMessage
}
