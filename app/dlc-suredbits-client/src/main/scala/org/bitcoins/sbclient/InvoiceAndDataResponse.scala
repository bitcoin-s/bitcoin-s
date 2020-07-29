package org.bitcoins.sbclient

import org.bitcoins.core.protocol.ln.LnInvoice

case class InvoiceAndDataResponse(invoice: LnInvoice, encryptedData: String) {

  def toJsonString: String = {
    s"""{"invoice":"${invoice.toString}", "encryptedData":"$encryptedData"}"""
  }
}
