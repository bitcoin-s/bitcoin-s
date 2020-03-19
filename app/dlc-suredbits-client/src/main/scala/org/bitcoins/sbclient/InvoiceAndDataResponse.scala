package org.bitcoins.sbclient

import org.bitcoins.core.protocol.ln.LnInvoice

case class InvoiceAndDataResponse(invoice: LnInvoice, encryptedData: String)
