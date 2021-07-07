package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.tlv.ContractDescriptorTLV

case class ContractTemplateDb(
    label: String,
    contractDescriptorTLV: ContractDescriptorTLV,
    totalCollateral: CurrencyUnit
)
