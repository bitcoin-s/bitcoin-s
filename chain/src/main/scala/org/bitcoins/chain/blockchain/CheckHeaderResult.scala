package org.bitcoins.chain.blockchain

import org.bitcoins.chain.validation.TipUpdateResult

case class CheckHeaderResult(result: TipUpdateResult, chain: Blockchain)
