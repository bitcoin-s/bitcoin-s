package org.bitcoins.dlc

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.Transaction

case class SetupDLC(
    fundingTx: Transaction,
    cetWin: Transaction,
    cetWinWitness: P2WSHWitnessV0,
    cetLose: Transaction,
    cetLoseWitness: P2WSHWitnessV0,
    cetWinRemoteTxid: DoubleSha256DigestBE,
    cetWinRemoteWitness: P2WSHWitnessV0,
    cetLoseRemoteTxid: DoubleSha256DigestBE,
    cetLoseRemoteWitness: P2WSHWitnessV0,
    refundTx: Transaction)
