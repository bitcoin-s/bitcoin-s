package org.bitcoins.dlc.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  MultiNonceContractInfo,
  SingleNonceContractInfo
}
import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.commons.jsonmodels.dlc.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.tlv.{EnumOutcome, UnsignedNumericOutcome}
import org.bitcoins.crypto._
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

class DLCMultiNonceExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, multiNonce = true)
  }

  behavior of "DLCWallet"

  def getSigs(contractInfo: ContractInfo): (
      Vector[SchnorrDigitalSignature],
      Vector[SchnorrDigitalSignature]) = {
    val multiNonceContractInfo: MultiNonceContractInfo = contractInfo match {
      case info: MultiNonceContractInfo => info
      case _: SingleNonceContractInfo =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    val initiatorWinVec =
      multiNonceContractInfo.outcomeVec
        .maxBy(_._2.toLong)
        ._1

    val kValues = DLCWalletUtil.kValues.take(initiatorWinVec.size)

    val initiatorWinSigs = initiatorWinVec.zip(kValues).map {
      case (num, kValue) =>
        DLCWalletUtil.oraclePrivKey
          .schnorrSignWithNonce(CryptoUtil.sha256(num.toString).bytes, kValue)
    }

    val recipientWinVec =
      multiNonceContractInfo.outcomeVec.find(_._2 == Satoshis.zero).get._1

    val kValues2 = DLCWalletUtil.kValues.take(recipientWinVec.size)

    val recipientWinSigs = recipientWinVec.zip(kValues2).map {
      case (num, kValue) =>
        DLCWalletUtil.oraclePrivKey
          .schnorrSignWithNonce(CryptoUtil.sha256(num.toString).bytes, kValue)
    }

    (initiatorWinSigs, recipientWinSigs)
  }

  it must "execute as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      offer <- getInitialOffer(wallets._1.wallet)
      (sigs, _) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sigs)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
            verifyingMatchingOracleSigs(statusA, statusB)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Claimed)
          assert(dlcB.state == DLCState.RemoteClaimed)
        case (_, _) => fail()
      }
    }
  }

  it must "execute as the recipient" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      offer <- getInitialOffer(wallets._2.wallet)
      (_, sigs) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sigs)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: RemoteClaimed), Some(statusB: Claimed)) =>
            verifyingMatchingOracleSigs(statusB, statusA)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.RemoteClaimed)
          assert(dlcB.state == DLCState.Claimed)
        case (_, _) => fail()
      }
    }
  }

  private def verifyingMatchingOracleSigs(
      statusA: Claimed,
      statusB: RemoteClaimed): Boolean = {
    val outcome = statusB.outcome
    val numSigs = outcome match {
      case EnumOutcome(outcome) =>
        throw new RuntimeException(s"Unexpected outcome type, got $outcome")
      case UnsignedNumericOutcome(digits) => digits.size
    }

    val aggR = statusA.oracleSigs
      .take(numSigs)
      .map(_.rx.publicKey)
      .reduce(_.add(_))
      .schnorrNonce

    val aggS = statusA.oracleSigs
      .take(numSigs)
      .map(_.sig)
      .reduce(_.add(_))

    val aggregateSignature =
      SchnorrDigitalSignature(aggR, aggS)
    aggregateSignature == statusB.oracleSig
  }
}
