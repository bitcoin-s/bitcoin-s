package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

class DLCNumericExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, DLCWalletUtil.multiNonceContractOraclePair)
  }

  behavior of "DLCWallet"

  def getSigs(contractInfo: ContractInfo): (
      OracleAttestmentTLV,
      OracleAttestmentTLV) = {
    contractInfo.contractDescriptor match {
      case _: NumericContractDescriptor => ()
      case _: EnumContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    val oracleInfo = DLCWalletUtil.multiNonceContractInfo.oracleInfo
      .asInstanceOf[NumericSingleOracleInfo]

    val initiatorWinVec =
      contractInfo.allOutcomesAndPayouts
        .maxBy(_._2.toLong)
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]
        .digits
        .padTo(oracleInfo.nonces.size, 0)

    val kValues = DLCWalletUtil.kValues.take(initiatorWinVec.size)

    val initiatorWinSigs =
      initiatorWinVec.zip(kValues).map { case (num, kValue) =>
        DLCWalletUtil.oraclePrivKey
          .schnorrSignWithNonce(CryptoUtil
                                  .sha256DLCAttestation(num.toString)
                                  .bytes,
                                kValue)
      }

    val recipientWinVec =
      contractInfo.allOutcomesAndPayouts
        .find(_._2 == Satoshis.zero)
        .get
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]
        .digits
        .padTo(oracleInfo.nonces.size, 0)

    val kValues2 = DLCWalletUtil.kValues.take(recipientWinVec.size)

    val recipientWinSigs =
      recipientWinVec.zip(kValues2).map { case (num, kValue) =>
        DLCWalletUtil.oraclePrivKey
          .schnorrSignWithNonce(CryptoUtil
                                  .sha256DLCAttestation(num.toString)
                                  .bytes,
                                kValue)
      }

    val publicKey = DLCWalletUtil.oraclePrivKey.schnorrPublicKey
    val eventId = DLCWalletUtil.sampleOracleInfo.announcement.eventTLV match {
      case v0: OracleEventV0TLV => v0.eventId
    }

    (OracleAttestmentV0TLV(eventId,
                           publicKey,
                           initiatorWinSigs,
                           initiatorWinVec.map(_.toString)),
     OracleAttestmentV0TLV(eventId,
                           publicKey,
                           recipientWinSigs,
                           recipientWinVec.map(_.toString)))
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
            assert(verifyingMatchingOracleSigs(statusA, statusB))
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
            assert(verifyingMatchingOracleSigs(statusB, statusA))
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
    val outcome = statusB.oracleOutcome
    outcome match {
      case _: EnumOracleOutcome =>
        throw new RuntimeException(s"Unexpected outcome type, got $outcome")
      case numeric: NumericOracleOutcome =>
        val aggR = numeric.aggregateNonce

        val neededNonces = numeric.oraclesAndOutcomes.flatMap {
          case (oracle, outcome) =>
            oracle.nonces.take(outcome.serialized.length)
        }

        val aggS = statusA.oracleSigs
          .filter(sig => neededNonces.contains(sig.rx))
          .map(_.sig)
          .reduce(_.add(_))

        val aggregateSignature =
          SchnorrDigitalSignature(aggR, aggS)
        aggregateSignature == statusB.oracleSig
    }
  }
}
