package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedSchnorrSignatures
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
    contractInfo.contractDescriptors.head match {
      case _: NumericContractDescriptor => ()
      case _: EnumContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    val oracleInfo = DLCWalletUtil.multiNonceContractInfo.oracleInfos.head
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
                           OrderedSchnorrSignatures(initiatorWinSigs).toVector,
                           initiatorWinVec.map(_.toString)),
     OracleAttestmentV0TLV(eventId,
                           publicKey,
                           OrderedSchnorrSignatures(recipientWinSigs).toVector,
                           recipientWinVec.map(_.toString)))
  }

  it must "execute as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      (sigs, _) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sigs).map(_.get)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

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
      status <- getDLCStatus(wallets._2.wallet)
      (_, sigs) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sigs).map(_.get)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

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

  it must "throw an exception for a numeric contract when do not have all the oracle signatures/outcomes" in {
    wallets =>
      val resultF = for {
        contractId <- getContractId(wallets._1.wallet)
        status <- getDLCStatus(wallets._2.wallet)
        (_, goodAttestment) = getSigs(status.contractInfo)
        //purposefully drop these
        //we cannot drop just a sig, or just an outcome because
        //of invariants in OracleAttestmentV0TLV
        badSigs = OrderedSchnorrSignatures.fromUnsorted(
          goodAttestment.sigs.dropRight(1).toVector)
        badOutcomes = goodAttestment.outcomes.dropRight(1)
        badAttestment = OracleAttestmentV0TLV(eventId = goodAttestment.eventId,
                                              publicKey =
                                                goodAttestment.publicKey,
                                              unsortedSignatures =
                                                badSigs.toVector,
                                              outcomes = badOutcomes)
        func = (wallet: DLCWallet) =>
          wallet.executeDLC(contractId, badAttestment).map(_.get)

        result <- dlcExecutionTest(wallets = wallets,
                                   asInitiator = false,
                                   func = func,
                                   expectedOutputs = 1)
      } yield assert(result)

      recoverToSucceededIf[IllegalArgumentException](resultF)
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
