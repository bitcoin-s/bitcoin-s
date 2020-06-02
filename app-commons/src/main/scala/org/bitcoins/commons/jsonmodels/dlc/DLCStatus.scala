package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCMutualCloseSig,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}

sealed trait DLCStatus {
  def eventId: Sha256DigestBE
  def isInitiator: Boolean
  def offer: DLCOffer
}

sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
}

object DLCStatus {

  def statusString(status: DLCStatus): String = {
    status match {
      case _: Offered         => "OFFERED"
      case _: Accepted        => "ACCEPTED"
      case _: Signed          => "SIGNED"
      case _: Broadcasted     => "BROADCASTED"
      case _: Confirmed       => "CONFIRMED"
      case _: CloseOffered    => "CLOSE OFFERED"
      case _: Closed          => "CLOSED"
      case _: Claiming        => "CLAIMING"
      case _: Claimed         => "CLAIMED"
      case _: Penalized       => "PENALIZED"
      case _: RemoteClaiming  => "REMOTE CLAIMING"
      case _: RemoteClaimed   => "REMOTE CLAIMED"
      case _: RemotePenalized => "REMOTE PENALIZED"
      case _: Refunded        => "REFUNDED"
    }
  }

  case class Offered(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {

    def toAccepted(accept: DLCAccept): Accepted = {
      Accepted(eventId, isInitiator, offer, accept)
    }
  }

  case class Accepted(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept)
      extends AcceptedDLCStatus {

    def toSigned(sign: DLCSign): Signed = {
      Signed(eventId, isInitiator, offer, accept, sign)
    }
  }

  case class Signed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign)
      extends AcceptedDLCStatus {

    def toBroadcasted(fundingTx: Transaction): Broadcasted = {
      Broadcasted(eventId, isInitiator, offer, accept, sign, fundingTx)
    }

    def toConfirmed(fundingTx: Transaction): Confirmed = {
      toBroadcasted(fundingTx).toConfirmed
    }
  }

  case class Broadcasted(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends AcceptedDLCStatus {

    def toConfirmed: Confirmed = {
      Confirmed(eventId, isInitiator, offer, accept, sign, fundingTx)
    }
  }

  case class Confirmed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends AcceptedDLCStatus {

    def toCloseOffered(closeSig: DLCMutualCloseSig): CloseOffered = {
      CloseOffered(eventId,
                   isInitiator,
                   offer,
                   accept,
                   sign,
                   fundingTx,
                   closeSig)
    }

    def toClaiming(
        oracleSig: SchnorrDigitalSignature,
        cet: Transaction): Claiming = {
      Claiming(eventId,
               isInitiator,
               offer,
               accept,
               sign,
               fundingTx,
               oracleSig,
               cet)
    }

    def toRemoteClaiming(cet: Transaction): RemoteClaiming = {
      RemoteClaiming(eventId, isInitiator, offer, accept, sign, fundingTx, cet)
    }
  }

  case class CloseOffered(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      closeSig: DLCMutualCloseSig)
      extends AcceptedDLCStatus {

    def toClosed(closeTx: Transaction): Closed = {
      Closed(eventId,
             isInitiator,
             offer,
             accept,
             sign,
             fundingTx,
             closeSig,
             closeTx)
    }

    def toRefunded(refundTx: Transaction): Refunded = {
      Refunded(eventId, isInitiator, offer, accept, sign, fundingTx, refundTx)
    }
  }

  case class Closed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      closeSig: DLCMutualCloseSig,
      closeTx: Transaction)
      extends AcceptedDLCStatus

  case class Claiming(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSig: SchnorrDigitalSignature,
      cet: Transaction)
      extends AcceptedDLCStatus {

    def toClaimed(closingTx: Transaction): Claimed = {
      Claimed(eventId,
              isInitiator,
              offer,
              accept,
              sign,
              fundingTx,
              oracleSig,
              cet,
              closingTx)
    }

    def toPenalized(penaltyTx: Transaction): Penalized = {
      Penalized(eventId,
                isInitiator,
                offer,
                accept,
                sign,
                fundingTx,
                oracleSig,
                cet,
                penaltyTx)
    }
  }

  case class Claimed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSig: SchnorrDigitalSignature,
      cet: Transaction,
      closingTx: Transaction)
      extends AcceptedDLCStatus

  case class Penalized(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSig: SchnorrDigitalSignature,
      cet: Transaction,
      penaltyTx: Transaction)
      extends AcceptedDLCStatus

  case class RemoteClaiming(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction)
      extends AcceptedDLCStatus {

    def toRemoteClaimed(closingTx: Transaction): RemoteClaimed = {
      RemoteClaimed(eventId,
                    isInitiator,
                    offer,
                    accept,
                    sign,
                    fundingTx,
                    cet,
                    closingTx)
    }

    def toRemotePenalized(penaltyTx: Transaction): RemotePenalized = {
      RemotePenalized(eventId,
                      isInitiator,
                      offer,
                      accept,
                      sign,
                      fundingTx,
                      cet,
                      penaltyTx)
    }
  }

  case class RemoteClaimed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction,
      closingTx: Transaction)
      extends AcceptedDLCStatus

  case class RemotePenalized(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction,
      penaltyTx: Transaction)
      extends AcceptedDLCStatus

  case class Refunded(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      refundTx: Transaction)
      extends AcceptedDLCStatus
}
