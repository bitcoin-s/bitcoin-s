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
  def statusString: String
}

sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
}

object DLCStatus {

  case class Offered(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {
    override val statusString: String = "OFFERED"

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
    override val statusString: String = "ACCEPTED"

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
    override val statusString: String = "SIGNED"

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
    override val statusString: String = "BROADCASTED"

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
    override val statusString: String = "CONFIRMED"

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
    override val statusString: String = "CLOSE OFFERED"

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
      extends AcceptedDLCStatus {
    override val statusString: String = "CLOSED"
  }

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
    override val statusString: String = "CLAIMING"

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
      extends AcceptedDLCStatus {
    override val statusString: String = "CLAIMED"
  }

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
      extends AcceptedDLCStatus {
    override val statusString: String = "PENALIZED"
  }

  case class RemoteClaiming(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "REMOTE CLAIMING"

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
      extends AcceptedDLCStatus {
    override val statusString: String = "REMOTE CLAIMED"
  }

  case class RemotePenalized(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction,
      penaltyTx: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "REMOTE PENALIZED"
  }

  case class Refunded(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      refundTx: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "REFUNDED"
  }
}
