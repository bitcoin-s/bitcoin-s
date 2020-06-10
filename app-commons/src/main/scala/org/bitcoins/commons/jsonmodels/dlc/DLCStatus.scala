package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCMutualCloseSig,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}

/** Represents the state of specific DLC for a given party.
  * This state is made up of all messages that have been
  * passed back and forth as well as any relevant on-chain
  * transactions and oracle signatures.
  */
sealed trait DLCStatus {
  def eventId: Sha256DigestBE
  def isInitiator: Boolean
  def offer: DLCOffer
  def statusString: String
}

/** All states other than Offered contain an accept message. */
sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
}

object DLCStatus {

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
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

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
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

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
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

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
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

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
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

  /** The state where a party has initiated a mutual close
    * with a DLCMutualCloseSig message (containing an
    * oracle signature and their signature of the close tx)
    * but the mutual close tx has not yet been broadcasted.
    */
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

  /** The state where a mutual close has been successfully
    * executed with an on-chain close transaction.
    */
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

  /** The state where this party has initiated a unilateral
    * execution by broadcasting a CET to the network. In this
    * state the to_local output on the CET has not yet been spent.
    */
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

  /** The state where this party has successfully executed
    * a unilateral execution by broadcasting a CET and successfully
    * spending the to_local output of that CET.
    */
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

  /** The state where this party has broadcasted a CET
    * to the network and failed to spend the to_local output
    * in time, so that the counterparty has successfully
    * spent the to_local output to themselves with a
    * penalty transaction.
    */
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

  /** The state where the remote party has initiated a
    * unilateral execution by broadcasting one of their
    * CETs but has not yet spent their to_local output
    */
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

  /** The state where the remote party has successfully
    * executed a unilateral execution by broadcasting both
    * a CET and successfully spending their to_local output.
    */
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

  /** The state where the remote party has published a
    * CET but failed to spend their to_local output
    * and this party has successfully spent that to_local
    * output with a penalty transaction.
    */
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

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
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
