package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCMutualCloseSig,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import ujson._

import scala.util.{Failure, Success, Try}

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

  def toJson: Obj
}

/** All states other than Offered contain an accept message. */
sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
}

object DLCStatus {

  def fromJson(json: Value): DLCStatus = {
    val statusT = json.obj("status").str match {
      case "OFFERED"          => Offered.fromJson(json)
      case "ACCEPTED"         => Accepted.fromJson(json)
      case "SIGNED"           => Signed.fromJson(json)
      case "BROADCASTED"      => Broadcasted.fromJson(json)
      case "CONFIRMED"        => Confirmed.fromJson(json)
      case "CLOSE OFFERED"    => CloseOffered.fromJson(json)
      case "CLOSED"           => Closed.fromJson(json)
      case "CLAIMING"         => Claiming.fromJson(json)
      case "CLAIMED"          => Claimed.fromJson(json)
      case "PENALIZED"        => Penalized.fromJson(json)
      case "REMOTE CLAIMING"  => RemoteClaiming.fromJson(json)
      case "REMOTE CLAIMED"   => RemoteClaimed.fromJson(json)
      case "REMOTE PENALIZED" => RemotePenalized.fromJson(json)
      case "REFUNDED"         => Refunded.fromJson(json)
      case _: String =>
        Failure(new RuntimeException(s"No status field found: $json"))
    }

    statusT match {
      case Success(status) => status
      case Failure(err)    => throw err
    }
  }

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  case class Offered(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {
    override val statusString: String = "OFFERED"

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson
      )

    def toAccepted(accept: DLCAccept): Accepted = {
      Accepted(eventId, isInitiator, offer, accept)
    }
  }

  object Offered {

    def fromJson(json: Value): Try[Offered] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("OFFERED"))) {
        val offeredOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
        } yield {
          Offered(eventId, isInitiator, offer)
        }

        offeredOpt match {
          case None          => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(offered) => Success(offered)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson
      )

    def toSigned(sign: DLCSign): Signed = {
      Signed(eventId, isInitiator, offer, accept, sign)
    }
  }

  object Accepted {

    def fromJson(json: Value): Try[Accepted] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("ACCEPTED"))) {
        val acceptedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
        } yield {
          Accepted(eventId, isInitiator, offer, accept)
        }

        acceptedOpt match {
          case None           => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(accepted) => Success(accepted)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson
      )

    def toBroadcasted(fundingTx: Transaction): Broadcasted = {
      Broadcasted(eventId, isInitiator, offer, accept, sign, fundingTx)
    }

    def toConfirmed(fundingTx: Transaction): Confirmed = {
      toBroadcasted(fundingTx).toConfirmed
    }
  }

  object Signed {

    def fromJson(json: Value): Try[Signed] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("SIGNED"))) {
        val signedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
        } yield {
          Signed(eventId, isInitiator, offer, accept, sign)
        }

        signedOpt match {
          case None         => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(signed) => Success(signed)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex
      )

    def toConfirmed: Confirmed = {
      Confirmed(eventId, isInitiator, offer, accept, sign, fundingTx)
    }
  }

  object Broadcasted {

    def fromJson(json: Value): Try[Broadcasted] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("BROADCASTED"))) {
        val broadcastedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          Broadcasted(eventId, isInitiator, offer, accept, sign, fundingTx)
        }

        broadcastedOpt match {
          case None              => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(broadcasted) => Success(broadcasted)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex
      )

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

    def toRefunded(refundTx: Transaction): Refunded = {
      Refunded(eventId, isInitiator, offer, accept, sign, fundingTx, refundTx)
    }
  }

  object Confirmed {

    def fromJson(json: Value): Try[Confirmed] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("CONFIRMED"))) {
        val confirmedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          Confirmed(eventId, isInitiator, offer, accept, sign, fundingTx)
        }

        confirmedOpt match {
          case None            => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(confirmed) => Success(confirmed)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "closeSig" -> closeSig.toJson
      )

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
  }

  object CloseOffered {

    def fromJson(json: Value): Try[CloseOffered] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("CLOSE OFFERED"))) {
        val closeOfferedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          closeSig <- json.obj.get("closeSig").map(DLCMutualCloseSig.fromJson)
        } yield {
          CloseOffered(eventId,
                       isInitiator,
                       offer,
                       accept,
                       sign,
                       fundingTx,
                       closeSig)
        }

        closeOfferedOpt match {
          case None               => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(closeOffered) => Success(closeOffered)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "closeSig" -> closeSig.toJson,
        "closeTx" -> closeTx.hex
      )
  }

  object Closed {

    def fromJson(json: Value): Try[Closed] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("CLOSED"))) {
        val closedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          closeSig <- json.obj.get("closeSig").map(DLCMutualCloseSig.fromJson)
          closeTx <-
            json.obj
              .get("closeTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          Closed(eventId,
                 isInitiator,
                 offer,
                 accept,
                 sign,
                 fundingTx,
                 closeSig,
                 closeTx)
        }

        closedOpt match {
          case None         => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(closed) => Success(closed)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "oracleSig" -> oracleSig.hex,
        "cet" -> cet.hex
      )

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

  object Claiming {

    def fromJson(json: Value): Try[Claiming] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("CLAIMING"))) {
        val claimingOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          oracleSig <-
            json.obj
              .get("oracleSig")
              .flatMap(_.strOpt)
              .map(SchnorrDigitalSignature.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
        } yield {
          Claiming(eventId,
                   isInitiator,
                   offer,
                   accept,
                   sign,
                   fundingTx,
                   oracleSig,
                   cet)
        }

        claimingOpt match {
          case None           => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(claiming) => Success(claiming)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "oracleSig" -> oracleSig.hex,
        "cet" -> cet.hex,
        "closingTx" -> closingTx.hex
      )
  }

  object Claimed {

    def fromJson(json: Value): Try[Claimed] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("CLAIMED"))) {
        val claimedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          oracleSig <-
            json.obj
              .get("oracleSig")
              .flatMap(_.strOpt)
              .map(SchnorrDigitalSignature.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
          closingTx <-
            json.obj
              .get("closingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
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

        claimedOpt match {
          case None          => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(claimed) => Success(claimed)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "oracleSig" -> oracleSig.hex,
        "cet" -> cet.hex,
        "penaltyTx" -> penaltyTx.hex
      )
  }

  object Penalized {

    def fromJson(json: Value): Try[Penalized] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("PENALIZED"))) {
        val penalizedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          oracleSig <-
            json.obj
              .get("oracleSig")
              .flatMap(_.strOpt)
              .map(SchnorrDigitalSignature.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
          penaltyTx <-
            json.obj
              .get("penaltyTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
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

        penalizedOpt match {
          case None            => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(penalized) => Success(penalized)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "cet" -> cet.hex
      )

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

  object RemoteClaiming {

    def fromJson(json: Value): Try[RemoteClaiming] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (
        json.obj
          .get("status")
          .contains(ujson.Str("REMOTE CLAIMING"))
      ) {
        val remoteClaimingOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
        } yield {
          RemoteClaiming(eventId,
                         isInitiator,
                         offer,
                         accept,
                         sign,
                         fundingTx,
                         cet)
        }

        remoteClaimingOpt match {
          case None           => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(claiming) => Success(claiming)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "cet" -> cet.hex,
        "closingTx" -> closingTx.hex
      )
  }

  object RemoteClaimed {

    def fromJson(json: Value): Try[RemoteClaimed] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("REMOTE CLAIMED"))) {
        val remoteClaimedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
          closingTx <-
            json.obj
              .get("closingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          RemoteClaimed(eventId,
                        isInitiator,
                        offer,
                        accept,
                        sign,
                        fundingTx,
                        cet,
                        closingTx)
        }

        remoteClaimedOpt match {
          case None          => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(claimed) => Success(claimed)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "cet" -> cet.hex,
        "penaltyTx" -> penaltyTx.hex
      )
  }

  object RemotePenalized {

    def fromJson(json: Value): Try[RemotePenalized] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (
        json.obj
          .get("status")
          .contains(ujson.Str("REMOTE PENALIZED"))
      ) {
        val remotePenalizedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          cet <- json.obj.get("cet").flatMap(_.strOpt).map(Transaction.fromHex)
          penaltyTx <-
            json.obj
              .get("penaltyTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          RemotePenalized(eventId,
                          isInitiator,
                          offer,
                          accept,
                          sign,
                          fundingTx,
                          cet,
                          penaltyTx)
        }

        remotePenalizedOpt match {
          case None            => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(penalized) => Success(penalized)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
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

    override def toJson: Obj =
      Obj(
        "status" -> statusString,
        "eventId" -> eventId.hex,
        "isInitiator" -> isInitiator,
        "offer" -> offer.toJson,
        "accept" -> accept.toJson,
        "sign" -> sign.toJson,
        "fundingTx" -> fundingTx.hex,
        "refundTx" -> refundTx.hex
      )
  }

  object Refunded {

    def fromJson(json: Value): Try[Refunded] = {
      if (json.objOpt.isEmpty) {
        Failure(new IllegalArgumentException(s"$json was not an Obj"))
      } else if (json.obj.get("status").contains(ujson.Str("REFUNDED"))) {
        val refundedOpt = for {
          eventId <-
            json.obj
              .get("eventId")
              .flatMap(_.strOpt)
              .map(Sha256DigestBE.fromHex)
          isInitiator <- json.obj.get("isInitiator").flatMap(_.boolOpt)
          offer <- json.obj.get("offer").map(DLCOffer.fromJson)
          accept <- json.obj.get("accept").map(DLCAccept.fromJson)
          sign <- json.obj.get("sign").map(DLCSign.fromJson)
          fundingTx <-
            json.obj
              .get("fundingTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
          refundTx <-
            json.obj
              .get("refundTx")
              .flatMap(_.strOpt)
              .map(Transaction.fromHex)
        } yield {
          Refunded(eventId,
                   isInitiator,
                   offer,
                   accept,
                   sign,
                   fundingTx,
                   refundTx)
        }

        refundedOpt match {
          case None           => Failure(new RuntimeException(s"Failed to parse $json"))
          case Some(refunded) => Success(refunded)
        }
      } else {
        Failure(
          new IllegalArgumentException(
            s"Invalid status: ${json.obj.get("status")}"))
      }
    }
  }
}
