package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

/** Represents the state of specific DLC for a given party.
  * This state is made up of all messages that have been
  * passed back and forth as well as any relevant on-chain
  * transactions and oracle signatures.
  */
sealed trait DLCStatus {
  def paramHash: Sha256DigestBE
  def isInitiator: Boolean
  def offer: DLCOffer
  def state: DLCState
  lazy val tempContractId: Sha256Digest = offer.tempContractId
  lazy val statusString: String = state.toString

  def toJson: Value
}

/** All states other than Offered contain an accept message. */
sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
  // TODO: add contractId when we can calc, currently cannot because app-commons doesn't depend on DLC
}

sealed trait SignedDLCStatus extends AcceptedDLCStatus {
  def sign: DLCSign
  val contractId: ByteVector = sign.contractId
}

sealed trait BroadcastedDLCStatus extends SignedDLCStatus {
  def fundingTx: Transaction
}

sealed trait ClosedDLCStatus extends BroadcastedDLCStatus {
  def closingTx: Transaction
}

object DLCStatus {

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  case class Offered(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {
    override val state: DLCState = DLCState.Offered

    def toAccepted(accept: DLCAccept): Accepted = {
      Accepted(paramHash, isInitiator, offer, accept)
    }

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex)
      )
  }

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
  case class Accepted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept)
      extends AcceptedDLCStatus {
    override val state: DLCState = DLCState.Accepted

    def toSigned(sign: DLCSign): Signed = {
      Signed(paramHash, isInitiator, offer, accept, sign)
    }

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex)
      )
  }

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
  case class Signed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign)
      extends SignedDLCStatus {
    override val state: DLCState = DLCState.Signed

    def toBroadcasted(fundingTx: Transaction): Broadcasted = {
      Broadcasted(paramHash, isInitiator, offer, accept, sign, fundingTx)
    }

    def toConfirmed(fundingTx: Transaction): Confirmed = {
      toBroadcasted(fundingTx).toConfirmed
    }

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex)
      )
  }

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
  case class Broadcasted(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends BroadcastedDLCStatus {
    override val state: DLCState = DLCState.Broadcasted

    def toConfirmed: Confirmed = {
      Confirmed(paramHash, isInitiator, offer, accept, sign, fundingTx)
    }

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex),
        "fundingTxId" -> Str(fundingTx.txIdBE.hex),
        "fundingTx" -> Str(fundingTx.hex)
      )
  }

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
  case class Confirmed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends BroadcastedDLCStatus {
    override val state: DLCState = DLCState.Confirmed

    def toClaimed(
        oracleSigs: Vector[SchnorrDigitalSignature],
        cet: Transaction): Claimed = {
      Claimed(paramHash,
              isInitiator,
              offer,
              accept,
              sign,
              fundingTx,
              oracleSigs,
              cet)
    }

    def toRemoteClaimed(cet: Transaction): RemoteClaimed = {
      RemoteClaimed(paramHash, isInitiator, offer, accept, sign, fundingTx, cet)
    }

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex),
        "fundingTxId" -> Str(fundingTx.txIdBE.hex),
        "fundingTx" -> Str(fundingTx.hex)
      )
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by ourselves.
    */
  case class Claimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSigs: Vector[SchnorrDigitalSignature],
      cet: Transaction)
      extends ClosedDLCStatus {
    override val state: DLCState = DLCState.Claimed

    val outcome: DLCOutcomeType = {
      offer.oracleAndContractInfo.findOutcome(oracleSigs) match {
        case Some(outcome) => outcome
        case None =>
          throw new IllegalArgumentException(
            s"No outcome found for signatures: $oracleSigs")
      }
    }

    override lazy val toJson: Value = {
      val outcomeJs = outcome match {
        case EnumOutcome(outcome) =>
          Str(outcome)
        case UnsignedNumericOutcome(digits) =>
          Arr.from(digits.map(num => Num(num)))
      }

      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex),
        "fundingTxId" -> Str(fundingTx.txIdBE.hex),
        "fundingTx" -> Str(fundingTx.hex),
        "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
        "outcome" -> outcomeJs,
        "cetTxId" -> Str(cet.txIdBE.hex),
        "cet" -> Str(cet.hex)
      )
    }

    override lazy val closingTx: Transaction = cet
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by a remote party.
    */
  case class RemoteClaimed(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction)
      extends ClosedDLCStatus {
    override val state: DLCState = DLCState.RemoteClaimed

    /** This represents the sum of all oracle signatures in the case that there are multiple */
    val (oracleSig: SchnorrDigitalSignature, outcome: DLCOutcomeType) = {
      val cetSigs = cet
        .asInstanceOf[WitnessTransaction]
        .witness
        .head
        .asInstanceOf[P2WSHWitnessV0]
        .signatures

      require(cetSigs.size == 2, "There must be only 2 signatures")

      val oraclePubKey = offer.oracleInfo.pubKey
      val rVals = offer.oracleInfo.nonces

      def aggregateR(numSigs: Int): SchnorrNonce = {
        rVals.take(numSigs).map(_.publicKey).reduce(_.add(_)).schnorrNonce
      }

      def sigFromMsgAndSigs(
          outcome: DLCOutcomeType,
          adaptorSig: ECAdaptorSignature,
          cetSig: ECDigitalSignature): SchnorrDigitalSignature = {
        val (sigPubKey, numSigs) = outcome match {
          case EnumOutcome(outcome) =>
            val sigPoint = oraclePubKey.computeSigPoint(
              CryptoUtil.sha256(outcome).bytes,
              aggregateR(1))

            (sigPoint, 1)
          case UnsignedNumericOutcome(digits) =>
            val sigPoint = digits
              .zip(rVals.take(digits.length))
              .map {
                case (digit, nonce) =>
                  oraclePubKey.computeSigPoint(
                    CryptoUtil.sha256(digit.toString).bytes,
                    nonce)
              }
              .reduce(_.add(_))

            (sigPoint, digits.length)
        }

        val possibleOracleS =
          sigPubKey
            .extractAdaptorSecret(adaptorSig,
                                  ECDigitalSignature(cetSig.bytes.init))
            .fieldElement
        SchnorrDigitalSignature(aggregateR(numSigs), possibleOracleS)
      }

      val outcomeValues = cet.outputs.map(_.value).sorted
      val totalCollateral = offer.totalCollateral + accept.totalCollateral

      val possibleMessages = offer.contractInfo match {
        case DLCMessage.SingleNonceContractInfo(outcomeValueMap) =>
          outcomeValueMap
            .filter {
              case (_, amt) =>
                Vector(amt, totalCollateral - amt)
                  .filter(_ >= Policy.dustThreshold)
                  .sorted == outcomeValues
            }
            .map(_._1)
        case info: DLCMessage.MultiNonceContractInfo =>
          info.outcomeVec
            .filter {
              case (_, amt) =>
                val amts = Vector(amt, totalCollateral - amt)
                  .filter(_ >= Policy.dustThreshold)
                  .sorted

                Math.abs(
                  (amts.head - outcomeValues.head).satoshis.toLong) <= 1 && Math
                  .abs((amts.last - outcomeValues.last).satoshis.toLong) <= 1
            }
            .map { case (digits, _) => UnsignedNumericOutcome(digits) }
      }

      val (offerCETSig, acceptCETSig) =
        if (
          offer.pubKeys.fundingKey.hex.compareTo(
            accept.pubKeys.fundingKey.hex) > 0
        ) {
          (cetSigs.last, cetSigs.head)
        } else {
          (cetSigs.head, cetSigs.last)
        }

      val (cetSig, outcomeSigs) = if (isInitiator) {
        val possibleOutcomeSigs = sign.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.contains(msg)
        }
        (acceptCETSig, possibleOutcomeSigs)
      } else {
        val possibleOutcomeSigs = accept.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.contains(msg)
        }
        (offerCETSig, possibleOutcomeSigs)
      }

      val sigOpt = outcomeSigs.find {
        case (outcome, adaptorSig) =>
          val possibleOracleSig = sigFromMsgAndSigs(outcome, adaptorSig, cetSig)
          val sigPoint = offer.oracleAndContractInfo.sigPointForOutcome(outcome)
          possibleOracleSig.sig.getPublicKey == sigPoint
      }

      sigOpt match {
        case Some((msg, adaptorSig)) =>
          (sigFromMsgAndSigs(msg, adaptorSig, cetSig), msg)
        case None =>
          throw new IllegalArgumentException(
            "No Oracle Signature found from CET")
      }
    }

    override lazy val toJson: Value = {

      val outcomeJs = outcome match {
        case EnumOutcome(outcome) =>
          Str(outcome)
        case UnsignedNumericOutcome(digits) =>
          Arr.from(digits.map(num => Num(num)))
      }

      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex),
        "fundingTxId" -> Str(fundingTx.txIdBE.hex),
        "fundingTx" -> Str(fundingTx.hex),
        "oracleSig" -> Str(oracleSig.hex),
        "outcome" -> outcomeJs,
        "cetTxId" -> Str(cet.txIdBE.hex),
        "cet" -> Str(cet.hex)
      )
    }

    override lazy val closingTx: Transaction = cet
  }

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
  case class Refunded(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      refundTx: Transaction)
      extends ClosedDLCStatus {
    override val state: DLCState = DLCState.Refunded

    override lazy val toJson: Value =
      Obj(
        "state" -> Str(state.toString),
        "paramHash" -> Str(paramHash.hex),
        "isInitiator" -> Bool(isInitiator),
        "offer" -> Str(offer.toMessage.hex),
        "accept" -> Str(accept.toMessage.hex),
        "sign" -> Str(sign.toMessage.hex),
        "fundingTxId" -> Str(fundingTx.txIdBE.hex),
        "fundingTx" -> Str(fundingTx.hex),
        "refundTxId" -> Str(refundTx.txIdBE.hex),
        "refundTx" -> Str(refundTx.hex)
      )

    override lazy val closingTx: Transaction = refundTx
  }

  def fromJson(json: Value): DLCStatus = {
    val obj = json.obj

    val state = DLCState.fromString(obj("state").str)
    val paramHash = Sha256DigestBE(obj("paramHash").str)
    val isInitiator = obj("isInitiator").bool
    val offer = DLCOffer.fromMessage(
      LnMessageFactory(DLCOfferTLV).fromHex(obj("offer").str))

    lazy val accept = DLCAccept.fromMessage(
      LnMessageFactory(DLCAcceptTLV).fromHex(obj("accept").str),
      offer)
    lazy val sign = DLCSign.fromMessage(
      LnMessageFactory(DLCSignTLV).fromHex(obj("sign").str),
      offer)
    lazy val fundingTx = Transaction(obj("fundingTx").str)
    lazy val cet = Transaction(obj("cet").str)
    lazy val refundTx = Transaction(obj("refundTx").str)
    lazy val oracleSigs =
      obj("oracleSigs").arr
        .map(value => SchnorrDigitalSignature(value.str))
        .toVector

    state match {
      case DLCState.Offered =>
        Offered(paramHash, isInitiator, offer)
      case DLCState.Accepted =>
        Accepted(paramHash, isInitiator, offer, accept)
      case DLCState.Signed =>
        Signed(paramHash, isInitiator, offer, accept, sign)
      case DLCState.Broadcasted =>
        Broadcasted(paramHash, isInitiator, offer, accept, sign, fundingTx)
      case DLCState.Confirmed =>
        Confirmed(paramHash, isInitiator, offer, accept, sign, fundingTx)
      case DLCState.Claimed =>
        Claimed(paramHash,
                isInitiator,
                offer,
                accept,
                sign,
                fundingTx,
                oracleSigs,
                cet)
      case DLCState.RemoteClaimed =>
        RemoteClaimed(paramHash,
                      isInitiator,
                      offer,
                      accept,
                      sign,
                      fundingTx,
                      cet)
      case DLCState.Refunded =>
        Refunded(paramHash,
                 isInitiator,
                 offer,
                 accept,
                 sign,
                 fundingTx,
                 refundTx)
    }
  }

  def getContractId(status: DLCStatus): Option[ByteVector] = {
    status match {
      case status: SignedDLCStatus =>
        Some(status.contractId)
      case _: Offered | _: Accepted =>
        None
    }
  }

  def getFundingTx(status: DLCStatus): Option[Transaction] = {
    status match {
      case status: BroadcastedDLCStatus =>
        Some(status.fundingTx)
      case _: Offered | _: Accepted | _: Signed =>
        None
    }
  }

  def getClosingTx(status: DLCStatus): Option[Transaction] = {
    status match {
      case status: ClosedDLCStatus =>
        Some(status.closingTx)
      case _: Offered | _: Accepted | _: Signed | _: BroadcastedDLCStatus =>
        None
    }
  }

  def getOracleSignatures(
      status: DLCStatus): Option[Vector[SchnorrDigitalSignature]] = {
    status match {
      case remoteClaimed: RemoteClaimed =>
        Some(Vector(remoteClaimed.oracleSig))
      case claimed: Claimed =>
        Some(claimed.oracleSigs)
      case _: Offered | _: Accepted | _: Signed | _: BroadcastedDLCStatus |
          _: Refunded =>
        None
    }
  }
}
