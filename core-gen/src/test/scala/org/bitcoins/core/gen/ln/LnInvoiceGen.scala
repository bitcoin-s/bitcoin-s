package org.bitcoins.core.gen.ln

import org.bitcoins.core.gen._
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.util.NumberUtil
import org.scalacheck.Gen

sealed abstract class LnInvoiceGen {

  /**
   * Generates a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart]]
   * that does not contain a amount
   * @return
   */
  def lnHrpNoAmt: Gen[LnHumanReadablePart] = {
    ChainParamsGenerator.lnNetworkParams.flatMap {
      lnParam =>
        LnHumanReadablePart.fromLnParams(lnParam)
    }
  }

  /**
   * Generates a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart]]
   * with an amount encoded
   */
  def lnHrpAmt: Gen[LnHumanReadablePart] = {
    ChainParamsGenerator.lnNetworkParams.flatMap { lnParam =>
      LnCurrencyUnitGen.realisticLnInvoice.map { lcu =>
        LnHumanReadablePart.fromParamsAmount(
          network = lnParam,
          amount = Some(lcu))
      }
    }
  }

  def lnHrp: Gen[LnHumanReadablePart] = {
    Gen.oneOf(lnHrpAmt, lnHrpNoAmt)
  }

  def paymentHashTag: Gen[LnTag.PaymentHashTag] = {
    CryptoGenerators.sha256Digest.map { hash =>
      LnTag.PaymentHashTag(hash)
    }
  }

  def descriptionTag: Gen[LnTag.DescriptionTag] = {
    StringGenerators.genString.map { description =>
      LnTag.DescriptionTag(description)
    }
  }

  def descriptionHashTag: Gen[LnTag.DescriptionHashTag] = {
    descriptionTag.map { desc =>
      desc.descriptionHashTag
    }
  }

  def descriptionOrDescriptionHashTag: Gen[Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag]] = {
    if (scala.util.Random.nextBoolean()) {
      descriptionTag.map(Left(_))
    } else {
      descriptionHashTag.map(Right(_))
    }
  }

  def expiryTime: Gen[LnTag.ExpiryTimeTag] = {
    NumberGenerator.uInt32s.map { u32 =>
      LnTag.ExpiryTimeTag(u32)
    }
  }

  def cltvExpiry: Gen[LnTag.MinFinalCltvExpiry] = {
    NumberGenerator.uInt32s.map { u32 =>
      LnTag.MinFinalCltvExpiry(u32)

    }
  }

  def fallbackAddress: Gen[LnTag.FallbackAddressTag] = {
    AddressGenerator.address.map { addr =>
      LnTag.FallbackAddressTag(addr)
    }
  }

  def routingInfo: Gen[LnTag.RoutingInfo] = {
    LnRouteGen.routes
      .map(rs => LnTag.RoutingInfo(rs))
  }

  def taggedFields: Gen[LnTaggedFields] = for {
    paymentHash <- paymentHashTag
    descOrHashTag <- descriptionOrDescriptionHashTag

    //optional fields
    expiryTime <- Gen.option(expiryTime)
    cltvExpiry <- Gen.option(cltvExpiry)
    fallbackAddress <- Gen.option(fallbackAddress)
    routes <- Gen.option(routingInfo)

  } yield LnTaggedFields(
    paymentHash = paymentHash,
    descriptionOrHash = descOrHashTag,
    expiryTime = expiryTime,
    cltvExpiry = cltvExpiry,
    fallbackAddress = fallbackAddress,
    routingInfo = routes)

  def lnInvoiceSignature: Gen[LnInvoiceSignature] = for {
    sig <- CryptoGenerators.digitalSignature
    version <- Gen.choose(0, 3).map(UInt8(_))
  } yield LnInvoiceSignature(version, sig)

  def lnInvoice: Gen[LnInvoice] = for {
    hrp <- lnHrp
    //timestamp is 35 bits according to BOLT11
    timestamp <- Gen.choose(0, NumberUtil.pow2(35).toLong).map(UInt64(_))
    tags <- taggedFields
    signature <- lnInvoiceSignature
  } yield LnInvoice(
    hrp = hrp,
    timestamp = timestamp,
    lnTags = tags,
    signature = signature)

}

object LnInvoiceGen extends LnInvoiceGen
