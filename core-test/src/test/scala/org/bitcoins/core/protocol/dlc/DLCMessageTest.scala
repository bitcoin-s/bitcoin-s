package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.{BlockHeight, BlockTime}
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.gen.{LnMessageGen, TLVGen}
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class DLCMessageTest extends BitcoinSJvmTest {
  behavior of "DLCMessage"

  it must "not allow a DLCTimeout where the contract times out before it matures" in {
    assertThrows[IllegalArgumentException](
      DLCTimeouts(BlockHeight(4), BlockHeight(2)))
    assertThrows[IllegalArgumentException](
      DLCTimeouts(BlockTime(UInt32(4)), BlockTime(UInt32(2))))
  }

  val dummyPubKey: ECPublicKey = ECPublicKey.freshPublicKey
  val dummyPubKey2: ECPublicKey = ECPublicKey.freshPublicKey

  val dummyAddress: BitcoinAddress = BitcoinAddress(
    "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")

  val dummyStr: String =
    "00000000000000000008bba30d4d0fb53dcbffb601557de9f16d257d4f1985b7"

  val dummyOracle: EnumSingleOracleInfo = EnumSingleOracleInfo.dummyForKeys(
    ECPrivateKey.freshPrivateKey,
    ECPublicKey.freshPublicKey.schnorrNonce,
    Vector(EnumOutcome(dummyStr)))

  val dummySig: PartialSignature =
    PartialSignature(dummyPubKey, DummyECDigitalSignature)

  it must "not allow a negative collateral for a DLCOffer" in {
    assertThrows[IllegalArgumentException](
      DLCOffer(
        contractInfo = ContractInfo.dummy,
        pubKeys = DLCPublicKeys(dummyPubKey, dummyAddress),
        totalCollateral = Satoshis(-1),
        fundingInputs = Vector.empty,
        changeAddress = dummyAddress,
        payoutSerialId = UInt64.zero,
        changeSerialId = UInt64.one,
        fundOutputSerialId = UInt64.max,
        feeRate = SatoshisPerVirtualByte.one,
        timeouts = DLCTimeouts(BlockHeight(1), BlockHeight(2))
      ))
  }

  it must "not allow same change and fund output serial id for a DLCOffer" in {
    assertThrows[IllegalArgumentException](
      DLCOffer(
        contractInfo = ContractInfo.dummy,
        pubKeys = DLCPublicKeys(dummyPubKey, dummyAddress),
        totalCollateral = Satoshis(-1),
        fundingInputs = Vector.empty,
        changeAddress = dummyAddress,
        payoutSerialId = UInt64.zero,
        changeSerialId = UInt64.one,
        fundOutputSerialId = UInt64.one,
        feeRate = SatoshisPerVirtualByte.one,
        timeouts = DLCTimeouts(BlockHeight(1), BlockHeight(2))
      ))
  }

  it must "not allow a negative collateral for a DLCAccept" in {
    assertThrows[IllegalArgumentException](
      DLCAccept(
        Satoshis(-1),
        DLCPublicKeys(dummyPubKey, dummyAddress),
        Vector.empty,
        dummyAddress,
        payoutSerialId = UInt64.zero,
        changeSerialId = UInt64.one,
        CETSignatures(
          Vector(
            EnumOracleOutcome(
              Vector(dummyOracle),
              EnumOutcome(dummyStr)).sigPoint -> ECAdaptorSignature.dummy),
          dummySig),
        DLCAccept.NoNegotiationFields,
        Sha256Digest.empty
      )
    )
  }

  it must "be able to go back and forth between TLV and deserialized" in {
    forAll(TLVGen.dlcOfferTLVAcceptTLVSignTLV) {
      case (offerTLV, acceptTLV, signTLV) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        assert(offer.toTLV == offerTLV)
        assert(accept.toTLV == acceptTLV)
        assert(sign.toTLV == signTLV)
    }
  }

  it must "be able to go back and forth between LN Message and deserialized" in {
    forAll(LnMessageGen.dlcOfferMessageAcceptMessageSignMessage) {
      case (offerMsg, acceptMsg, signMsg) =>
        val offer = DLCOffer.fromMessage(offerMsg)
        val accept = DLCAccept.fromMessage(acceptMsg, offer)
        val sign = DLCSign.fromMessage(signMsg, offer)

        assert(offer.toMessage == offerMsg)
        assert(accept.toMessage == acceptMsg)
        assert(sign.toMessage == signMsg)
    }
  }
}
