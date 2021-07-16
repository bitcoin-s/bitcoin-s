package org.bitcoins.core.protocol.tlv

import org.bitcoins.testkitcore.gen.TLVGen
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TLVTest extends BitcoinSUnitTest {

  "NormalizedString" must "correctly compare" in {
    val original =
      "G�\u0006=ẁ7\u007F8�\u0001 �\u0001z �/�\u001F��\u0001\u0001��\u0001�d\u0012\u0001� �\u007F���\u0001�\u0001��9�\u0001\u007F��\u007F\u0001'��\u0001�\u001B���\u001F"
    val normalized =
      "G�\u0006=ẁ7\u007F8�\u0001 �\u0001z �/�\u001F��\u0001\u0001��\u0001�d\u0012\u0001� �\u007F���\u0001�\u0001��9�\u0001\u007F��\u007F\u0001'��\u0001�\u001B���\u001F"

    assert(NormalizedString(original) == NormalizedString(normalized))
  }

  "TLV" must "have serialization symmetry" in {
    forAll(TLVGen.tlv) { tlv =>
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  it must "have unique types" in {
    val allTypes = TLV.allFactories.map(_.tpe)
    assert(allTypes.distinct == allTypes)
  }

  "UnknownTLV" must "have serialization symmetry" in {
    forAll(TLVGen.unknownTLV) { unknown =>
      assert(UnknownTLV(unknown.bytes) == unknown)
      assert(TLV(unknown.bytes) == unknown)
    }
  }

  "InitTLV" must "have serialization symmetry" in {
    forAll(TLVGen.initTLV) { init =>
      assert(InitTLV(init.bytes) == init)
      assert(TLV(init.bytes) == init)
    }
  }

  "ErrorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.errorTLV) { error =>
      assert(ErrorTLV(error.bytes) == error)
      assert(TLV(error.bytes) == error)
    }
  }

  "PingTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pingTLV) { ping =>
      assert(PingTLV(ping.bytes) == ping)
      assert(TLV(ping.bytes) == ping)
    }
  }

  "PongTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pongTLV) { pong =>
      assert(PongTLV(pong.bytes) == pong)
      assert(TLV(pong.bytes) == pong)
    }
  }

  "EventDescriptorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.eventDescriptorTLV) { tlv =>
      assert(EventDescriptorTLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "EnumEventDescriptorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.enumEventDescriptorV0TLV) { tlv =>
      assert(EnumEventDescriptorV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "DigitDecompositionEventDescriptorV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.digitDecompositionEventDescriptorV0TLV) { tlv =>
      assert(DigitDecompositionEventDescriptorV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "OracleEventV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleEventV0TLV) { tlv =>
      val oracleEvent = OracleEventV0TLV(tlv.bytes)
      assert(oracleEvent == tlv)
      assert(oracleEvent.maturation == tlv.maturation)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "OracleAnnouncementV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleAnnouncementV0TLV) { tlv =>
      assert(OracleAnnouncementV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "OracleAttestmentV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleAttestmentV0TLV) { tlv =>
      assert(OracleAttestmentV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "ContractInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.contractInfoV0TLV) { contractInfo =>
      assert(ContractInfoV0TLV(contractInfo.bytes) == contractInfo)
      assert(TLV(contractInfo.bytes) == contractInfo)
    }
  }

  "ContractDescriptorV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.contractDescriptorV0TLV) { contractDescriptorV0TLV =>
      assert(
        ContractDescriptorV0TLV(
          contractDescriptorV0TLV.bytes) == contractDescriptorV0TLV)
      assert(TLV(contractDescriptorV0TLV.bytes) == contractDescriptorV0TLV)
    }
  }

  "ContractDescriptorV1TLV" must "have serialization symmetry" in {
    forAll(TLVGen.contractDescriptorV1TLV) { contractDescriptorV1TLV =>
      assert(
        ContractDescriptorV1TLV(
          contractDescriptorV1TLV.bytes) == contractDescriptorV1TLV)
      assert(TLV(contractDescriptorV1TLV.bytes) == contractDescriptorV1TLV)
    }
  }

  "OracleInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleInfoV0TLV) { oracleInfo =>
      assert(OracleInfoV0TLV(oracleInfo.bytes) == oracleInfo)
      assert(TLV(oracleInfo.bytes) == oracleInfo)
    }
  }

  "OracleInfoV1TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleInfoV1TLV) { oracleInfo =>
      assert(OracleInfoV1TLV(oracleInfo.bytes) == oracleInfo)
      assert(TLV(oracleInfo.bytes) == oracleInfo)
    }
  }

  "FundingInputV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.fundingInputV0TLV()) { fundingInput =>
      assert(FundingInputV0TLV(fundingInput.bytes) == fundingInput)
      assert(TLV(fundingInput.bytes) == fundingInput)
    }
  }

  "CETSignaturesV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.cetSignaturesV0TLV) { cetSigs =>
      assert(CETSignaturesV0TLV(cetSigs.bytes) == cetSigs)
      assert(TLV(cetSigs.bytes) == cetSigs)
    }
  }

  "FundingSignaturesV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.fundingSignaturesV0TLV) { fundingSigs =>
      assert(FundingSignaturesV0TLV(fundingSigs.bytes) == fundingSigs)
      assert(TLV(fundingSigs.bytes) == fundingSigs)
    }
  }

  "DLCOfferTLV" must "have serialization symmetry" in {
    forAll(TLVGen.dlcOfferTLV) { offer =>
      assert(DLCOfferTLV(offer.bytes) == offer)
      assert(TLV(offer.bytes) == offer)
    }
  }

  "DLCAcceptTLV" must "have serialization symmetry" in {
    forAll(TLVGen.dlcAcceptTLV) { accept =>
      assert(DLCAcceptTLV(accept.bytes) == accept)
      assert(TLV(accept.bytes) == accept)
    }
  }

  "DLCSignTLV" must "have serialization symmetry" in {
    forAll(TLVGen.dlcSignTLV) { sign =>
      assert(DLCSignTLV(sign.bytes) == sign)
      assert(TLV(sign.bytes) == sign)
    }
  }
}
