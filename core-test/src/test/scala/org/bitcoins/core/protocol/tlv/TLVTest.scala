package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.protocol.dlc.models.ContractInfo
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

  it must "parse a contract info pre 144" in {
    //this was a contract info created before we implemented support for
    //https://github.com/discreetlogcontracts/dlcspecs/pull/144
    val oldHex =
      "fdd82efd032500000000000186a0fda720540011fda72648000501000000000000000000000001fd9c400000000000000000000001fda604000000000000c350000001fdafc800000000000186a0000001fe0001ffff00000000000186a00000fda724020000fda712fd02bffdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
    //https://test.oracle.suredbits.com/contract/numeric/d4d4df2892fb2cfd2e8f030f0e69a568e19668b5d355e7713f69853db09a4c33
    assert(ContractInfo.fromHexOpt(oldHex).isDefined)
  }
}
