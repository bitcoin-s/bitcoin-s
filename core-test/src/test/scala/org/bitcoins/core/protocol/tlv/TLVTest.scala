package org.bitcoins.core.protocol.tlv

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{DLCAccept, DLCOffer}
import org.bitcoins.testkit.core.gen.TLVGen
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TLVTest extends BitcoinSUnitTest {

  "TLV" must "have serialization symmetry" in {
    forAll(TLVGen.tlv) { tlv =>
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "UnknownTLV" must "have serialization symmetry" in {
    forAll(TLVGen.unknownTLV) { unknown =>
      assert(UnknownTLV(unknown.bytes) == unknown)
      assert(TLV(unknown.bytes) == unknown)
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

  "ExternalEventDescriptorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.externalEventDescriptorV0TLV) { tlv =>
      assert(ExternalEventDescriptorV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "EnumEventDescriptorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.enumEventDescriptorV0TLV) { tlv =>
      assert(EnumEventDescriptorV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "RangeEventDescriptorV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.rangeEventDescriptorV0TLV) { tlv =>
      assert(RangeEventDescriptorV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "OracleEventV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleEventV0TLV) { tlv =>
      assert(OracleEventV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "OracleAnnouncementV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleAnnouncementV0TLV) { tlv =>
      assert(OracleAnnouncementV0TLV(tlv.bytes) == tlv)
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "ContractInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.contractInfoV0TLV) { contractInfo =>
      assert(ContractInfoV0TLV(contractInfo.bytes) == contractInfo)
      assert(TLV(contractInfo.bytes) == contractInfo)
    }
  }

  "OracleInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleInfoV0TLV) { oracleInfo =>
      assert(OracleInfoV0TLV(oracleInfo.bytes) == oracleInfo)
      assert(TLV(oracleInfo.bytes) == oracleInfo)
    }
  }

  "FundingInputV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.fundingInputV0TLV) { fundingInput =>
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

  it must "x" in {
    val jsonString = s"""{
                        |  "contractInfo": [
                        |    {
                        |      "sha256": "e8ef711baf2e0b8023125c021951d5fa3d5c963a7bacabb7894017fd8e903d02",
                        |      "sats": 0
                        |    },
                        |    {
                        |      "sha256": "7e5d428f799cac55b76828f90b32e216a84fabf76e9d27f3380a7bf7af14f452",
                        |      "sats": 100000000
                        |    },
                        |    {
                        |      "sha256": "d9298a10d1b0735837dc4bd85dac641b0f3cef27a47e5d53a54f2f3f5b2fcffa",
                        |      "sats": 60000000
                        |    }
                        |  ],
                        |  "oracleInfo": "57caa081b0a0e9e9413cf4fb72ddc2630d609bdf6a912b98c4cfd358a4ce149692ba989222e76cf0cb263fedd67587812110bde1dc1468bef63c8f6974692ea1",
                        |  "pubKeys": {
                        |    "fundingKey": "038cc19a94f9d90656173802a2cfcc4f87d43b11537a608175111f37136a2c2811",
                        |    "payoutAddress": "bc1q6cj7a6aerxxm4wc3h8z0cm7s8rp2049nsluh3w"
                        |  },
                        |  "totalCollateral": 60000000,
                        |  "fundingInputs": [
                        |    {
                        |      "prevTx": "02000000000101619d742014a3a867e7679e84b5fbf076e01c5f98446b71fc5b51ba19cf8301500100000000000000000298f708000000000016001470d0edeb2d54a194a6124e917f4be75b6d6e44607e41b7050000000016001452584eb56f12d421e5efc49ecddc5eb873958cc00247304402202a0ca561c1d8ab27adb1285452f8d3b1c65e4db9db0085e382adcf13e37e2eaa02206f78473ffd5f16518430411d4b49d403ea17ee1eb9c1b48d5a746111fa8c77440121032dbb453be8b548075ec467c77ed9a20375b4e671ac521f289fbcc132975472a400000000",
                        |      "prevTxVout": 1,
                        |      "sequence": 4294967295,
                        |      "maxWitnessLength": 107
                        |    }
                        |  ],
                        |  "changeAddress": "bc1qfwtf3538dku853wq5ga8pcx7haejdjd96mdwcu",
                        |  "feeRate": 200,
                        |  "timeouts": {
                        |    "contractMaturity": 0,
                        |    "contractTimeout": 667759
                        |  }
                        |}""".stripMargin

    val jsonValue: ujson.Value = ujson.read(jsonString)
    val acceptMsg = DLCOffer.fromJson(jsonValue)

    println(s"offer=${acceptMsg.toTLV}")
  }
}
