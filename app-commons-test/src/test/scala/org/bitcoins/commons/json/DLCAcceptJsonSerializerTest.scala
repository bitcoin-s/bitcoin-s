package org.bitcoins.commons.json

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.protocol.tlv.DLCAcceptTLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCAcceptJsonSerializerTest extends BitcoinSUnitTest {

  behavior of "DLCAcceptJsonSerializer"

  private val testString: String =
    s"""
       |{
       |      "temporaryContractId": "bdc5286cd4b56c2b2525bc9e0e3dda1d6a2a130cc7fd8ca8a38d401ef9a5d3e7",
       |      "acceptCollateral": 100000000,
       |      "fundingPubkey": "0208dfffdda2a61c78c906f5d76afdb0b8fe0555e6a3644c41f53b511427f80f0a",
       |      "payoutSpk": "001463c84b34fcf37ee58566aa6daf0747f74e509970",
       |      "payoutSerialId": "11859227771650291066",
       |      "fundingInputs": [
       |        {
       |          "inputSerialId": "6134040349072004330",
       |          "prevTx": "020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03510101ffffffff0200f2052a01000000160014c87d38bcd3a468680e7c0abeeb7821d6302df9120000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf90120000000000000000000000000000000000000000000000000000000000000000000000000",
       |          "prevTxVout": 0,
       |          "sequence": 4294967295,
       |          "maxWitnessLen": 107,
       |          "redeemScript": ""
       |        }
       |      ],
       |      "changeSpk": "001481467abc1d30f5139fe8ff8c1ca7f7cb3f5bc031",
       |      "changeSerialId": "13987689245506757418",
       |      "cetAdaptorSignatures": {
       |        "ecdsaAdaptorSignatures": [
       |          {
       |            "signature": "02b7dda1b4030e0f85a98eb15a6806f1ffb72b578a508f671f4e6bbd954aa2d5b9022a01536f70340da9ba2b0e034deec1a0b658cbec2432f2fa2a96de09000eafaafb586eec1375c85737bb7e9fde1cb7fcf3a8a970e698d0e5da55297ea64a45b8ffb2b705974b91e8e3d9b0e46573c648122fda1ef941980ba845ab09d1e7a43949add788ed79e4a23b832b1418c3b54251b0d3c83e791ce24c2e30544456d3b8"
       |          },
       |          {
       |            "signature": "02d510a07687553f7dd26ff6124cdccbf73ccd8661ae9bd6a59a25cedac04727fd03fb8955cb520fde3dc30fbf095054a87f8c3e87f027238e3bc435b0dc6df2532af475c531e234ee045d6119d989a62d8b29bdfdbdcdf8d6761b0cb5fbd1eb6ef71fa04c7a993801e2d02d5659c08f629f7d5ef02173ce5b1fcec361d99b9aee79549a085c14bb3f7df507e891a35089b3d9886e7c81b7367cac8c75bbb9432861"
       |          },
       |          {
       |            "signature": "0328ca81f2f281c39eda04fb69456f6b104f09d920d57def9e396aa60de6c0b38c03e73ec3891966e5d339ea154bf17e265f80cb75bfc922b83d79102f44479a69e281a485c8e99abc382aad656983f8f92a5836437156c783e261bfe0bf7eec9e3bb83ff8b4d948458b3d8fdfbc74b23cd02a72b06b84aa156a7bf6a578757c205257aa86a728b6b8022c303c9b01164f0332d6bc5aaa17014c7bb87d598b83af5d"
       |          },
       |          {
       |            "signature": "0337e4ce2c5b3fb9d70663bac8797f8e7a1493af23f74ac9ace7449a35d59c757a02676f9cc3adf9d3d03ef5a4daf704c5d178dae12364f0297ab09982d5da08145010fc26c71166e6a2e651e4c351916f2e7d538c14adeccbd2f21c18f1d4ddea619120af9bcef67dcda0ed5b8d5c4dddee167107263becf05c91e0a13c2e653cd9722674531031610ef6b8cd80b205fc78834db55cf08e45d37616a6e218b7e09c"
       |          }
       |        ]
       |      },
       |      "refundSignature": "304402202c9b25719f0a22d7372c54c36f916e44f445135809433585636417fe18b9316c0220125584711c7238d0adf4a494e30a166fef35edf3d0d1718955abd73b76a26e9d",
       |      "negotiationFields": null
       |    }
       |""".stripMargin

  it must "have serialization symmetry for a accept json message" in {
    val accept = upickle.default.read[DLCAcceptTLV](testString)(
      Picklers.dlcAcceptTLVPickler)
    val json: String =
      upickle.default.write(accept)(Picklers.dlcAcceptTLVPickler)
    assert(json == testString.replaceAll("\\s", ""))
  }

}
