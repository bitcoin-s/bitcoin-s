package org.bitcoins.commons.json

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCOfferJsonSerializerTest extends BitcoinSUnitTest {

  behavior of "DLCOfferJsonSerializer"

  private val testString: String = {
    s"""
       |{
       |      "contractFlags": 0,
       |      "chainHash": "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f",
       |      "contractInfo": {
       |        "singleContractInfo": {
       |          "totalCollateral": 200000000,
       |          "contractInfo": {
       |            "contractDescriptor": {
       |              "enumeratedContractDescriptor": {
       |                "payouts": [
       |                  {
       |                    "outcome": "a",
       |                    "localPayout": 200000000
       |                  },
       |                  {
       |                    "outcome": "b",
       |                    "localPayout": 0
       |                  },
       |                  {
       |                    "outcome": "c",
       |                    "localPayout": 200000000
       |                  },
       |                  {
       |                    "outcome": "d",
       |                    "localPayout": 0
       |                  }
       |                ]
       |              }
       |            },
       |            "oracleInfo": {
       |              "single": {
       |                "oracleAnnouncement": {
       |                  "announcementSignature": "1d63bcf31eadaf46a551085dc0aa2e7952a42e5e31f191f87ee780626ad7ec6dc8693e6e916beddf7bafc30cdd2878b777f35b960e5b5f79a3b2bdf204b7f900",
       |                  "oraclePublicKey": "d369457a58f3b2d58b2b7181175553cd5630f2675b4e9a502fc83e42d734c0fc",
       |                  "oracleEvent": {
       |                    "oracleNonces": [
       |                      "a9209c1353d01c0cf5f886dafea25562335a474db09528c5d9c1799c2c003471"
       |                    ],
       |                    "eventMaturityEpoch": 1623133104,
       |                    "eventDescriptor": {
       |                      "enumEvent": {
       |                        "outcomes": [
       |                          "a",
       |                          "b",
       |                          "c",
       |                          "d"
       |                        ]
       |                      }
       |                    },
       |                    "eventId": "Test"
       |                  }
       |                }
       |              }
       |            }
       |          }
       |        }
       |      },
       |      "fundingPubkey": "0235e7068dc5c1ec01f2d5bf71b6e511f8eda6fe4a7407d5f465e40ffcf20c6bf1",
       |      "payoutSpk": "00144be5b3b04ee5efdbca2487560603e83a81466e11",
       |      "payoutSerialId": "8158853424317423947",
       |      "offerCollateral": 100000000,
       |      "fundingInputs": [
       |        {
       |          "inputSerialId": "10405578030502158246",
       |          "prevTx": "020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014dd0dd71cd151577fcfc9a179f3213ab2de6721fc0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf90120000000000000000000000000000000000000000000000000000000000000000000000000",
       |          "prevTxVout": 0,
       |          "sequence": 4294967295,
       |          "maxWitnessLen": 107,
       |          "redeemScript": ""
       |        }
       |      ],
       |      "changeSpk": "0014005c54a219caaa4b2bbb74506c1a27a672f6c6f7",
       |      "changeSerialId": "13407051885209863522",
       |      "fundOutputSerialId": "9963503867272128222",
       |      "feeRatePerVb": 2,
       |      "contractMaturityBound": 1623133104,
       |      "contractTimeout": 1623737904,
       |      "temporaryContractId":"30c1655440045a24f8d51de650c738415469f145f0b691998b2dc81e51dd5c9d"
       |}
       |""".stripMargin
  }

  it must "have serialization symmetry for a offer json message" in {
    val offer =
      upickle.default.read[DLCOfferTLV](testString)(Picklers.dlcOfferTLVPickler)
    val json: String =
      upickle.default.write(offer)(Picklers.dlcOfferTLVPickler)

    assert(json == testString.replaceAll("\\s", ""))
  }
}
