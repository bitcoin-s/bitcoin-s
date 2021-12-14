package org.bitcoins.commons.json

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.protocol.tlv.DLCSignTLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCSignJsonSerializerTest extends BitcoinSUnitTest {
  behavior of "DLCAcceptJsonSerializer"

  private val testString: String = {
    s"""
       |{
       |      "contractId": "afcd7e786c0b9085784a6d063c7f4e7291784f9be3ba23cd8734559040fd2202",
       |      "cetAdaptorSignatures": {
       |        "ecdsaAdaptorSignatures": [
       |          {
       |            "signature": "02feee4c75948830549fd19efd93cd4e00d4f538041ecf2f2cb6820c78c0a57fc9034d9ce5849e0abe235b8c3137506745508aa17c7a3064ec4172e76c88e66ca27257c5ec09f99201c9bc95b8ab345aff9b32b89c328d21a6ca5553f287a650320162381ef95042d9ac57f66f39dc6904f17a472671d3828f75ef29fbddcd31119b69adb216dac9b5960b91b3e3d83b9310484ef667d19f4bed01964ff063a2acbd"
       |          },
       |          {
       |            "signature": "025e840169038f0d046cc5005d9d622fd19ec2dd84d15a022f3eff4a781c6f404602cf8ec975191b73ecc3944786f5cca2a01ba3e71bbde9ea6932083c23591dfb6e3e06489cde6415c39cc9e187283f7a9b76227b157572dc62f1a5b130b4fc6b68c14ea07deddb2865d634a244839a87f158145c59d9aecb51b082c3a2ef0cb0201bc65252a3876f7253855ce4d3e72cd650bc275dd878f920162af74865b0346b"
       |          },
       |          {
       |            "signature": "03ea132cd20738f1a6640f98dc5664d12952e000feb69bbb3c4929c9054b3ca965030b788e83098f906cd3b088ad297e8149d08eb02442f42bd2b9e4455d5a19a96fd329625e1f1d0db9b9f625a6242f1fc53f56ec8090575c704e364518bdf7d690386d0f4a7bfea6c263c5fbc91631b0f0323a92c51f43f3228c9649c5441b338f2e37cbad0d10ce0d80429e2a6a528e4a0e9700188b86c267246b5049ce7da031"
       |          },
       |          {
       |            "signature": "02c2ec44af74653bdad7db89a38ab61d3dabf0646acdfab1bd5906d4a75f8d7a0c03051281626d4c8c84a5b7d09717c69f1c5340300be5ef6d94a4efe4d1baebbf820f8891a3b86d0f4a9a42d3fa0a44f7d711a5e65cc887b0424530e9b7e49f8d3a21d89762d535c7f121bc057b01b67372ea19762fad714580767ea6a091771808c3806b273382e0792573f624a90e15004a3b852e627f422c07209fd876c38169"
       |          }
       |        ]
       |      },
       |      "refundSignature": "3044022005d4dcc449db1a5997b82f2aabaffb3414ad1e10c159fc3fcbd6dab121a3cb98022037583d39f66aa347a8170dba12cf88102997f460bb27d70365f7f85cd4ce5ee2",
       |      "fundingSignatures": {
       |        "fundingSignatures": [
       |          {
       |            "witnessElements": [
       |              {
       |                "witness": "304402204bbe45fd65402ad89784062c2af0f0c88a55f1de4509e4491f7933ada30fbab102207144fe9b8d621d2bef5c226d6f2f8ff43548f4c0bb9ea221a0c2a034a069c74f01"
       |              },
       |              {
       |                "witness": "03bdb496d31a30d8e87ab7767fd26bfdfa7f6d06a0cfaea082a91af1ae1814f251"
       |              }
       |            ]
       |          }
       |        ]
       |      }
       |    }
       |""".stripMargin
  }

  it must "have serialization symmetry for dlc sign messages" in {
    val sign =
      upickle.default.read[DLCSignTLV](testString)(Picklers.dlcSignTLVPickler)
    val json: String =
      upickle.default.write(sign)(Picklers.dlcSignTLVPickler)
    assert(json == testString.replaceAll("\\s", ""))
  }
}
