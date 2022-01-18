package org.bitcoins.testkitcore.dlc

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.dlc.models.{
  DLCPayoutCurve,
  EnumContractDescriptor,
  NumericContractDescriptor,
  PiecewisePolynomialEndpoint,
  RoundingIntervals
}
import org.bitcoins.core.protocol.tlv.{DLCSerializationVersion, EnumOutcome}
import org.bitcoins.core.util.NumberUtil

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[String] = {
    (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector
  }

  def genValues(size: Int, totalAmount: CurrencyUnit): Vector[Satoshis] = {
    val vals = if (size < 2) {
      throw new IllegalArgumentException(
        s"Size must be at least two, got $size")
    } else if (size == 2) {
      Vector(totalAmount.satoshis, Satoshis.zero)
    } else {
      (0 until size - 2).map { _ =>
        Satoshis(NumberUtil.randomLong(totalAmount.satoshis.toLong))
      }.toVector :+ totalAmount.satoshis :+ Satoshis.zero
    }

    val valsWithOrder = vals.map(_ -> scala.util.Random.nextDouble())
    valsWithOrder.sortBy(_._2).map(_._1)
  }

  def genContractDescriptors(
      outcomes: Vector[String],
      totalInput: CurrencyUnit): (
      EnumContractDescriptor,
      EnumContractDescriptor) = {
    val outcomeMap =
      outcomes
        .map(EnumOutcome.apply)
        .zip(DLCTestUtil.genValues(outcomes.length, totalInput))

    val info = EnumContractDescriptor(outcomeMap)
    val remoteInfo = info.flip(totalInput.satoshis)

    (info, remoteInfo)
  }

  /** Generates a collared forward contract.
    *
    * If roundingIntervals is noRounding and numRounds > 0, then
    * roundingIntervals is ignored and instead the contract is rounded
    * in numRounds different ways in between the collars.
    * Otherwise roundingIntervals is used.
    */
  def genMultiDigitContractInfo(
      numDigits: Int,
      totalCollateral: CurrencyUnit,
      roundingIntervals: RoundingIntervals = RoundingIntervals.noRounding,
      numRounds: Int = 0): (
      NumericContractDescriptor,
      NumericContractDescriptor) = {
    val overMaxValue = Math.pow(2, numDigits).toLong
    // Left collar goes from [0, botCollar]
    val botCollar = NumberUtil.randomLong(overMaxValue / 2)
    val halfWindow = scala.math.min(overMaxValue / 4, 2500)
    val topCollarDiff = NumberUtil.randomLong(halfWindow)
    // Right collar goes from [topCollar, overMaxValue)
    val topCollar = botCollar + halfWindow + topCollarDiff
    val isGoingLong = scala.util.Random.nextBoolean()
    // leftVal and rightVal determine whether the contract shape
    // goes from total to 0 or 0 to total
    val (leftVal, rightVal) =
      if (isGoingLong) (Satoshis.zero, totalCollateral.satoshis)
      else (totalCollateral.satoshis, Satoshis.zero)
    val func = DLCPayoutCurve.polynomialInterpolate(
      Vector(
        PiecewisePolynomialEndpoint(0, leftVal),
        PiecewisePolynomialEndpoint(botCollar + 1, leftVal),
        PiecewisePolynomialEndpoint(topCollar, rightVal),
        PiecewisePolynomialEndpoint(overMaxValue - 1, rightVal)
      ),
      serializationVersion = DLCSerializationVersion.Beta
    )
    val roundingIntervalsToUse =
      if (numRounds > 0 && roundingIntervals == RoundingIntervals.noRounding) {
        val intervalStarts = 0.until(numRounds).toVector.map { num =>
          val intervalStart =
            ((numRounds - num) * botCollar + num * topCollar) / numRounds
          val roundingMod = 1L << num
          RoundingIntervals.IntervalStart(BigDecimal(intervalStart),
                                          roundingMod)
        }
        RoundingIntervals(intervalStarts)
      } else roundingIntervals
    val info =
      NumericContractDescriptor(func, numDigits, roundingIntervalsToUse)
    val remoteInfo = info.flip(totalCollateral.satoshis)
    (info, remoteInfo)
  }

  val acceptHex: String =
    "a71cd1dc7596d2a2f14a2542f2b7e563c149c910ba2238b59532b9fc1ebf6f8465f1000000000000ea60037dbfd3eb9296d1bd537c4008794989893821d48a163b0f3b7b01433599e8121100160014cb62b501146622502837f5cd898a657feba867eb9701e48dde0387770001fda714f4e2f7667f1f20d72700de02000000000101433df2df0297e1de67283fd8d369a16be5179f3a696fe36bf601337426408af50100000000000000000280f0fa0200000000160014d431e458cc852e7f2f21a6055751b5d2f30b7bcac190f30200000000160014aba424ac1cdf6c4ff53dcc76c1458c2e8bb7c71702473044022049911e914325d2c21f1f7a52d0bef9f50362cf130a87d73a30e61863fa72582502205338e9fa57b6172c821cb9e2530e7b0bf27812e2110ffa7adf277da1be31872d012103cf091ac1820aabeb4399d63bacb5b1c58310166a8b9466bc5efb4dbb678aba1e0000000000000000ffffffff006b0000001600145097a3a31e88036a7396728b6e3ea47c70780f8db68e98eec3ad4f26fda716fd01e70302834a87e2019a7c431db58235abd00ce62ca5a36e8b5e60bc6dd895045e9d936302e00d2ff673cf117f8bc3e6cbbbdc9efeb6b641d55c32c58ca46422c7279db2a3d07fa999360ba675ad6d342d21997e8644f60509a106df667f85f948f1d73ca5de082db23a50f07d9180e253bd8879567d1e4110f07de6f57ef75cf32206edc4d0656f6dcd738f0301d4baf73182aac102a1917b9688dcb7b74bcdcc85374b9203e9a2eb7600712af2236b80ebceb644d5bad65f3ccbcdcf8f2a649a48fd8a3b72023a6a694000be8df5519f7d5e11414b73274377300655be6ad1cc0da6c0eedf1fe36e4e82f3125c8ba40a1bcda444b4ec528d1674e6cee5e61d71f92586212ab2b5afdc90fe4744db5c9873eb240dfaf1952b41b3f3fa7e7a86def2a691bcbae5ad968459f712041a6ea8f39314c19545f8b5b501250aae6d09d0cfff7d9f79f403293e081859a95c05ab7a6fb6b5e960c928a199b89166188d4db5c4546fa8baef03a08004a37d041bcb10ceb94d3797814789ac192c2c79d9ddd41397cb0d0e6af84fcb15cdc885df164aaa81e2dbc95af76d4ca71017b81b2b20307c4a067d844c427f1e5c06da2082911203f61bc44d5983a203861f06e48241d407d56b72bb612792014ff77d8e2288699225241ca75d35823e563f71118851bd5f4f1aebf94c76d380d9f9315a822ef2054c1e234def6087ec7504e7423d51adcf6ed199a317199dbb561ee6ba7ecc736c5cd746fc17fa3609c6b0e1f19c4323d9e77ac67d92fdd82600"

  val expectedAcceptJsonString: String = {
    s"""
       |{
       |  "result":
       |    {
       |      "temporaryContractId": "d1dc7596d2a2f14a2542f2b7e563c149c910ba2238b59532b9fc1ebf6f8465f1",
       |      "acceptCollateral": 60000,
       |      "fundingPubkey": "037dbfd3eb9296d1bd537c4008794989893821d48a163b0f3b7b01433599e81211",
       |      "payoutSpk": "0014cb62b501146622502837f5cd898a657feba867eb",
       |      "payoutSerialId": "10881229472670123895",
       |      "fundingInputs": [
       |        {
       |          "inputSerialId": "16354653267988371239",
       |          "prevTx": "02000000000101433df2df0297e1de67283fd8d369a16be5179f3a696fe36bf601337426408af50100000000000000000280f0fa0200000000160014d431e458cc852e7f2f21a6055751b5d2f30b7bcac190f30200000000160014aba424ac1cdf6c4ff53dcc76c1458c2e8bb7c71702473044022049911e914325d2c21f1f7a52d0bef9f50362cf130a87d73a30e61863fa72582502205338e9fa57b6172c821cb9e2530e7b0bf27812e2110ffa7adf277da1be31872d012103cf091ac1820aabeb4399d63bacb5b1c58310166a8b9466bc5efb4dbb678aba1e00000000",
       |          "prevTxVout": 0,
       |          "sequence": 4294967295,
       |          "maxWitnessLen": 107,
       |          "redeemScript": ""
       |        }
       |      ],
       |      "changeSpk": "00145097a3a31e88036a7396728b6e3ea47c70780f8d",
       |      "changeSerialId": "13154619712848351014",
       |      "cetAdaptorSignatures": {
       |        "ecdsaAdaptorSignatures": [
       |          {
       |            "signature": "02834a87e2019a7c431db58235abd00ce62ca5a36e8b5e60bc6dd895045e9d936302e00d2ff673cf117f8bc3e6cbbbdc9efeb6b641d55c32c58ca46422c7279db2a3d07fa999360ba675ad6d342d21997e8644f60509a106df667f85f948f1d73ca5de082db23a50f07d9180e253bd8879567d1e4110f07de6f57ef75cf32206edc4d0656f6dcd738f0301d4baf73182aac102a1917b9688dcb7b74bcdcc85374b92"
       |          },
       |          {
       |            "signature": "03e9a2eb7600712af2236b80ebceb644d5bad65f3ccbcdcf8f2a649a48fd8a3b72023a6a694000be8df5519f7d5e11414b73274377300655be6ad1cc0da6c0eedf1fe36e4e82f3125c8ba40a1bcda444b4ec528d1674e6cee5e61d71f92586212ab2b5afdc90fe4744db5c9873eb240dfaf1952b41b3f3fa7e7a86def2a691bcbae5ad968459f712041a6ea8f39314c19545f8b5b501250aae6d09d0cfff7d9f79f4"
       |          },
       |          {
       |            "signature": "03293e081859a95c05ab7a6fb6b5e960c928a199b89166188d4db5c4546fa8baef03a08004a37d041bcb10ceb94d3797814789ac192c2c79d9ddd41397cb0d0e6af84fcb15cdc885df164aaa81e2dbc95af76d4ca71017b81b2b20307c4a067d844c427f1e5c06da2082911203f61bc44d5983a203861f06e48241d407d56b72bb612792014ff77d8e2288699225241ca75d35823e563f71118851bd5f4f1aebf94c"
       |          }
       |        ]
       |      },
       |      "refundSignature": "3044022076d380d9f9315a822ef2054c1e234def6087ec7504e7423d51adcf6ed199a3170220199dbb561ee6ba7ecc736c5cd746fc17fa3609c6b0e1f19c4323d9e77ac67d92",
       |      "negotiationFields": null
       |    },
       |
       |  "error": null
       |}
       |""".stripMargin
  }

  val expectedAccept: ujson.Value = ujson.read(expectedAcceptJsonString)

  val signHex =
    "a71eafcd7e786c0b9085784a6d063c7f4e7291784f9be3ba23cd8734559040fd2202fda716fd02890402feee4c75948830549fd19efd93cd4e00d4f538041ecf2f2cb6820c78c0a57fc9034d9ce5849e0abe235b8c3137506745508aa17c7a3064ec4172e76c88e66ca27257c5ec09f99201c9bc95b8ab345aff9b32b89c328d21a6ca5553f287a650320162381ef95042d9ac57f66f39dc6904f17a472671d3828f75ef29fbddcd31119b69adb216dac9b5960b91b3e3d83b9310484ef667d19f4bed01964ff063a2acbd025e840169038f0d046cc5005d9d622fd19ec2dd84d15a022f3eff4a781c6f404602cf8ec975191b73ecc3944786f5cca2a01ba3e71bbde9ea6932083c23591dfb6e3e06489cde6415c39cc9e187283f7a9b76227b157572dc62f1a5b130b4fc6b68c14ea07deddb2865d634a244839a87f158145c59d9aecb51b082c3a2ef0cb0201bc65252a3876f7253855ce4d3e72cd650bc275dd878f920162af74865b0346b03ea132cd20738f1a6640f98dc5664d12952e000feb69bbb3c4929c9054b3ca965030b788e83098f906cd3b088ad297e8149d08eb02442f42bd2b9e4455d5a19a96fd329625e1f1d0db9b9f625a6242f1fc53f56ec8090575c704e364518bdf7d690386d0f4a7bfea6c263c5fbc91631b0f0323a92c51f43f3228c9649c5441b338f2e37cbad0d10ce0d80429e2a6a528e4a0e9700188b86c267246b5049ce7da03102c2ec44af74653bdad7db89a38ab61d3dabf0646acdfab1bd5906d4a75f8d7a0c03051281626d4c8c84a5b7d09717c69f1c5340300be5ef6d94a4efe4d1baebbf820f8891a3b86d0f4a9a42d3fa0a44f7d711a5e65cc887b0424530e9b7e49f8d3a21d89762d535c7f121bc057b01b67372ea19762fad714580767ea6a091771808c3806b273382e0792573f624a90e15004a3b852e627f422c07209fd876c3816905d4dcc449db1a5997b82f2aabaffb3414ad1e10c159fc3fcbd6dab121a3cb9837583d39f66aa347a8170dba12cf88102997f460bb27d70365f7f85cd4ce5ee2fda71870000100020047304402204bbe45fd65402ad89784062c2af0f0c88a55f1de4509e4491f7933ada30fbab102207144fe9b8d621d2bef5c226d6f2f8ff43548f4c0bb9ea221a0c2a034a069c74f01002103bdb496d31a30d8e87ab7767fd26bfdfa7f6d06a0cfaea082a91af1ae1814f251"

  val expectedSignJsonString: String = {
    s"""
       |{
       |  "result":
       |
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
       |    },
       |
       |  "error": null
       |}
       |""".stripMargin
  }

  val expectedSign: ujson.Value = ujson.read(expectedSignJsonString)
}
