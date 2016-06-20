package org.bitcoins.core.util

import org.bitcoins.core.crypto.{ECPrivateKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.util.testprotocol.{ConfigParamsImpl, Base58TestCaseImpl, Base58TestCase}
import org.bitcoins.core.util.testprotocol.Base58TestCaseProtocol._
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source

/**
  * Created by tom on 5/17/16.
  */
class Base58Test extends FlatSpec with MustMatchers {
  "Base58" must "encode byte value of 0 to character of 1" in {
    Base58.base58Characters(0) must be ('1')
  }

  it must "encode byte value of 22 to character P" in {
    Base58.base58Characters(22) must be ('P')
  }

  it must "decode character 1 to byte value of 0" in {
    Base58.base58Pairs('1') must be (0.toByte)
  }

  it must "decode character Z to byte value of 32" in {
    Base58.base58Pairs('Z') must be (32.toByte)
  }

  it must "decode and return same result as bitcoinj" in {
    val address = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    val bitcoinj = org.bitcoinj.core.Base58.decode(address)
    Base58.decode(address) must be (bitcoinj)
  }

  it must "encode/decode tests in base58_encode_decode.json" in {
    /*
    [
    ["", ""],
    ["61", "2g"],
    ["626262", "a3gV"],
    ["636363", "aPEr"],
    ["73696d706c792061206c6f6e6720737472696e67", "2cFupjhnEsSn59qHXstmK2ffpLv2"],
    ["00eb15231dfceb60925886b67d065299925915aeb172c06647", "1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L"],
    ["516b6fcd0f", "ABnLTmg"],
    ["bf4f89001e670274dd", "3SEo3LWLoPntC"],
    ["572e4794", "3EFU7m"],
    ["ecac89cad93923c02321", "EJDM8drfXA6uyA"],
    ["10c8511e", "Rt5zm"],
    ["00000000000000000000", "1111111111"]
    ]
     */
    Base58.encode("") must be ("")
    Base58.encode("61") must be ("2g")
    Base58.encode("626262") must be ("a3gV")
    Base58.encode("636363") must be ("aPEr")
    Base58.encode("73696d706c792061206c6f6e6720737472696e67") must be ("2cFupjhnEsSn59qHXstmK2ffpLv2")
    Base58.encode("00eb15231dfceb60925886b67d065299925915aeb172c06647") must be ("1NS17iag9jJgTHD1VXjvLCEnZuQ3rJDE9L")
    Base58.encode("516b6fcd0f") must be ("ABnLTmg")
    Base58.encode("bf4f89001e670274dd") must be ("3SEo3LWLoPntC")
    Base58.encode("572e4794") must be ("3EFU7m")
    Base58.encode("ecac89cad93923c02321") must be ("EJDM8drfXA6uyA")
    Base58.encode("10c8511e") must be ("Rt5zm")
    Base58.encode("00000000000000000000") must be ("1111111111")
  }

  it must "decode address into bytes, then encode bytes back to address the same as bitcoinj" in {
    //1C4kYhyLftmkn48YarSoLupxHfYFo8kp64
    val address = TestUtil.bitcoinAddress.value
    val bitcoinj = org.bitcoinj.core.Base58.encode(org.bitcoinj.core.Base58.decode(address))
    Base58.encode(Base58.decode(address)) must be (bitcoinj)
    Base58.encode(Base58.decode(address)) must be ("1C4kYhyLftmkn48YarSoLupxHfYFo8kp64")
  }

  it must "decode asset address into bytes then encode back to asset address" in {
    //akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA
    val asset = TestUtil.assetAddress.value
    val bitcoinj = org.bitcoinj.core.Base58.encode(org.bitcoinj.core.Base58.decode(asset))
    Base58.encode(Base58.decode(asset)) must be ("akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")
    Base58.encode(Base58.decode(asset)) must be (bitcoinj)
  }

  it must "decode multisig address into bytes then encode back to multisig" in {
    val multi = TestUtil.multiSigAddress.value
    val bitcoinj = org.bitcoinj.core.Base58.encode(org.bitcoinj.core.Base58.decode(multi))
    Base58.encode(Base58.decode(multi)) must be (TestUtil.multiSigAddress.value)
    Base58.encode(Base58.decode(multi)) must be (bitcoinj)
  }

  it must "read base58_keys_valid.json and validate each case" in {
    val source = Source.fromURL(this.getClass.getResource("/base58_keys_valid.json"))
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCases : Seq[Base58TestCase] = json.convertTo[Seq[Base58TestCase]]
    for {
      testCase <- testCases
    } yield {
      testCase must be (Base58TestCaseImpl(testCase.addressOrWIFPrivKey, testCase.hashOrPrivKey, testCase.configParams))
    }

    //first, second and 48th test cases:
    testCases.head must be (Base58TestCaseImpl(Left(Address("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i")),
      Left(Sha256Hash160Digest("65a16059864a2fdbc7c99a4723a8395bc6f188eb")),
      ConfigParamsImpl(Left("pubkey"), false, false)))

    testCases(1) must be (Base58TestCaseImpl(Left(Address("3CMNFxN1oHBc4R1EpboAL5yzHGgE611Xou")),
      Left(Sha256Hash160Digest("74f209f6ea907e2ea48f74fae05782ae8a665257")), ConfigParamsImpl(Left("script"), false, false)))

    testCases(47) must be (Base58TestCaseImpl(Right("cMxXusSihaX58wpJ3tNuuUcZEQGt6DKJ1wEpxys88FFaQCYjku9h"),
      Right(ECPrivateKey("0b3b34f0958d8a268193a9814da92c3e8b58b4a4378a542863e34ac289cd830c")),
      ConfigParamsImpl(Right(true), true, true)))
  }


}
