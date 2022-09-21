package org.bitcoins.core.protocol.tlv

import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion

import scala.io.Source

class DLCStaticTestVectors extends BitcoinSUnitTest {

  behavior of "DLCStaticTestVector"

  sealed trait TestCaseMessage[T <: TLV] {
    def serialized: String

    def msg: T

    def lnMsg: LnMessage[T] = LnMessage(msg)

    def json: ujson.Value

    def tlv: T = {
      LnMessage.fromHex(serialized).tlv.asInstanceOf[T]
    }
  }

  case class OfferTestMessage(
      msg: DLCOfferTLV,
      serialized: String,
      json: ujson.Value)
      extends TestCaseMessage[DLCOfferTLV]

  case class AcceptTestMessage(
      msg: DLCAcceptTLV,
      serialized: String,
      json: ujson.Value)
      extends TestCaseMessage[DLCAcceptTLV]

  case class SignTestMessage(
      msg: DLCSignTLV,
      serialized: String,
      json: ujson.Value)
      extends TestCaseMessage[DLCSignTLV]

  case class AttestmentTestMessage(
      msg: SchnorrAttestationTLV,
      serialized: String,
      json: ujson.Value)
      extends TestCaseMessage[SchnorrAttestationTLV]

  case class AllMessages(
      offerTestMessage: OfferTestMessage,
      acceptTestMessage: AcceptTestMessage,
      signTestMessage: SignTestMessage,
      attestmentTestMessage: Vector[AttestmentTestMessage])

  private def runTest(jsonString: String): Assertion = {
    val allMessages = parseTestCase(jsonString)
    val offerMsg = allMessages.offerTestMessage
    val acceptMsg = allMessages.acceptTestMessage
    val signMsg = allMessages.signTestMessage

    assert(offerMsg.tlv == offerMsg.msg)
    assert(offerMsg.lnMsg.hex == offerMsg.serialized)
    val offerJson =
      upickle.default.writeJs(offerMsg.msg)(Picklers.dlcOfferTLVPickler)
    assert(offerJson == offerMsg.json)

    assert(acceptMsg.tlv == acceptMsg.msg)
    assert(acceptMsg.lnMsg.hex == acceptMsg.serialized)

    val acceptJson =
      upickle.default.writeJs(acceptMsg.msg)(Picklers.dlcAcceptTLVPickler)
    assert(acceptJson == acceptMsg.json)

    assert(signMsg.tlv == signMsg.msg)
    assert(signMsg.lnMsg.hex == signMsg.serialized)
    val signJson =
      upickle.default.writeJs(signMsg.msg)(Picklers.dlcSignTLVPickler)
    assert(signJson == signMsg.json)

    val attestmentMsgs = allMessages.attestmentTestMessage
    attestmentMsgs.foreach { attestmentMsg =>
      assert(attestmentMsg.lnMsg.hex == attestmentMsg.serialized)
      assert(attestmentMsg.tlv == attestmentMsg.msg)
      val attestmentJson = upickle.default.writeJs(attestmentMsg.msg)(
        Picklers.schnorrAttestationTLV)
      assert(attestmentJson == attestmentMsg.json)
    }
    succeed
  }

  it must "conform to the enum single test vector" in {
    val url = getClass.getResource("/enum_single_oracle_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the single oracle numerical test" in {

    val url = getClass.getResource("/single_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the single oracle numerical hyperbola test" in {
    val url =
      getClass.getResource("/single_oracle_numerical_hyperbola_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_3_of_3_test.json test" in {
    val url =
      getClass.getResource("/enum_3_of_3_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_test.json test" in {
    val url =
      getClass.getResource("/three_of_three_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_with_diff_test.json test" in {
    val url =
      getClass.getResource(
        "/three_of_three_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_test.json test" in {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_test.json test" in {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_5_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_with_diff_3_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_with_diff_5_of_5_test.json test" in {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_with_diff_diff_nb_digits_test.json test" in {
    val url =
      getClass.getResource(
        "/three_of_three_oracle_numerical_with_diff_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_diff_nb_digits_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_diff_nb_digits_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_with_diff_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_three_oracle_numerical_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_three_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_three_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_three_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_diff_nb_digits_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_with_diff_diff_nb_digits_test.json test" in {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_with_diff_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  private def parseOfferMsg(obj: ujson.Obj): OfferTestMessage = {
    val offerMsgJson = obj(PicklerKeys.offerMessageKey)
    val offerJson = offerMsgJson.obj(PicklerKeys.msgKey).obj
    val offerTLV = upickle.default.read(offerJson)(Picklers.dlcOfferTLVPickler)
    val serialized = offerMsgJson(PicklerKeys.serializedKey).str
    OfferTestMessage(offerTLV, serialized, offerMsgJson)
  }

  private def parseAcceptMsg(obj: ujson.Obj): AcceptTestMessage = {
    val acceptMsgJson = obj(PicklerKeys.acceptMessageKey)
    val acceptJson = acceptMsgJson.obj(PicklerKeys.msgKey).obj
    val acceptTLV =
      upickle.default.read(acceptJson)(Picklers.dlcAcceptTLVPickler)
    val serialized = acceptMsgJson(PicklerKeys.serializedKey).str
    AcceptTestMessage(acceptTLV, serialized, acceptMsgJson)
  }

  private def parseSignMsg(obj: ujson.Obj): SignTestMessage = {
    val signMsgJson = obj(PicklerKeys.signMessageKey)
    val signJson = signMsgJson.obj(PicklerKeys.msgKey).obj
    val signTLV = upickle.default.read(signJson)(Picklers.dlcSignTLVPickler)
    val serialized = signMsgJson(PicklerKeys.serializedKey).str

    SignTestMessage(signTLV, serialized, signMsgJson)
  }

  private def parseAttestmentMsg(
      obj: ujson.Obj): Vector[AttestmentTestMessage] = {
    val attestmentMsgJson = obj(PicklerKeys.attestationsKey).arr
    attestmentMsgJson.map {
      case obj: ujson.Obj =>
        val attestmentTLV: SchnorrAttestationTLV =
          upickle.default.read(obj)(Picklers.schnorrAttestationTLV)
        val serialized = obj(PicklerKeys.serializedKey).str
        AttestmentTestMessage(attestmentTLV, serialized, obj)
      case x @ (_: ujson.Bool | _: ujson.Num | ujson.Null | _: ujson.Arr |
          _: ujson.Str) =>
        sys.error(s"Unexpectd json for attestment, got=$x")
    }.toVector

  }

  private def parseTestCase(jsonString: String): AllMessages = {
    val obj = ujson.read(jsonString).obj
    val offer = parseOfferMsg(obj)
    val accept = parseAcceptMsg(obj)
    val sign = parseSignMsg(obj)
    val attestment = parseAttestmentMsg(obj)

    AllMessages(offer, accept, sign, attestment)
  }

}

//must conform to the enum single test vector *** FAILED *** (683 milliseconds)
//[info]   "[fda71afd02440006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910ffdd82eda000000000bebc200fda71029040161000000000bebc200016200000000000000000163000000000bebc2000164000000000000000
//0fda712a1fdd8249dc5a85547be455db7aed0a03c4f8d9b45b0d5baac3c7534329a05827d0afbd4057d7951b20d1f86adcde402e6d4d2d8b0d80f9dd4d4227b9d9a9e909f22fdb2ae79fece7a58f025b865a8f562b128d20f6d5643d66c7e86c9830ce4b24
//6970055fdd822390001971fef1ec7bd1502cab3be96c27a3835ea6aae0129ae6dd520866674d91d996a60bf0bb0fdd8060a000401610162016301640454657374020d28500c90a24abf0c966b252b4368a48bd4f4355aac9cf20b1dc1c69cc45f190016001
//45a96e3dcabaa4e2a5da0f9ba2b74c6751c96c7936815a9b3b1e804000000000005f5e1000001fda714be6b4b18659574f6bc00a8020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101fff
//fffff0200f2052a01000000160014b57463cf52d05c5ad3918292a32cb81d9d7616c90000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf9012000000000000000000000000000000000000
//000000000000000000000000000000000000000000000ffffffff006b0000001600149cb01e57728a1abab75b53a84c3f3aae7509fbd8145e27b099f8a2002fd1c41a5f853e00000000000000000260bf0bb060c8463]0" did not equal
//
// "[a71a000622
//6e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f00000000000bebc20000040161000000000bebc200016200000000000000000163000000000bebc2000164000000000000000000c5a85547be455db7aed0a03c4f8d9b45b0d5ba
//ac3c7534329a05827d0afbd4057d7951b20d1f86adcde402e6d4d2d8b0d80f9dd4d4227b9d9a9e909f22fdb2ae79fece7a58f025b865a8f562b128d20f6d5643d66c7e86c9830ce4b24697005501971fef1ec7bd1502cab3be96c27a3835ea6aae0129ae6d
//d520866674d91d996a60bf0bb0000401610162016301640454657374020d28500c90a24abf0c966b252b4368a48bd4f4355aac9cf20b1dc1c69cc45f19001600145a96e3dcabaa4e2a5da0f9ba2b74c6751c96c7936815a9b3b1e8044b0000000005f5e100
//016b4b18659574f6bca8020000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014b57463cf52d05c5ad3918292a32cb81d9d7616c90000000000000000
//266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8cf9012000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff006b0000001600149cb01e57728a1aba
//b75b53a84c3f3aae7509fbd8145e27b099f8a1ca2fd1c41a5f853dd0000000000000000260bf0bb060c84630000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
//00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000]0"
