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

  it must "conform to the enum single test vector" ignore {
    val url = getClass.getResource("/enum_single_oracle_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the single oracle numerical test" ignore {

    val url = getClass.getResource("/single_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the single oracle numerical hyperbola test" ignore {
    val url =
      getClass.getResource("/single_oracle_numerical_hyperbola_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_3_of_3_test.json test" ignore {
    val url =
      getClass.getResource("/enum_3_of_3_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_3_of_5_test.json test" ignore {
    val url =
      getClass.getResource("/enum_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_test.json test" ignore {
    val url =
      getClass.getResource("/three_of_three_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_with_diff_test.json test" ignore {
    val url =
      getClass.getResource(
        "/three_of_three_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_test.json test" ignore {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_test.json test" ignore {
    val url =
      getClass.getResource("/two_of_five_oracle_numerical_with_diff_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_3_of_5_test.json test" ignore {
    val url =
      getClass.getResource("/enum_and_numerical_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_5_of_5_test.json test" ignore {
    val url =
      getClass.getResource("/enum_and_numerical_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_with_diff_3_of_5_test.json test" ignore {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_3_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the enum_and_numerical_with_diff_5_of_5_test.json test" ignore {
    val url =
      getClass.getResource("/enum_and_numerical_with_diff_5_of_5_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the three_of_three_oracle_numerical_with_diff_diff_nb_digits_test.json test" ignore {
    val url =
      getClass.getResource(
        "/three_of_three_oracle_numerical_with_diff_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_diff_nb_digits_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_five_oracle_numerical_with_diff_diff_nb_digits_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_five_oracle_numerical_with_diff_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_three_oracle_numerical_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_three_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_three_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_three_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_diff_nb_digits_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_diff_nb_digits_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json test" ignore {
    val url =
      getClass.getResource(
        "/two_of_two_oracle_numerical_with_diff_diff_nb_digits_max_value_test.json")
    val jsonString: String = Source.fromURL(url).getLines().mkString
    runTest(jsonString)
  }

  it must "conform to the two_of_two_oracle_numerical_with_diff_diff_nb_digits_test.json test" ignore {
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
