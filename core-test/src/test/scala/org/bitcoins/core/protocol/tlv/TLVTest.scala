package org.bitcoins.core.protocol.tlv

import org.bitcoins.testkit.core.gen.TLVGen
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TLVTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

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
}
