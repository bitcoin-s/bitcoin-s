package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BigSizeJsonTestVectors._
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector
import ujson.Value
import upickle.default._

import scala.util.{Failure, Success, Try}

class BigSizeUIntTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "BigSizeUInt"

  it must "have serialization symmetry" in {
    forAll(NumberGenerator.bigSizeUInt) { num =>
      assert(BigSizeUInt(num.bytes) == num)
    }
  }

  it must "fail to parse an empty ByteVector" in {
    assertThrows[IllegalArgumentException] {
      BigSizeUInt(ByteVector.empty)
    }
  }

  it must "pass encoding tests" in {
    val tests = read[Vector[EncodeTestVector]](BigSizeJsonTestVectors.encode)
    tests.foreach { test =>
      assert(BigSizeUInt(test.value).bytes == test.bytes, test.name)
    }
  }

  it must "pass decoding tests" in {
    val tests = read[Vector[DecodeTestVector]](BigSizeJsonTestVectors.decode)
    tests.foreach { test =>
      if (test.value.nonEmpty) {
        assert(BigSizeUInt(test.bytes).num == UInt64(BigInt(test.value)),
               test.name)
      } else {
        Try {
          assertThrows[IllegalArgumentException] {
            BigSizeUInt(test.bytes)
          }
        } match {
          case Failure(err)     => fail(test.expectedErrorOpt.get, err)
          case Success(success) => success
        }
      }
    }
  }
}

object BigSizeJsonTestVectors {

  case class EncodeTestVector(name: String, value: BigInt, bytes: ByteVector)

  implicit
  val encodeTestVectorR: Reader[EncodeTestVector] = reader[Value].map { value =>
    val obj = value.obj
    val name = obj("name").str
    val num = BigInt(obj("value").str)
    val bytes = ByteVector.fromValidHex(obj("bytes").str)

    EncodeTestVector(name, num, bytes)
  }

  val encode: String = """[
                         |  {
                         |    "name": "zero",
                         |    "value": "0",
                         |    "bytes": "00"
                         |  },
                         |  {
                         |    "name": "one byte high",
                         |    "value": "252",
                         |    "bytes": "fc"
                         |  },
                         |  {
                         |    "name": "two byte low",
                         |    "value": "253",
                         |    "bytes": "fd00fd"
                         |  },
                         |  {
                         |    "name": "two byte high",
                         |    "value": "65535",
                         |    "bytes": "fdffff"
                         |  },
                         |  {
                         |    "name": "four byte low",
                         |    "value": "65536",
                         |    "bytes": "fe00010000"
                         |  },
                         |  {
                         |    "name": "four byte high",
                         |    "value": "4294967295",
                         |    "bytes": "feffffffff"
                         |  },
                         |  {
                         |    "name": "eight byte low",
                         |    "value": "4294967296",
                         |    "bytes": "ff0000000100000000"
                         |  },
                         |  {
                         |    "name": "eight byte high",
                         |    "value": "18446744073709551615",
                         |    "bytes": "ffffffffffffffffff"
                         |  }
                         |]""".stripMargin

  case class DecodeTestVector(
      name: String,
      value: String,
      bytes: ByteVector,
      expectedErrorOpt: Option[String])

  implicit
  val decodeTestVectorR: Reader[DecodeTestVector] = reader[Value].map { value =>
    val obj = value.obj
    val name = obj("name").str
    val num = obj("value").str
    val bytes = ByteVector.fromValidHex(obj("bytes").str)
    val expectedErrorOpt = Try(obj("exp_error").str).toOption

    DecodeTestVector(name, num, bytes, expectedErrorOpt)
  }

  val decode: String = """[
                         |  {
                         |    "name": "zero",
                         |    "value": "0",
                         |    "bytes": "00"
                         |  },
                         |  {
                         |    "name": "one byte high",
                         |    "value": "252",
                         |    "bytes": "fc"
                         |  },
                         |  {
                         |    "name": "two byte low",
                         |    "value": "253",
                         |    "bytes": "fd00fd"
                         |  },
                         |  {
                         |    "name": "two byte high",
                         |    "value": "65535",
                         |    "bytes": "fdffff"
                         |  },
                         |  {
                         |    "name": "four byte low",
                         |    "value": "65536",
                         |    "bytes": "fe00010000"
                         |  },
                         |  {
                         |    "name": "four byte high",
                         |    "value": "4294967295",
                         |    "bytes": "feffffffff"
                         |  },
                         |  {
                         |    "name": "eight byte low",
                         |    "value": "4294967296",
                         |    "bytes": "ff0000000100000000"
                         |  },
                         |  {
                         |    "name": "eight byte high",
                         |    "value": "18446744073709551615",
                         |    "bytes": "ffffffffffffffffff"
                         |  },
                         |  {
                         |    "name": "two byte not canonical",
                         |    "value": "",
                         |    "bytes": "fd00fc",
                         |    "exp_error": "decoded bigsize is not canonical"
                         |  },
                         |  {
                         |    "name": "four byte not canonical",
                         |    "value": "",
                         |    "bytes": "fe0000ffff",
                         |    "exp_error": "decoded bigsize is not canonical"
                         |  },
                         |  {
                         |    "name": "eight byte not canonical",
                         |    "value": "",
                         |    "bytes": "ff00000000ffffffff",
                         |    "exp_error": "decoded bigsize is not canonical"
                         |  },
                         |  {
                         |    "name": "two byte short read",
                         |    "value": "",
                         |    "bytes": "fd00",
                         |    "exp_error": "unexpected EOF"
                         |  },
                         |  {
                         |    "name": "four byte short read",
                         |    "value": "",
                         |    "bytes": "feffff",
                         |    "exp_error": "unexpected EOF"
                         |  },
                         |  {
                         |    "name": "eight byte short read",
                         |    "value": "",
                         |    "bytes": "ffffffffff",
                         |    "exp_error": "unexpected EOF"
                         |  },
                         |  {
                         |    "name": "one byte no read",
                         |    "value": "",
                         |    "bytes": "",
                         |    "exp_error": "EOF"
                         |  },
                         |  {
                         |    "name": "two byte no read",
                         |    "value": "",
                         |    "bytes": "fd",
                         |    "exp_error": "unexpected EOF"
                         |  },
                         |  {
                         |    "name": "four byte no read",
                         |    "value": "",
                         |    "bytes": "fe",
                         |    "exp_error": "unexpected EOF"
                         |  },
                         |  {
                         |    "name": "eight byte no read",
                         |    "value": "",
                         |    "bytes": "ff",
                         |    "exp_error": "unexpected EOF"
                         |  }
                         |]""".stripMargin
}
