package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt64
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

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
    val bufferedSource =
      io.Source.fromURL(getClass.getResource("/bigsize_encoding.json"))
    try {
      val builder = new StringBuilder
      bufferedSource.getLines().foreach(builder.append)
      val tests = ujson.read(builder.result()).arr.toVector
      tests.foreach { test =>
        val obj = test.obj
        val name = obj("name").str
        val num = BigInt(obj("value").str)
        val bytes = ByteVector.fromValidHex(obj("bytes").str)
        assert(BigSizeUInt(num).bytes == bytes, name)
      }
    } finally {
      bufferedSource.close()
    }
  }

  it must "pass decoding tests" in {
    val bufferedSource =
      io.Source.fromURL(getClass.getResource("/bigsize_decoding.json"))
    try {
      val builder = new StringBuilder
      bufferedSource.getLines().foreach(builder.append)
      val tests = ujson.read(builder.result()).arr.toVector
      tests.foreach { test =>
        val obj = test.obj
        val name = obj("name").str
        val numStr = obj("value").str
        val bytes = ByteVector.fromValidHex(obj("bytes").str)
        if (numStr.nonEmpty) {
          assert(BigSizeUInt(bytes).num == UInt64(BigInt(numStr)), name)
        } else {
          Try {
            assertThrows[IllegalArgumentException] {
              BigSizeUInt(bytes)
            }
          } match {
            case Failure(err)     => fail(obj("exp_error").str, err)
            case Success(success) => success
          }
        }
      }
    } finally {
      bufferedSource.close()
    }
  }
}
