package org.bitcoins.core.util

import org.bitcoins.crypto.{ECPublicKey, NetworkElement, SchnorrNonce}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class SortedVecTest extends BitcoinSUnitTest {
  behavior of "SortedVec"

  it should "sort correctly on construction with normal orderings" in {
    assert(SortedVec.sort(Vector(1, 2, 3)) == SortedVec(Vector(1, 2, 3)))
    assert(SortedVec.sort(Vector(3, 2, 1)) == SortedVec(Vector(1, 2, 3)))
    assert(
      SortedVec.sort(Vector(3.0, 2.0, 1.123)) == SortedVec(
        Vector(1.123, 2.0, 3.0)))
    assert(
      SortedVec.sort(Vector('c', 'b', 'a')) == SortedVec(Vector('a', 'b', 'c')))
    assert(
      SortedVec.sort(Vector("cab", "ceb", "abc")) == SortedVec(
        Vector("abc", "cab", "ceb")))
    assert(
      SortedVec.sort[SchnorrNonce, NetworkElement](Vector(
        SchnorrNonce(
          "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea"),
        SchnorrNonce(
          "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc")
      ))(SortedVec.networkElementOrd) == SortedVec[SchnorrNonce,
                                                   NetworkElement](Vector(
        SchnorrNonce(
          "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc"),
        SchnorrNonce(
          "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea")
      ))(SortedVec.networkElementOrd))
  }

  it should "fail to construct with unsorted vector under normal orderings" in {
    assertThrows[IllegalArgumentException](SortedVec(Vector(3, 2, 1)))
    assertThrows[IllegalArgumentException](SortedVec(Vector(3.0, 2.0, 1.123)))
    assertThrows[IllegalArgumentException](SortedVec(Vector('c', 'b', 'a')))
    assertThrows[IllegalArgumentException](
      SortedVec(Vector("cab", "ceb", "abc")))
    assertThrows[IllegalArgumentException](
      SortedVec[SchnorrNonce, NetworkElement](Vector(
        SchnorrNonce(
          "c4b89873c8753de3f0a9e94c4a6190badaa983513a6624a3469eb4577904bfea"),
        SchnorrNonce(
          "92efe81609c773d97da2b084eb691f48ef5e926acc6eecd629f80fb1184711bc")
      ))(SortedVec.networkElementOrd))
  }

  it should "sort correctly on construction with custom orderings" in {
    implicit val parityOrd: Ordering[Int] = { case (x: Int, y: Int) =>
      (math.abs(x) % 2) - (math.abs(y) % 2)
    }

    assert(SortedVec.sort(Vector(1, 2, 3, 4)) == SortedVec(Vector(2, 4, 1, 3)))
    assert(SortedVec.sort(Vector(2, 4, 1, 3)) == SortedVec(Vector(2, 4, 1, 3)))

    implicit val remainderOrd: Ordering[Double] = {
      case (x: Double, y: Double) =>
        val diff = (x - x.floor) - (y - y.floor)
        if (diff > 0) 1 else if (diff == 0) 0 else -1
    }

    assert(
      SortedVec.sort(Vector(0.95, 123.123, 5.55)) == SortedVec(
        Vector(123.123, 5.55, 0.95)))
  }

  it should "fail to construct with unsorted vector under custom orderings" in {
    implicit val parityOrd: Ordering[Int] = { case (x: Int, y: Int) =>
      (math.abs(x) % 2) - (math.abs(y) % 2)
    }

    assertThrows[IllegalArgumentException](SortedVec(Vector(1, 2, 3, 4)))

    implicit val remainderOrd: Ordering[Double] = {
      case (x: Double, y: Double) =>
        val diff = (x - x.floor) - (y - y.floor)
        if (diff > 0) 1 else if (diff == 0) 0 else -1
    }

    assertThrows[IllegalArgumentException](
      SortedVec(Vector(0.95, 5.55, 123.123)))
  }

  it should "correctly handle ordered list" in {
    val nonce1 = ECPublicKey.freshPublicKey.schnorrNonce
    val nonce2 = ECPublicKey.freshPublicKey.schnorrNonce
    val nonce3 = ECPublicKey.freshPublicKey.schnorrNonce

    val vec = SortedVec.forOrdered(Vector(nonce1, nonce2, nonce3))

    assertThrows[IllegalArgumentException](
      vec.copy(Vector(nonce2, nonce3, nonce1))(vec.ord))
  }
}
