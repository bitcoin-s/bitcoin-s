package org.bitcoins.core.number
import org.bitcoins.core.gen.NumberGenerator
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class BasicArithmeticSpec
    extends FlatSpec
    with MustMatchers
    with PropertyChecks {

  // We have to wrap BasicArithmetic instead of doing
  // an anonymous class, that causes overloading confusion
  private case class NumWrapper(underlying: BigInt)
      extends BasicArithmetic[NumWrapper] {

    override def +(n: NumWrapper): NumWrapper =
      NumWrapper(underlying + n.underlying)

    override def -(n: NumWrapper): NumWrapper =
      NumWrapper(underlying - n.underlying)

    override def *(factor: BigInt): NumWrapper =
      NumWrapper(underlying * factor)

    override def *(factor: NumWrapper): NumWrapper =
      NumWrapper(underlying * factor.underlying)
  }

  private val numWrapperGen = for {
    int <- NumberGenerator.bigInts
  } yield NumWrapper(int)

  behavior of "BasicArithmetic"

  it must "multiply safely and unsafely with an int" in {
    PropertyChecks.forAll(NumberGenerator.bigInts, numWrapperGen) { (i, num) =>
      val unsafe = num * i
      val safe = num.multiplySafe(i)
      assert(safe.toOption.contains(unsafe))
    }
  }

  it must "multiply safely and unsafely with itself" in {
    PropertyChecks.forAll(numWrapperGen, numWrapperGen) { (first, second) =>
      val unsafe = first * second
      val safe = first
    }
  }

  it must "add safely and unsafely" in {
    PropertyChecks.forAll(numWrapperGen, numWrapperGen) { (first, second) =>
      val unsafe = first + second
      val safe = first.addSafe(second)
      assert(safe.toOption.contains(unsafe))
    }
  }

  it must "subtract safely and unsafely" in {
    PropertyChecks.forAll(numWrapperGen, numWrapperGen) { (first, second) =>
      val unsafe = first - second
      val safe = first.subtractSafe(second)
      assert(safe.toOption.contains(unsafe))
    }
  }
}
