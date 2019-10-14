package org.bitcoins.core.compat

import org.bitcoins.testkit.util.BitcoinSUnitTest

class CompatEitherTest extends BitcoinSUnitTest {

  it should "create left and right" in {

    val right = Right("Right")
    val compatRight = CompatEither(right)
    assert(compatRight.isInstanceOf[CompatRight[Nothing, String]])
    assert(right.toTry == compatRight.toTry)

    val left = Left(new RuntimeException("Left"))
    val compatLeft = CompatEither(left)
    assert(compatLeft.isInstanceOf[CompatLeft[RuntimeException, Nothing]])
    assert(left.toTry == compatLeft.toTry)
  }

  it should "do traverse operations" in {
    val mappedRight = CompatEither(Right(12)).map(_ => "flower")
    assert(mappedRight == CompatEither(Right("flower")))
    val mappedLeft = CompatEither(Left(12)).map(_ => "flower")
    assert(mappedLeft == CompatEither(Left(12)))

    val flatmappedRight: CompatEither[Int, String] =
      CompatEither(Right(12)).flatMap(_ => CompatEither(Right("flower")))
    assert(flatmappedRight == CompatRight("flower"))
    val flatmappedLeft =
      CompatEither(Left(12)).flatMap(_ => CompatEither(Left("21")))
    assert(flatmappedLeft == CompatLeft(12))

    val foldedRight = CompatEither(Right(12)).fold({ _ =>
      "left"
    }, { _ =>
      "right"
    })
    assert(foldedRight == "right")
    val foldedLeft = CompatEither(Left(12)).fold({ _ =>
      "left"
    }, { _ =>
      "right"
    })
    assert(foldedLeft == "left")
  }

}
