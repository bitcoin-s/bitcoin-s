package org.bitcoins.testkit.fixtures

import org.scalatest.FutureOutcome

trait EmptyFixture extends BitcoinSFixture {

  final override type FixtureParam = Unit

  final override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    test(())
}
