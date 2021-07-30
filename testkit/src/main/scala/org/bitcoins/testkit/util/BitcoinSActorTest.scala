package org.bitcoins.testkit.util

import akka.testkit.{ImplicitSender, TestKitBase}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.BeforeAndAfterAll

trait BitcoinSActorTest
    extends FixtureAsyncFlatSpec
    with Matchers
    with TestKitBase
    with BeforeAndAfterAll
    with ImplicitSender

trait BitcoinSActorFixtureWithDLCWallet
    extends BitcoinSActorTest
    with BitcoinSWalletTest {

  override def afterAll(): Unit = {
    super[BitcoinSWalletTest].afterAll()
    super[BitcoinSActorTest].afterAll()
  }
}
