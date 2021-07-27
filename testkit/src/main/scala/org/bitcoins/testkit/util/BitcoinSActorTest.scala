package org.bitcoins.testkit.util

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestKitBase}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{BeforeAndAfterAll}

trait BitcoinSActorTest
    extends FixtureAsyncFlatSpec
    with Matchers
    with TestKitBase
    with BeforeAndAfterAll
    with ImplicitSender {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
}

trait BitcoinSActorFixtureWithDLCWallet
    extends BitcoinSActorTest
    with BitcoinSWalletTest {

  implicit override val system: ActorSystem = ActorSystem(
    s"${getClass.getSimpleName}-${System.currentTimeMillis()}")

  override def afterAll(): Unit = {
    super[BitcoinSWalletTest].afterAll()
    super[BitcoinSActorTest].afterAll()
  }
}
