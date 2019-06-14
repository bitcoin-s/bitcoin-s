package org.bitcoins.testkit.core.gen.p2p
import org.scalacheck.Gen
import org.bitcoins.core.p2p.NetworkIpAddress
import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.core.p2p.ServiceIdentifier
import java.net.InetAddress
import org.bitcoins.core.p2p.NetworkPayload

object P2PGenerator {

  /** Generates a valid P2P network message */
  def message: Gen[NetworkPayload] =
    Gen.oneOf(ControlMessageGenerator.controlMessage,
              DataMessageGenerator.dataMessage)

  def inetAddress: Gen[InetAddress] = {
    def ipRangeNum = Gen.choose(0, 255)
    for {
      first <- ipRangeNum
      second <- ipRangeNum
      third <- ipRangeNum
      fourth <- ipRangeNum
    } yield {
      // as long as we don't pass in a host name no IO is performed
      // https://stackoverflow.com/questions/5571744/java-convert-a-string-representing-an-ip-to-inetaddress
      InetAddress.getByName(s"$first.$second.$third.$fourth")
    }

  }

  def networkIpAddress: Gen[NetworkIpAddress] = {
    for {
      time <- NumberGenerator.uInt32s
      services <- serviceIdentifier
      address <- inetAddress
      port <- Gen.choose(1025, 64000)
    } yield NetworkIpAddress(time, services, address, port)
  }

  def serviceIdentifier: Gen[ServiceIdentifier] = {
    for {
      num <- NumberGenerator.uInt64
    } yield ServiceIdentifier(num)
  }
}
