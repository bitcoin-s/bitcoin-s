package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.{
  DecodeScriptResultPreV22,
  DecodeScriptResultV22
}
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class UtilRpcTest extends BitcoindRpcTest {

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  behavior of "RpcUtilTest"

  it should "be able to validate a bitcoin address" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      validation <- client.validateAddress(address)
    } yield assert(validation.isvalid)
  }

  it should "be able to decode a reedem script" in {
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <-
        client
          .createMultiSig(2, Vector(pubKey1, pubKey2), AddressType.Legacy)
      decoded <- client.decodeScript(multisig.redeemScript)
    } yield {
      decoded match {
        case decodedPreV22: DecodeScriptResultPreV22 =>
          assert(decodedPreV22.reqSigs.exists(_ == 2))
          assert(decoded.typeOfScript.exists(_ == ScriptType.MULTISIG))
          assert(decodedPreV22.addresses.get.exists(_ == address))
        case decodedV22: DecodeScriptResultV22 =>
          assert(decodedV22.typeOfScript.contains(ScriptType.MULTISIG))
      }

    }
  }
}
