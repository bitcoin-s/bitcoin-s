package org.bitcoins.rpc.common

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.common.RpcOpts.AddressType
import org.bitcoins.rpc.jsonmodels.RpcScriptType
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
    val pubKey1 = ecPrivKey1.publicKey
    for {
      (client, _) <- clientsF
      address <- client.getNewAddress(addressType = AddressType.Legacy)
      multisig <- client
        .addMultiSigAddress(
          2,
          Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
      decoded <- client.decodeScript(multisig.redeemScript)
    } yield {
      assert(decoded.reqSigs.contains(2))
      assert(decoded.typeOfScript.contains(RpcScriptType.MULTISIG))
      assert(decoded.addresses.get.contains(address))
    }
  }
}
