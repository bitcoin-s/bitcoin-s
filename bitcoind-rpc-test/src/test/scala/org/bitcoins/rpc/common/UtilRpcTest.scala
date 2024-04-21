package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.DecodeScriptResultV22
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairNewest

class UtilRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "RpcUtilTest"

  it should "be able to validate a bitcoin address" in { case nodePair =>
    val (client, otherClient) = (nodePair.node1, nodePair.node2)
    for {
      address <- otherClient.getNewAddress
      validation <- client.validateAddress(address)
    } yield assert(validation.isvalid)
  }

  it should "be able to decode a reedem script" in { case nodePair =>
    val (client, _) = (nodePair.node1, nodePair.node2)
    val ecPrivKey1 = ECPrivateKey.freshPrivateKey
    val ecPrivKey2 = ECPrivateKey.freshPrivateKey
    val pubKey1 = ecPrivKey1.publicKey
    val pubKey2 = ecPrivKey2.publicKey
    for {
      multisig <-
        client
          .createMultiSig(2, Vector(pubKey1, pubKey2), AddressType.Legacy)
      decoded <- client.decodeScript(multisig.redeemScript)
    } yield {
      decoded match {
        case decodedV22: DecodeScriptResultV22 =>
          assert(decodedV22.typeOfScript.contains(ScriptType.MULTISIG))
      }
    }
  }
}
