package org.bitcoins.rpc.v22

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedV22

class BitcoindV22RpcClientTest extends BitcoindFixturesFundedCachedV22 {

  behavior of "BitcoindV22RpcClient"

  it should "be able to start a V22 bitcoind instance" in {
    client: BitcoindV22RpcClient =>
      for {
        v <- client.version
      } yield assert(v == BitcoindVersion.V22)
  }

  it should "be able to get network info" in {
    freshClient: BitcoindV22RpcClient =>
      for {
        info <- freshClient.getNetworkInfo
      } yield {
        assert(info.networkactive)
        assert(info.localrelay)
      }
  }

  it should "be able to decode a reedem script" in {
    client: BitcoindV22RpcClient =>
      val ecPrivKey1 = ECPrivateKey.freshPrivateKey
      val pubKey1 = ecPrivKey1.publicKey
      for {
        address <- client.getNewAddress(addressType = AddressType.Legacy)
        multisig <-
          client
            .addMultiSigAddress(
              2,
              Vector(Left(pubKey1), Right(address.asInstanceOf[P2PKHAddress])))
        decoded <- client.decodeScript(multisig.redeemScript)
      } yield {
        assert(decoded.typeOfScript.contains(ScriptType.MULTISIG))
        // these fields are deprecated since v22
        assert(decoded.reqSigs.isEmpty)
        assert(decoded.addresses.isEmpty)
      }
  }

}
