package org.bitcoins.core.protocol

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.protocol.script.P2SHScriptPubKey
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkitcore.gen.{
  AddressGenerator,
  CryptoGenerators,
  ScriptGenerators
}
import org.scalacheck.{Prop, Properties}

/** Created by chris on 7/21/16.
  */
class BitcoinAddressSpec extends Properties("BitcoinAddressSpec") {

  property("get the same p2sh address no matter what factory function we use") =
    Prop.forAll(ScriptGenerators.randomNonP2SHScriptPubKey) {
      case (scriptPubKey, _) =>
        //we should get the same address no matter which factory function we use
        val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
        P2SHAddress(scriptPubKey, TestNet3) == P2SHAddress(p2shScriptPubKey,
                                                           TestNet3)

    }

  property("All p2sh addresses created from factory functions must be valid") =
    Prop.forAll(ScriptGenerators.randomNonP2SHScriptPubKey) {
      case (scriptPubKey, _) =>
        //we should get the same address no matter which factory function we use
        val addr = P2SHAddress(scriptPubKey, TestNet3)
        P2SHAddress.isValid(addr.toString)
    }

  property(
    "get the same p2pkh address no matter what factory function we use") =
    Prop.forAll(CryptoGenerators.publicKey) { pubKey =>
      val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
      P2PKHAddress(pubKey, TestNet3) == P2PKHAddress(hash, TestNet3)
    }
  property("All p2pkh addresses created from factory functions must be valid") =
    Prop.forAll(CryptoGenerators.publicKey) { pubKey =>
      val addr = P2PKHAddress(pubKey, TestNet3)
      P2PKHAddress.isValid(addr.toString)
    }

  property("serialization symmetry between script and address") = {
    Prop.forAll(AddressGenerator.address) { addr =>
      val spk = addr.scriptPubKey
      val network = addr.networkParameters
      Address.fromScriptPubKey(spk, network) == addr
    }
  }
}
