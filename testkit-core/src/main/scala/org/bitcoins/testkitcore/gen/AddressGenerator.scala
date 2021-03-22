package org.bitcoins.testkitcore.gen

import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.script.WitnessVersion0
import org.scalacheck.Gen

/** Created by chris on 6/12/17.
  */
sealed trait AddressGenerator {

  def p2pkhAddress: Gen[P2PKHAddress] =
    for {
      hash <- CryptoGenerators.sha256Hash160Digest
      network <- ChainParamsGenerator.networkParams
      addr = P2PKHAddress(hash, network)
    } yield addr

  def p2shAddress: Gen[P2SHAddress] =
    for {
      hash <- CryptoGenerators.sha256Hash160Digest
      network <- ChainParamsGenerator.networkParams
      addr = P2SHAddress(hash, network)
    } yield addr

  def bech32Address: Gen[Bech32Address] =
    for {
      (witSPKV0, _) <- ScriptGenerators.witnessScriptPubKeyV0
      network <- ChainParamsGenerator.networkParams
      addr = Bech32Address(witSPKV0, network)
    } yield addr

  def bech32mAddress: Gen[Bech32mAddress] =
    for {
      (witSPK, _) <- ScriptGenerators.witnessScriptPubKey.suchThat(
        _._1.witnessVersion != WitnessVersion0)
      network <- ChainParamsGenerator.networkParams
    } yield Bech32mAddress(witSPK, network)

  def bitcoinAddress: Gen[BitcoinAddress] =
    Gen.oneOf(p2pkhAddress, p2shAddress, bech32Address, bech32mAddress)

  def address: Gen[Address] = bitcoinAddress
}

object AddressGenerator extends AddressGenerator
