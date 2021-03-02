package org.bitcoins.testkitcore.gen

import org.bitcoins.core.protocol._
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

  def bitcoinAddress: Gen[BitcoinAddress] =
    Gen.oneOf(p2pkhAddress, p2shAddress, bech32Address)

  def address: Gen[Address] =
    Gen.oneOf(p2pkhAddress, p2shAddress, bech32Address)
}

object AddressGenerator extends AddressGenerator
