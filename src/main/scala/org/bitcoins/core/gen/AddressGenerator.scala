package org.bitcoins.core.gen

import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress, P2PKHAddress, P2SHAddress}
import org.scalacheck.Gen

/**
  * Created by chris on 6/12/17.
  */
sealed trait AddressGenerator {

  def p2pkhAddress: Gen[P2PKHAddress] = for {
    hash <- CryptoGenerators.sha256Hash160Digest
    network <- ChainParamsGenerator.networkParams
    addr = P2PKHAddress(hash,network)
  } yield addr

  def p2shAddress: Gen[P2SHAddress] = for {
    hash <- CryptoGenerators.sha256Hash160Digest
    network <- ChainParamsGenerator.networkParams
    addr = P2SHAddress(hash,network)
  } yield addr

  def bech32Address: Gen[Bech32Address] = for {
    (witSPK,_) <- ScriptGenerators.witnessScriptPubKey
    network <- ChainParamsGenerator.networkParams
    addr = Bech32Address(witSPK,network)
  } yield addr.get

  def address: Gen[BitcoinAddress] = Gen.oneOf(p2pkhAddress,p2shAddress,bech32Address)
}

object AddressGenerator extends AddressGenerator