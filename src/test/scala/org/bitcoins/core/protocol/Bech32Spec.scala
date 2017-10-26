package org.bitcoins.core.protocol

import org.bitcoins.core.gen.{AddressGenerator, ChainParamsGenerator, ScriptGenerators}
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.{Random, Success}

class Bech32Spec extends Properties("Bech32Spec") {

  property("serialization symmetry") = {
    Prop.forAll(ScriptGenerators.witnessScriptPubKeyV0,ChainParamsGenerator.networkParams) { case ((witSPK,_),network) =>
        val addr = Bech32Address(witSPK,network)
        val spk = addr.flatMap(a => Bech32Address.fromString(a.value))
        spk == Success(witSPK)
    }
  }

  property("checksum must not work if we modify a char") = {
    Prop.forAll(AddressGenerator.bech32Address) { case addr : Bech32Address =>
      val old = addr. value
      val rand = Math.abs(Random.nextInt)
      val idx = rand % old.size
      val replacementChar = Bech32Address.charset(rand % Bech32Address.charset.size)
      val (f,l) = old.splitAt(idx)
      val replaced = f ++ Seq(replacementChar) ++ l.tail
      //should fail because we replaced a char in the addr, so checksum invalid
      Bech32Address.fromString(replaced).isFailure
    }
  }
}
