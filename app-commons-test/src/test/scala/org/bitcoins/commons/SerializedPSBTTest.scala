package org.bitcoins.commons

import org.bitcoins.commons.jsonmodels.{SerializedPSBT, SerializedTransaction}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.psbt.{GlobalPSBTRecord, OutputPSBTRecord, PSBT}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class SerializedPSBTTest extends BitcoinSUnitTest {

  behavior of "SerializedPSBT"

  it must "correctly decode a psbt" in {
    val psbt = PSBT.fromBase64(
      "cHNidP9Fq2AoiZroZUZZ7/Fl0n4dcF8zKWfpD3QzRcAm1QTxUQzzGnHjM5xU+xYUvYSPokH86tLWHVVmhrOQE2d//fPeu6Px6r0LW2DbYkBubiYldhPvO50/3M0wHtHncJ6w/UmdpFVMt/z1iQfbH9U4bq6iLS930BnOiRlc0KX8DQmDnKFdTdiyceBPOmWKSJT+cR1RIQabSiKY6plO4jkSbZ2yFGMBAP3dAgIAAAABG2Z29d/AhiGmmjjrgO9p0LwwYozRbr404YcnQQ+LTQMEAAAAAAAAAAADGQVGPusCAAD9vwFjVyEDkIsYrb9OJzC2oJUtRDntXxhNo7cZeByTHPkuJeCnAmshA4B9R4qr45hWk4fD3F+0um+wJVmP8kFRyXWflgEYKi61IQMLERF8Yx7x85yRmZyD7888+GENPbV2xACyxsAbEJoZpCEDLfkiP2NbzOWOTrM5qIiGFWLDrAYEoYj+6B/p474XWbQhAuNDwvhwWxJHuG/S7CS/bsk/wr8pR9LzCm8eEEqoQ58NIQPlPMn3Y69Fovcn4cexpU9r1whI0yWZ1OwvYHlsngyAyCECyGo6/HPABsWFUwjKuE+mG57duFkwbk17rug85rd0MCohA8ObOSpT1WPHeKKjc3MwDtFM8xr+LRlrDETYazNJhawyIQNqTHMWXX5dNDAuY71BaXfFF8yoHEaQDnDyNudqgfuMQSEC410iVARHWVmvKpcQUFJJx/H41G2/9sJ5jvRnyYIXwQIhA4APXyr35MW/4PPuMRtYg+caUGFqk+12x9wW+JFxHqnwIQMiS1b6HfL93efBgysG0PrlMPwA7eKMSFexdnOGHhOQP1yuZwXdzsmnALF1dqkUCzczWAtdSewwYE3SNlcLhnxJInGIrGjelDmdMwkAABYAFO26WaDgWxcVeSe4lfoysDTitaKJp/r3t2YNAAC4Y1IhA6WEMWgMOWjUk1FZvxYDTRvyVZKDWeCKsBvgJruAngCOIQKeg5DPhM1ghA26JAP1gvhyZ7K/Z2ttavmOd7AyFv2OyyEDjgniAeDz5qiX/5tyDxMJcyMN4yRoYXZwfFgT9x6iKqghAxmojRQWODhnuwBaEtxJHK9AlJXUkPjIOs4o08raE5zfVK5nBF6KYiOxdSEDvbxvoF42LUei1lcblCHsUzeXo7Wr1zKxw5agIRZVWqysaAAAAAAAAQMEggAAAAAAA6jMPk3fqYXY2pAV+YsGMMB2CAwXDBpwMObcmgEz8M0NO07ZYF60Sf8jj0zms7HOPzU3tBtRMROIXzyZreEJOfmqBIZKk6tg78xgjIW8mR+WuQAA")

    val tx = psbt.transaction
    val decoded = SerializedPSBT.decodePSBT(psbt)

    assert(decoded.global.tx == SerializedTransaction.decodeRawTransaction(tx))
    assert(decoded.global.version == UInt32.zero)
    assert(
      decoded.global.unknowns == Vector(GlobalPSBTRecord.Unknown(
        hex"ab6028899ae8654659eff165d27e1d705f332967e90f743345c026d504f1510cf31a71e3339c54fb1614bd848fa241fcead2d61d556686b39013677ffdf3debba3f1eabd0b",
        hex"60db62406e6e26257613ef3b9d3fdccd301ed1e7709eb0fd499da4554cb7fcf58907db1fd5386eaea22d2f77d019ce89195cd0a5fc0d09839ca15d4dd8b271e04f3a658a4894fe711d5121069b4a2298ea994ee239126d9db21463"
      )))

    assert(decoded.inputs.size == 1)
    assert(decoded.inputs.head.bip32Paths.isEmpty)
    assert(decoded.inputs.head.finalizedScriptSig.isEmpty)
    assert(decoded.inputs.head.finalizedScriptWitness.isEmpty)
    assert(decoded.inputs.head.nonWitnessUtxo.isEmpty)
    assert(decoded.inputs.head.witnessUtxo.isEmpty)
    assert(decoded.inputs.head.redeemScript.isEmpty)
    assert(decoded.inputs.head.witScript.isEmpty)
    assert(decoded.inputs.head.proofOfReservesCommitment.isEmpty)
    assert(decoded.inputs.head.signatures.isEmpty)
    assert(decoded.inputs.head.unknowns.isEmpty)
    assert(
      decoded.inputs.head.sigHashType
        .contains(HashType.sigHashNoneAnyoneCanPay))

    assert(decoded.outputs.size == 3)
    assert(decoded.outputs.head.bip32Paths.isEmpty)
    assert(decoded.outputs.head.redeemScript.isEmpty)
    assert(decoded.outputs.head.witScript.isEmpty)
    assert(decoded.outputs.head.unknowns.isEmpty)
    assert(decoded.outputs(1).bip32Paths.isEmpty)
    assert(decoded.outputs(1).redeemScript.isEmpty)
    assert(decoded.outputs(1).witScript.isEmpty)
    assert(
      decoded.outputs(1).unknowns == Vector(OutputPSBTRecord.Unknown(
        hex"a8cc3e",
        hex"dfa985d8da9015f98b0630c076080c170c1a7030e6dc9a0133f0cd0d3b4ed9605eb449ff238f4ce6b3b1ce3f3537b41b513113885f3c99ade10939f9aa04864a93ab60efcc608c85bc991f96b9"
      )))
    assert(decoded.outputs.last.bip32Paths.isEmpty)
    assert(decoded.outputs.last.redeemScript.isEmpty)
    assert(decoded.outputs.last.witScript.isEmpty)
  }

  it must "correctly decode a finalized psbt" in {
    val psbt = PSBT.fromBase64(
      "cHNidP8BAP1FAQIAAAABGYuEj8rH1dQ8DYG/wIJoOmA07cMG3qUA2joduT39u3QBAAAAAEhEAAAGZi6ggGgAAAAqBOhp5BGydSEDT4d2r4kKjBDT7N3xtlQRvQOoZMDnvl9kcu+Yz0N+zOWsB9sJYW0AAAAmaiSqIantmOy8chX8u/gjrWjoJwzItqupL01TN2d15WGchnbpGlW08zmpugUAAAcEuQUHI7J1tyGqaVkFAABJZCEDd0QjA5VNPMrHshsJ5ToPa6aXjJClUKFubu0Z3qUgGCmsZyEDCXN1QPwcYCN+5PBjESbL9eI3/Fd2SW/L1kOKOJ6lAsqsaMnuN1dBTAAAFlMU75XVn2xj3vMdoWxBKP6yYswBfsdaZnFQHAAAACZqJKohqe2Q60JHII6xx+YrJFTmibrNi7e8pICEfsbYEMYNKhOtMwAAAAAAAQCKAgAAAAAC7ekoBUtTAABQYyED6l4Vptz8do0rhD+X5Xlmyl5Wwi/fM/EwJmYE/LE4XWhnBfcyAqcAsnUhA/spF0dsJg2ZiBFYnA80MM2Pxou90wok6B8hA44T/vJlaKy4YEecR1gAAB4CSESydXapFHyp4KCUiJddycf+SqnAZI21/aJFiKwAAAAAAQdqRzBEAiBMdpIM1wDaIgn1BJarrd27uzx9kcTixUzQMFGbb0KTsQIgBxNIy+cxhoe1Bnxy/X3+ankfNtC4ydjJcMHlxV0MJqOAIQJUjewAnrbEt88DGQklYoDSLGlNS5Z2C6ukMaVGy+p6EgAAAAAAAAA=")
    val tx = psbt.transaction
    val decoded = SerializedPSBT.decodePSBT(psbt)

    assert(decoded.global.tx == SerializedTransaction.decodeRawTransaction(tx))
    assert(decoded.global.version == UInt32.zero)

    assert(decoded.inputs.size == 1)
    assert(decoded.inputs.head.bip32Paths.isEmpty)
    assert(decoded.inputs.head.finalizedScriptSig.nonEmpty)
    assert(decoded.inputs.head.finalizedScriptWitness.isEmpty)
    assert(decoded.inputs.head.nonWitnessUtxo.nonEmpty)
    assert(decoded.inputs.head.witnessUtxo.isEmpty)
    assert(decoded.inputs.head.redeemScript.isEmpty)
    assert(decoded.inputs.head.witScript.isEmpty)
    assert(decoded.inputs.head.proofOfReservesCommitment.isEmpty)
    assert(decoded.inputs.head.signatures.isEmpty)
    assert(decoded.inputs.head.unknowns.isEmpty)
  }
}
