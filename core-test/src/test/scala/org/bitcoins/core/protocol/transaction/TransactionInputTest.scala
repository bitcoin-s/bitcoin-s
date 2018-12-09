package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.script.{
  EmptyScriptSignature,
  P2PKScriptSignature
}
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 3/30/16.
  */
class TransactionInputTest extends FlatSpec with MustMatchers {

  "TransactionInput" must "define an empty transaction input" in {
    EmptyTransactionInput.previousOutput must be(EmptyTransactionOutPoint)
    EmptyTransactionInput.scriptSignature must be(EmptyScriptSignature)
    EmptyTransactionInput.sequence must be(TransactionConstants.sequence)
  }

  it must "write a transaction output to hex accurately" in {
    val input = TransactionInput(TestUtil.rawTxInput)

    input.hex must be(TestUtil.rawTxInput)
  }

  it must "read a transaction input with an empty script signature" in {
    val rawInput =
      "377758adf3337c927d37e3f28c1c83cbc837075e1fc7e500e9b801c69da626290bf565740003432d8a"
    val input = TransactionInput(rawInput)
    input.hex must be(rawInput)
    TransactionInput(input.hex) must be(input)
  }

  it must "read and write a transaction input with a large script signature" in {
    val rawInput =
      "cffe265210b42971f9642adcf88f6e6aaa40e6baec30037ce0088383b47c4f28031e306ffd2403004730450220259683be7d97e0a0f611926057bbd3fc8c5f886a06d39ec5ce330a87bcb33fa9022100ae6eb066c89dee2b5e4fefaa03450cfa6960508c2dc1c82f206bd66b9e8d3913463044022030d398ef4a69646ae40a84af9634e0608f4882269b92777f239c146555a3d4b602200ff23130697bca6bb7be3b69962387d6caeb25e198dc59969328d993119f9b5147304502206c0abf639640f56ab85c9800a2555be92569682a96bde261cfd9f1254578ee90022100c4b42232041d5f228ff832d1615151949d320eaf85bef9389463744f996e46b9473045022058d8a8ac02b5bd7139a99787dd42067e2de3a4e613f0878318aefcac4f13768f022100f620e66d3cc96230a0602e803a3f790861e1f66f72028e2ed79dc0ec3c24b98e4d01025f2103a2ae334a00a2a8b0191b41829b72eac3d4f207f87ea5b102383ed58716a43d452102351e70b50c30827ff18fde7b7458c991f74a938878f94c5f1384840b0775be2b210210c5d0c5528f61def05d0893e7b3145c5c5c58c4771c74de74470c69c8d86da121032999f0c7acd64f80b318beae5be7cedb4a72b0f370dcaa8ab9a0be1190df001821034afa6da2e9a4fbf84b33e5c35c81b0396542b4bf5eddaef0fb7e760833047fca21022875fe400ea9200ca013447c9dfe68e72d09e39a14c2b80f5beba35db00eb3a621031969406b809d0413d156fd2eb449cdc7b4b6fd431a3179abbd73cc2dd8a675bc21022f017723760552871039ba59678db384a1d195f2bcb6207a4514d1b86af0d6f42102a2d346e46656e5070433874521aaf24423f78438ec644c975ce8e2a32f35ef1d21027138384fcde2ae4aee8052f576227c3a158a09be5e8ee9df5145a39a06b5f6f32103d30805f2a4fc3d816b0f37b5500ffa7f59603ef45680c7f75991342d53af30d02103a27ef20b51281a98dcf55afdf53182d6a07c0d48afa511fa52d352faf79169b52102c50b2f80b12f0bbd304802916f906e366d017083d381cd45d64074fdb04b1b3d2103e3a5a4df1fc467589ed98d94f90cc1d41a2ac5a40b68a6c489cc1e4af04c5bb421039af4e1b6903db40e9cfe2c14cdd490c385d1339b0b7386719e277bbd5492378d5fae0f25e5bb"
    val input = TransactionInput(rawInput)
    input.hex must be(rawInput)
    (TransactionInput(input.hex) == input) must be(true)
  }

  it must "serialize and deserialize a coinbase input" in {
    val c = CoinbaseInput(
      P2PKScriptSignature(
        "4847304502210092d4e6183970b5e082d87563afbcfb3e1f38e801d89f036fd2935c394d6cc364022032b2a419e19f00b6f32f88c4427cf5e2a97f298b7d4e45efb5f723d84257ca03"),
      TransactionConstants.sequence
    )
    TransactionInput(c.previousOutput, c.scriptSignature, c.sequence) must be(c)
    c.hex must be(
      TransactionInput(c.previousOutput, c.scriptSignature, c.sequence).hex)
  }

}
