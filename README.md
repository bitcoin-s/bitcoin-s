
# Bitcoin-S-Core

This is the core functionality of bitcoin-s. 

This repostitory includes the following functionality:
  - Native Scala objects for various protocol types (Transaction, TransactionInput, ScriptSignatures...)
  - Serializers and deserializers for bitcoin data structures mentioned above
  - An implementation of Bitcoin's Script programming language 
    - This passes all of the test cases found inside of script_tests.json on the Bitcoin Core repo
    - Currently up to date through OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY still needs to be implemented
  - 90% test coverage throughout the codebase to ensure high quality code. 
  - Functions documented with Scaladocs for user friendliness 

# Examples

Here is an example scala console session with bitcoin-core-s

```scala
chris@chris:~/dev/bitcoins-core-chris$ sbt console

scala> import org.bitcoins.protocol.transaction._
import org.bitcoins.protocol.transaction._

scala> import org.bitcoins.protocol.script._
import org.bitcoins.protocol.script._

scala> val simpleRawTransaction = "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"
simpleRawTransaction: String = 0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000

scala> val tx = Transaction(simpleRawTransaction)
tx: org.bitcoins.protocol.transaction.Transaction = TransactionImpl(1,List(TransactionInputImpl(TransactionOutPointImpl(b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc,0),P2PKHScriptSignatureImpl(4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353,List(BytesToPushOntoStackImpl(72), ScriptConstantImpl(30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01), BytesToPushOntoStackImpl(33), ScriptConstantImpl(0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353))),4294967295)),List(TransactionOutputImpl(89994000 ...
```

This example gives us an example of a bitcoin transaction that is encoded in hex format that is deserialized to a native Scala ojbect called a Transaction.
