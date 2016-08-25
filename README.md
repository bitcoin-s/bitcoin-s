[![Build Status](https://travis-ci.org/bitcoin-s/bitcoin-s-core.svg?branch=master)](https://travis-ci.org/bitcoin-s/bitcoin-s-core) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s-core/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s-core?branch=master)

# Bitcoin-S-Core

This is the core functionality of bitcoin-s. 

[Quick Build Guide](BUILD_README.md)

This repostitory includes the following functionality:
  - Native Scala objects for various protocol types (Transaction, TransactionInput, ScriptSignatures...)
  - Serializers and deserializers for bitcoin data structures mentioned above
  - An implementation of Bitcoin's Script programming language 
    - Passes all tests found in Bitcoin Core's regression test suite called [script_test.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_tests.json)
    - Passes all tests inside of Bitcoin Core's transaction regression test suite [tx_valid.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json) / [tx_invalid.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_invalid.json) / 
    [sighash.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/sighash.json)
    - Currently up to date through OP_CHECKSEQUENCEVERIFY
  - 90% test coverage throughout the codebase to ensure high quality code. 
  - Functions documented with Scaladocs for user friendliness 

# Design Principles
  - Immutable data structures everywhere
  - Using Algebraic Data Types to allow the compiler to check for exhaustiveness on match statements
  - Favoring readability over terseness

# TODO
  - Segwit support
  - Java support
  - Android support

# Examples

Here is an example scala console session with bitcoin-core-s

```scala
chris@chris:~/dev/bitcoins-core-chris$ sbt console

scala> import org.bitcoins.core.protocol.transaction._
import org.bitcoins.protocol.transaction._

scala> import org.bitcoins.core.protocol.script._
import org.bitcoins.protocol.script._

scala> val simpleRawTransaction = "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"
simpleRawTransaction: String = 0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000

scala> val tx = Transaction(simpleRawTransaction)
tx: org.bitcoins.core.protocol.transaction.Transaction = TransactionImpl(1,List(TransactionInputImpl(TransactionOutPointImpl(b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc,0),P2PKHScriptSignatureImpl(4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353,List(BytesToPushOntoStackImpl(72), ScriptConstantImpl(30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01), BytesToPushOntoStackImpl(33), ScriptConstantImpl(0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353))),4294967295)),List(TransactionOutputImpl(89994000 ...
```

This gives us an example of a bitcoin transaction that is encoded in hex format that is deserialized to a native Scala object called a Transaction.

Transactions are run through the interpreter to check the validity of the Transaction. These are packaged up into an object called ScriptProgram, which contains the following:
  - The transaction that is being checked
  - The specific input index that it is checking
  - The scriptPubKey for the crediting transaction
  - The flags used to verify the script

Here is an example of a transaction spending a scriptPubKey which is correctly evaluated with our interpreter implementation:

```scala
chris@chris:~/dev/bitcoins-core$ sbt console
[info] Loading project definition from /home/chris/dev/bitcoins-core/project
[info] Set current project to bitcoins (in build file:/home/chris/dev/bitcoins-core/)
[info] Starting scala interpreter...
[info] 
Welcome to Scala version 2.11.7 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_92).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.script._

scala> import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.transaction._

scala> import org.bitcoins.core.script._
import org.bitcoins.core.script._

scala> import org.bitcoins.core.script.interpreter._
import org.bitcoins.core.script.interpreter._

scala> import org.bitcoins.core.policy._
import org.bitcoins.core.policy._

scala> val spendingTx = Transaction("0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000")
spendingTx: org.bitcoins.core.protocol.transaction.Transaction = TransactionImpl(1,List(TransactionInputImpl(TransactionOutPointImpl(b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc,0),P2PKHScriptSignatureImpl(4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353,List(BytesToPushOntoStackImpl(72), ScriptConstantImpl(30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01), BytesToPushOntoStackImpl(33), ScriptConstantImpl(0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353))),4294967295)),List(TransactionOutputImpl(8...
scala> val scriptPubKey = ScriptPubKey("76a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac")
scriptPubKey: org.bitcoins.core.protocol.script.ScriptPubKey = P2PKHScriptPubKeyImpl(76a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac,List(OP_DUP, OP_HASH160, BytesToPushOntoStackImpl(20), ScriptConstantImpl(31a420903c05a0a7de2de40c9f02ebedbacdc172), OP_EQUALVERIFY, OP_CHECKSIG))

scala> val inputIndex = 0
inputIndex: Int = 0

scala> val program = ScriptProgram(spendingTx,scriptPubKey,inputIndex, Policy.standardScriptVerifyFlags)
program: org.bitcoins.core.script.PreExecutionScriptProgram = PreExecutionScriptProgramImpl(TransactionSignatureComponentImpl(TransactionImpl(1,List(TransactionInputImpl(TransactionOutPointImpl(b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc,0),P2PKHScriptSignatureImpl(4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353,List(BytesToPushOntoStackImpl(72), ScriptConstantImpl(30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01), BytesToPushOntoStackImpl(33), ScriptConstantImpl(0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079...
scala> ScriptInterpreter.run(program)
res0: org.bitcoins.core.script.result.ScriptResult = ScriptOk
```
# Running tests

To run the entire test suite all you need to do is run the following command
```scala 

chris@chris:~/dev/bitcoins-core$ sbt test
[info] Run completed in 8 seconds, 805 milliseconds.
[info] Total number of tests run: 613
[info] Suites: completed 91, aborted 0
[info] Tests: succeeded 613, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 17 s, completed May 7, 2016 1:11:34 PM
chris@chris:~/dev/bitcoins-core$ 
```

To run a specific suite of tests you can specify the suite name in the following way
```scala
chris@chris:~/dev/bitcoins-core$ sbt
> test-only *ScriptInterpreterTest*
[info] ScriptInterpreterTest:
[info] ScriptInterpreter
[info] - must evaluate all the scripts from the bitcoin core script_tests.json
[info] Run completed in 8 seconds, 208 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
>
```


