[![Build Status](https://travis-ci.org/bitcoin-s/bitcoin-s-core.svg?branch=master)](https://travis-ci.org/bitcoin-s/bitcoin-s-core) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s-core/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s-core?branch=master)

# Bitcoin-S-Core

This is the core functionality of bitcoin-s. Please join us in #bitcoin-scala on freenode for help/collaboration.

[Quick Build Guide](BUILD_README.md)

This repostitory includes the following functionality:
  - Native Scala objects for various protocol types ([transactions](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala), [inputs](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/protocol/transaction/TransactionInput.scala), [outputs](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/protocol/transaction/TransactionOutput.scala), [scripts signatures](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/protocol/script/ScriptSignature.scala), [scriptpubkeys](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/protocol/script/ScriptPubKey.scala))
  - [Serializers and deserializers for bitcoin data structures mentioned above](https://github.com/bitcoin-s/bitcoin-s-core/tree/master/src/main/scala/org/bitcoins/core/serializers)
  - [An implementation of Bitcoin's Script programming language](https://github.com/bitcoin-s/bitcoin-s-core/tree/master/src/main/scala/org/bitcoins/core/script) 
    - Passes all tests found in Bitcoin Core's regression test suite called [script_test.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/script_tests.json)
    - Passes all tests inside of Bitcoin Core's transaction regression test suite [tx_valid.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json) / [tx_invalid.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_invalid.json) / 
    [sighash.json](https://github.com/bitcoin/bitcoin/blob/master/src/test/data/sighash.json)
  - [Payment channel support](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/main/scala/org/bitcoins/core/channels/Channel.scala)
  - Integration with [bitcoin core's optimized secp256k1](https://github.com/bitcoin-core/secp256k1/) library
  - Consensus rule set up to date through segregated witness
  - A robust set of [generators](https://github.com/bitcoin-s/bitcoin-s-core/tree/master/src/main/scala/org/bitcoins/core/gen), which are used in property based testing
    - These are extremely useful for testing bitcoin applications
    - Here is an example of a specification for our [ECPrivateKey](https://github.com/bitcoin-s/bitcoin-s-core/blob/master/src/test/scala/org/bitcoins/core/crypto/ECPrivateKeySpec.scala)
  - 90% test coverage throughout the codebase to ensure high quality code. 
  - Functions documented with Scaladocs for user friendliness

# Design Principles
  - Immutable data structures everywhere
  - Algebraic Data Types to allow the compiler to check for exhaustiveness on match statements
  - Using [property based testing](http://www.scalatest.org/user_guide/property_based_testing) to test robustness of code 

# TODO
  - [BIP32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) HD keys
  - Java support
  - Android support

# Creating fat jar

Here is how you build a bitcoin-s-core fat jar file. Note this command will run the entire test suite in bitcoin-s-core.

```scala
$ sbt assembly
[info] ScalaCheck
[info] Passed: Total 149, Failed 0, Errors 0, Passed 149
[info] ScalaTest
[info] Run completed in 5 minutes, 33 seconds.
[info] Total number of tests run: 744
[info] Suites: completed 97, aborted 0
[info] Tests: succeeded 744, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[info] Passed: Total 909, Failed 0, Errors 0, Passed 909
[info] Checking every *.class/*.jar file's SHA-1.
[info] Merging files...
[warn] Merging 'META-INF/MANIFEST.MF' with strategy 'discard'
[warn] Strategy 'discard' was applied to a file
[info] SHA-1: 6ea465dcc996cefb68fc334778cac60d892bd7f0
[info] Packaging /home/chris/dev/bitcoin-s-core/target/scala-2.11/bitcoin-s-core-assembly-0.0.1.jar ...
[info] Done packaging.
[success] Total time: 337 s, completed Jul 20, 2017 1:53:11 PM
```

# Examples

Here is an example scala console session with bitcoins-core

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
[info] Elapsed time: 4 min 36.760 sec 
[info] ScalaCheck
[info] Passed: Total 149, Failed 0, Errors 0, Passed 149
[info] ScalaTest
[info] Run completed in 4 minutes, 55 seconds.
[info] Total number of tests run: 744
[info] Suites: completed 97, aborted 0
[info] Tests: succeeded 744, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[info] Passed: Total 909, Failed 0, Errors 0, Passed 909
[success] Total time: 297 s, completed Jul 20, 2017 10:34:16 AM
chris@chris:~/dev/bitcoin-s-core$ 
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


