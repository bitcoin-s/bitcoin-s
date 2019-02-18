# Contributing to Bitcoin-S

Bitcoin-S is an open source project where anyone is welcome to contribute. All contributions are encouraged and appreciated, whether that is code, testing, documentation or something else entirely.

## Communication Channels

It's possible to communicate with other developers through a variety of communication channels:

- [Suredbits Slack](https://join.slack.com/t/suredbits/shared_invite/enQtNDEyMjY3MTg1MTg3LTYyYjkwOGUzMDQ4NDAwZjE1M2I3MmQyNWNlZjNlYjg4OGRjYTRjNWUwNjRjNjg4Y2NjZjAxYjU1N2JjMTU1YWM) - Suredbits is a company monetizing APIs through the Lightning Network. Suredbits doesn't own Bitcoin-S, but the Suredbits CEO Chris Stewart is the maintainer of this library. There's a separate Bitcoin-S channel on their Slack, this is probably the easiest way of getting in touch with someone working on this project.
- [Bitcoin-S Gitter](https://gitter.im/bitcoin-s-core/)
- [#bitcoin-scala](https://webchat.freenode.net/?channels=bitcoin-scala) on IRC Freenode

## Documentation

One of the goals of Bitcoin-S is having useful and well-formatted Scaladoc comments on classes,
objects and functions. Here are some useful resources on how to properly format your Scaladoc comments:

- [Scaladoc for library authors](https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html)
- [Guidelines](https://docs.scala-lang.org/style/scaladoc.html) used by the official Scala language Scaladoc

### Bitcoin-S static site

Bitcoin-S comes bundles with it's own web site with documentation about the library. It consists if the generated Scaladoc of the project, as well as the content of `src/site`.

### Working with documentation locally

View generated site:

```bash
$ sbt previewSite
```

### Publishing Bitcoin-S site

```bash
$ sbt ghpagesPushSite
```

Read more on the [`sbt-ghpages`](https://github.com/sbt/sbt-ghpages) sbt plugin.

> Note: some setup is required before doing this the first time  
> From the `sbt-ghpages` documentation:

Before using sbt-ghpages, you must create the gh-pages branch in your repository and push the branch to GitHub. The quick steps are:

```bash
# Using a fresh, temporary clone is safest for this procedure
$ pushd /tmp
$ git clone git@github.com:youruser/yourproject.git
$ cd yourproject

# Create branch with no history or content
$ git checkout --orphan gh-pages
$ git rm -rf .

# Establish the branch existence
$ git commit --allow-empty -m "Initialize gh-pages branch"
$ git push origin gh-pages

# Return to original working copy clone, we're finished with the /tmp one
$ popd
$ rm -rf /tmp/yourproject
```

## Testing

### Property based testing

This library aims to achieve high level of correctness via property based testing. At the simplest level, you can think of property based testing as specifying a invariant that must always hold true. [Here](https://github.com/bitcoin-s/bitcoin-s-core/blob/89fbf35d78046b7ed21fd93fec05bb57cba023bb/src/test/scala/org/bitcoins/core/protocol/transaction/TransactionSpec.scala#L13-L17) is an example of a property in the bitcoin-s-core test suite

```scala
  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.transactions) { tx =>
      Transaction(tx.hex) == tx
  }
```

What this property says is that for every transaction we can generate with [`TransactionGenerators.transactions`](testkit/src/main/scala/org/bitcoins/core/gen/TransactionGenerators.scala) we _must_ be able to serialize it to hex format, then deserialize it back to a transaction and get the original `tx` back.

A more complex example of property based testing is checking that a multi signature transaction was signed correctly (see [`TransactionSignatureCreatorSpec`](core-test/src/test/scala/org/bitcoins/core/crypto/TransactionSignatureCreatorSpec.scala) line 29-34). First we generate a _supposedly_ validly signed multisig transaction with [`TransactionGenerators.signedMultiSigTransaction`](testkit/src/main/scala/org/bitcoins/core/gen/TransactionGenerators.scala) (line 102-108). These transactions have varying `m` of `n` requirements. An interesting corner case if when you have 0 of `n` signatures, which means no signature is required. Property based testing is really good at fleshing out these corner cases. We check to see if this transaction is valid by running it through our [`ScriptInterpreter`](core/src/main/scala/org/bitcoins/core/script/interpreter/ScriptInterpreter.scala). If we have built our functionality correctly the ScriptInterpreter should always return [`ScriptOk`](core/src/main/scala/org/bitcoins/core/script/result/ScriptResult.scala) indicating the script was valid.

```scala
  property("generate valid signatures for a multisignature transaction") =
    Prop.forAllNoShrink(TransactionGenerators.signedMultiSigTransaction) {
      case (txSignatureComponent: TxSigComponent, _)  =>
        //run it through the interpreter
        val program = ScriptProgram(txSignatureComponent)
        val result = ScriptInterpreter.run(program)
        result == ScriptOk
  }
```

### Running tests

To run the entire test suite all you need to do is run the following command

```scala
$ sbt test
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
```

To run a specific suite of tests you can specify the suite name in the following way

```scala
$ sbt testOnly *ScriptInterpreterTest*
[info] ScriptInterpreterTest:
[info] ScriptInterpreter
[info] - must evaluate all the scripts from the bitcoin core script_tests.json
[info] Run completed in 8 seconds, 208 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
```

The command `sbt testQuick` can also be handy. It runs tests that either:

1. Failed previously
2. Has not been run previously
3. Either the test or one of its dependencies has been recompiled

For more information on `testQuick`, see the offical [sbt docs](https://www.scala-sbt.org/1.x/docs/Testing.html#testQuick).
