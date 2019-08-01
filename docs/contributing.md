---
id: contributing
title: Contributing
---

Bitcoin-S is an open source project where anyone is welcome to contribute. All contributions are encouraged and appreciated, whether that is code, testing, documentation or something else entirely.

## Communication Channels

It's possible to communicate with other developers through a variety of communication channels:

- [Suredbits Slack](https://join.slack.com/t/suredbits/shared_invite/enQtNDEyMjY3MTg1MTg3LTYyYjkwOGUzMDQ4NDAwZjE1M2I3MmQyNWNlZjNlYjg4OGRjYTRjNWUwNjRjNjg4Y2NjZjAxYjU1N2JjMTU1YWM) - Suredbits is a company monetizing APIs through the Lightning Network. Suredbits doesn't own Bitcoin-S, but the Suredbits CEO Chris Stewart is the maintainer of this library. There's a separate Bitcoin-S channel on their Slack, this is probably the easiest way of getting in touch with someone working on this project.
- [Bitcoin-S Gitter](https://gitter.im/bitcoin-s-core/)
- [#bitcoin-scala](https://webchat.freenode.net/?channels=bitcoin-scala) on IRC Freenode

## Working on Bitcoin-S applications

Bitcoin-S includes a couple of applications that can be run as standalone executables.
This includes the node, wallet and (partial) blockchain verification modules, as well
as the server that bundles these three together and the CLI used to communicate with
the server. These applications are configured with HOCON files. The file
[`reference.conf`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/resources/reference.conf)
is the basis configuration file, and every option read by Bitcoin-S should be present in
this file. This means that you can copy sections from this file and edit them, to tune
how the application runs on your machine.

One example of things you can tune is logging levels. Lets say you wanted general logging
to happen at the `WARN` level, but the P2P message handling to be logged at `DEBUG`. Your
configuration file would then look like:

```conf
bitcoins-s {
  logging {
    level = warn

    p2p = debug
  }
}
```

### Running the applications

When running the applications configuration placed in `bitcoin-s.conf` in the current
data directory gets picked up. For linux this is by default `$HOME/.bitcoin-s/`, so the
file you should edit would be `$HOME/.bitcoin-s/bitcoin-s.conf`.

### Running tests for the applications

You can place configuration files in the data directory that tests are being run in,
but you can also edit [`reference.conf`](https://github.com/bitcoin-s/bitcoin-s/blob/master/db-commons/src/main/resources/reference.conf).

## Developer productivity

### Bloop

If you're tired of waiting around for sbt all day, there's a new,
cool kid on the block. It is called [Bloop](https://scalacenter.github.io/bloop/),
and it makes compilations in general faster, and in particular
incremental, small compilation units (which greatly help editor
performance). Bloop is a server that runs in the background of
your computer, and keeps several "hot" JVMs running at all
times. These JVMs serve compilation requests. Because the JVMs
are running in the background you avoid the startup lag, and you
also get code that's already [JIT compiled](https://en.wikipedia.org/wiki/Just-in-time_compilation)
for you.

The documentation on Bloops [site](https://scalacenter.github.io/bloop/) is good, but here is the highlights:

1. Install Bloop by doing step 1 & 2 in the [official guide](https://scalacenter.github.io/bloop/setup#universal)
2. Enable the Bloop background daemon
   1. macOS:
   ```bash
   $ brew services start bloop
   ```
   2. Ubuntu:
   ```bash
   $ systemctl --user enable $HOME/.bloop/systemd/bloop.service
   $ systemctl --user daemon-reload
   $ systemctl --user start bloop
   ```
3. Enable shell completion for the Bloop CLI
   1. Bash:
   ```bash
   $ echo '. $HOME/.bloop/bash/bloop' >> $HOME/.bash_profile
   ```
   2. Zsh:
   ```bash
   $ echo 'autoload -U compinit' >> $HOME/.zshrc
   $ echo 'fpath=($HOME/.bloop/zsh $fpath)' >> $HOME/.bashrc
   $ echo 'compinit' >> $HOME/.bashrc
   ```
   3. Fish:
   ```bash
   $ ln -s $HOME/.bloop/fish/bloop.fish ~/.config/fish/completions/bloop.fish
   ```
4. Generate configuration files
   ```bash
   $ sbt bloopInstall
   ```
5. Import Bitcoin-S into IntelliJ again, as a bsp (Build Server Protocol) project (instead of a sbt project). Make sure you're running on the most recent IntelliJ and Scala plugin. See [official docs](https://scalacenter.github.io/bloop/docs/ides/intellij) for details.
6. _(Bonus step):_ Lightning fast recompilations on file save:
   ```bash
   $ bloop compile --project <name of module your're working on> --watch
   ```

Your editor should now be much faster and require less resources :tada:

## Testing

### Property based testing

This library aims to achieve high level of correctness via property based
testing. At the simplest level, you can think of property based testing as
specifying a invariant that must always hold true.
[Here](https://github.com/bitcoin-s/bitcoin-s-core/blob/89fbf35d78046b7ed21fd93fec05bb57cba023bb/src/test/scala/org/bitcoins/core/protocol/transaction/TransactionSpec.scala#L13-L17)
is an example of a property in the bitcoin-s-core test suite

```scala
  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.transactions) { tx =>
      Transaction(tx.hex) == tx
  }
```

What this property says is that for every transaction we can generate with
[`TransactionGenerators.transactions`](/api/org/bitcoins/core/gen/TransactionGenerators)
we _must_ be able to serialize it to hex format, then deserialize it back
to a transaction and get the original `tx` back.

A more complex example of property based testing is checking that a
multisignature transaction was signed correctly (see
[`TransactionSignatureCreatorSpec`](core-test/src/test/scala/org/bitcoins/core/crypto/TransactionSignatureCreatorSpec.scala)
line 29-34). First we generate a _supposedly_ validly signed multisig
transaction with [`TransactionGenerators.signedMultiSigTransaction`](/api/org/bitcoins/testkit/core/gen/TransactionGenerators)
(line 102-108). These transactions have varying `m` of `n` requirements.
An interesting corner case if when you have 0 of `n` signatures, which
means no signature is required. Property based testing is really good at
fleshing out these corner cases. We check to see if this transaction is
valid by running it through our [`ScriptInterpreter`](/api/org/bitcoins/core/script/interpreter/ScriptInterpreter).
If we have built our functionality correctly the `ScriptInterpreter` should
always return [`ScriptOk`](/api/org/bitcoins/core/script/result/ScriptResult)
indicating the script was valid.

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

To run the entire test suite all you need to do is run the following command:

> This takes a long time, and runs a lot of tests that require IO. It may hog your computer at times.

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

For more information on `testQuick`, see the offical
[sbt docs](https://www.scala-sbt.org/1.x/docs/Testing.html#testQuick).

### Coverage

To produce a report that quantifies how much of our code is covered by tests:

```bash
sbt
> coverage
> coreTest/test
> core/coverageReport
```

This generates three different reports: Cobertura, XML and HTML formats.
See the output of your sbt shell to find the location of them.
Open up the HTML file in your browser. You'll now see code coverage
of all files in `core` project.

### CI

Bitcoin-S uses Travis to run tests and deploy library and website builds. Generally
speaking CI has to pass for a PR to get merged. If you make documentation/website only
changes, you can start your PR title with `Docs:`. This skips running tests on CI.
