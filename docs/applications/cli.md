---
id: cli
title: CLI
---


## Bitcoin-s command line interface

The [cli](/api/org/bitcoins/cli) project is meant to be a bitcoin-s command line interface (cli).

### Building the command line interface

You must first have [bitcoin-s properly installed](../getting-setup) on your machine, after which you should be able to build the cli with
```bashrc
$ sbt cli/universal:stage
```

After running that command you should find the executable here:

```bash
./app/cli/target/universal/stage/bin/bitcoin-s-cli
```

### Executing commands
You can ask the client for help with

```bash
./app/cli/target/universal/stage/bin/bitcoin-s-cli --help
Usage: bitcoin-s-cli [options] [<cmd>]

  -n, --network <value>  Select the active network.
  --debug                Print debugging information
  -h, --help             Display this help message and exit
  <cmd>                  The command and arguments to be executed. Try bitcoin-s-cli help for a list of all commands
```


Now you are are ready to start the server that the cli sends commands to. Take a look at our [server](server.md) documentation on how to build and start the server.

### Native binaries

Bitcoin-s also supports building native executables for various platforms supported by the [graalvm native image](https://www.graalvm.org/reference-manual/native-image/) tool.

You can build by using the [native image plugin for sbt](https://github.com/scalameta/sbt-native-image). This will download the appropriate graalvm
version and the native image tool, and then build the cli

```bashrc
sbt cli/nativeImage
```

After the build is done, you can find the artifact here locally

```bashrc
app/cli/target/native-image/bitcoin-s-cli
```

We also publish native image binaries every time we merge a commit to master on github.
As an example, you can [see the artifacts](https://github.com/bitcoin-s/bitcoin-s/actions?query=workflow%3A%22Native+Image+bitcoin-s-cli%22)
in the upper right hand corner.

If you don't want to build the `bitcoin-s-cli` yourself, you can download it for your platform there.