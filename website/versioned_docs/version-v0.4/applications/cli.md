---
id: version-v0.4-cli
title: CLI
original_id: cli
---


## Bitcoin-s command line interface

The [cli](../../app/cli/) project is meant to be a bitcoin-s command line interface (cli).

### Building the command line interface

You must first have [bitcoin-s properly installed](../getting-setup) on your machine, after which you should be able to build the cli with
```bashrc
$ sbt cli/universal:stage
```

After running that command you should find the executable here:

```bash
bitcoin-s/app/cli/target/universal/stage/bin/bitcoin-s-cli
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