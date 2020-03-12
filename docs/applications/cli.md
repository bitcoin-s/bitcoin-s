---
id: cli
title: CLI
---


## Bitcoin-s command line interface

The [cli](../../app/cli/) project is meant to be a bitcoin-s command line interface (cli). It uses [graalvm native image](https://www.graalvm.org/docs/reference-manual/native-image/) to
create a native executable that circumvents the jvm for fast start up time.

### Building the command line interface

#### Installing graalvm native image
First to build the command line interface you need to have the graal jdk installed. This can be installed on [graalvm's github](https://github.com/graalvm/graalvm-ce-builds/releases/).

Make sure you install the [prerequisites](https://www.graalvm.org/docs/reference-manual/native-image/#prerequisites) for installing the `native-image` executable.


If you do not have the `native-image` executable installed, you need to install it with

```bashrc
./graalvm-ce-java8-19.3.0/bin/gu install native-image
```

Once you have the graalvm installed you should be able to verify you have the `native-image` exeuctable installed by running

```bashrc
$ native-image --help
```

If your command did not work, make sure you can find the `native-image` executable on your `PATH`.

#### Building the native image

Now that you have graalvm native image installed, you should be able to build the cli with. It is important to note that
this only works with Scala `2.12.x`. I'm unsure of why the native image build fails with Scala `2.13.x`

```bashrc
$ sbt ++2.12.10 cli/graalvm-native-image:packageBin
```

After running that command you should find the executable here:

```bash
bitcoin-s/app/cli/target/graalvm-native-image
```

#### Executing commands
You can ask the client for help with

```bash
 ./app/cli/target/graalvm-native-image/bitcoin-s-cli --help
Usage: bitcoin-s-cli [options] [<cmd>]

  -n, --network <value>  Select the active network.
  --debug                Print debugging information
  -h, --help             Display this help message and exit
  <cmd>                  The command and arguments to be executed. Try bitcoin-s-cli help for a list of all commands
```


Now you are are ready to start the server that the cli sends commands to. Take a look at our [server](server.md) documentation on how to build and start the server.