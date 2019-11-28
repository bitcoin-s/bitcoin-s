## Bitcoin-s command line client

The [cli](../../app/cli/) project is meant to be a bitcoin-s command line client. It uses [graalvm native image]() to
create a native executable that circumvents the jvm for fast start up time.

### Building the command line

#### Installing graalvm native image
First to build the command line you need to have the graal jdk installed. This can be installed on graalvm's website.

Once you have the graalvm installed you should be able to verify you have the `native-image` exeuctable installed by running

```bashrc
$ native-image --help
```

#### Building the native image

Now that you have graalvm native image installed, you should be able to build the cli with

```bashrc
$ sbt cli/graalvm-native-image:packageBin
```

After running that command you should find the executable here:

```bash
???
```

#### Executing commands
You can ask the client for help with

```bash
$ ./bitcoin-s-cli help
```