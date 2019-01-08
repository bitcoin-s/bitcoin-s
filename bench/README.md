[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-bench/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-bench/_latestVersion)

# Benchmark suite

This is a WIP. It currently contains one bench mark for de-serializing large blocks

You can assembly the jar like this

```
sbt bench/assembly
```

and then run with

```
$ java -Xprof -jar bench/target/scala-2.11/bench-assembly-0.0.1-SNAPSHOT.jar > output.txt
```
