[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-bench/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-bench/_latestVersion)

# Benchmark suite

This is a WIP. It currently contains one bench mark for de-serializing large blocks

You can assembly the jar like this

```
sbt bench/assembly
[info] Updating secp256k1jni...
[info] Done updating.
[info] Updating core...
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies.
[warn] Run 'evicted' to see detailed eviction warnings
[info] Updating ...
[info] Done updating.
[warn] There may be incompatibilities among your library dependencies.
[warn] Run 'evicted' to see detailed eviction warnings
[info] Compiling 4 Scala sources to /home/chris/dev/bitcoin-s-core/core/target/scala-2.12/classes ...
[info] Done compiling.
[info] Compiling 56 Scala sources to /home/chris/dev/bitcoin-s-core/core/target/scala-2.12/classes ...
[info] Done compiling.
[info] Strategy 'discard' was applied to 11 files (Run the task at debug level to see details)
[info] Packaging /home/chris/dev/bitcoin-s-core/bench/target/scala-2.12/bitcoin-s-bench-assembly-f85fcf-1550326975891-SNAPSHOT.jar ...
[info] Done packaging.
```


and then run with

```
$ java -Xprof -jar /home/chris/dev/bitcoin-s-core/bench/target/scala-2.12/bitcoin-s-bench-assembly-f85fcf-1550326975891-SNAPSHOT.jar > output.txt
```

The `hash` and `timestamp` are in the console output above. In this example it is `f85fcf` and `1550326975891`
