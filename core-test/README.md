[ ![Download](https://api.bintray.com/packages/bitcoin-s/bitcoin-s-core/bitcoin-s-core-test/images/download.svg) ](https://bintray.com/bitcoin-s/bitcoin-s-core/bitcoin-s-core-test/_latestVersion)

## Running tests

To run the entire `core` test suite:

```bash
chris@chris:~/dev/bitcoins-core$ sbt coreTest/test
# lots of output, should end up with something like this
[info] All tests passed.
[info] Passed: Total 1008, Failed 0, Errors 0, Passed 1008
[success] Total time: 38 s, completed Dec 23, 2018 4:39:58 PM
```

## Coverage

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