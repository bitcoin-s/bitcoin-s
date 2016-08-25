SBT - http://www.scala-sbt.org/download.html
    
With SBT downloaded, open a terminal, and go to the directory in which you'd like to install Bitcoin-S-Core:

```shell
git clone https://github.com/bitcoin-s/bitcoin-s-core.git
sbt console
```

And you're ready to go. A sample import:

```scala
scala> import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.Address

scala> Address("mzgBH6odmdukcKbjDwMFTU3ZvzrqJLGVW7")
res0: org.bitcoins.core.protocol.Address = P2PKHAddressImpl(mzgBH6odmdukcKbjDwMFTU3ZvzrqJLGVW7)

```
