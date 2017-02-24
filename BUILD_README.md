SBT - http://www.scala-sbt.org/download.html
    
With SBT downloaded, open a terminal, and go to the directory in which you'd like to install Bitcoin-S-Core:

```shell
$ git clone https://github.com/bitcoin-s/bitcoin-s-core.git
```
bitcoin-s-core uses secp256k1 for signature verfication, you need to compile it and install it on your machine:

```shell
$ mkdir lib && cd secp256k1
$ ./autogen.sh
$ ./configure --enable-jni --enable-experimental --enable-module-ecdh
$ make
$ cd ../
$ cp secp256k1/.libs/libsecp256k1.so lib
```

and libsecp256k1 should be installed. You can open up your scala console to check this:

```shell
$ sbt console

scala> import org.bitcoin._
import org.bitcoin._

scala> Secp256k1Context.isEnabled()
res0: Boolean = true

```

And you're ready to go. A sample import:

```scala
scala> import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.Address

scala> Address("mzgBH6odmdukcKbjDwMFTU3ZvzrqJLGVW7")
res0: org.bitcoins.core.protocol.Address = P2PKHAddressImpl(mzgBH6odmdukcKbjDwMFTU3ZvzrqJLGVW7)

```
