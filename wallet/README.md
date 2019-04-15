### wallet

This is meant to be a stand alone project that can be used as a cold storage wallet _and_ hot wallet.

#### Features

- utxo storage
- key storage
- key generation
- coin selection 
- transaction building
- fee calculation

#### Design choices

 - Private key material is just stored once, as the 
 [xpriv](../core/src/main/scala/org/bitcoins/core/crypto/ExtKey.scala). 
 Addresses we hand out to users are stored with their 
 [BIP44 paths](../core/src/main/scala/org/bitcoins/core/crypto/bip44/BIP44Path.scala)
 and script types, so that everything we need for spending the money sent to an address
 is derivable.
 