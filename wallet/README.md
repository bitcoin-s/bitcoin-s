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

- Private key material is just stored once, as the mnemonic code used to initialize the
  wallet
- Addresses we hand out to users are stored with their
  [BIP44/BIP49/BIP84 paths](../core/src/main/scala/org/bitcoins/core/hd/HDPath.scala)
  and script types, so that everything we need for spending the money sent to an address
  is derivable.
- **The wallet is a "dumb" wallet that acts mostly as a database of UTXOs, transactions and
  addresses, with associated operations on these.**
  The wallet module does very little verification of incoming data about transactions,
  UTXOs and reorgs. We're aiming to write small, self contained modules, that can be
  composed together into more fully fledged systems. That means the `chain` and `node`
  modules does the actual verification of data we receive, and `wallet` just blindly
  acts on this. This results in a design where you can swap out `node` for a Bitcoin Core
  full node, use it with hardware wallets, or something else entirely. However, that also
  means that users of `wallet` that doesn't want to use the other modules we provide have
  to make sure that the data they are feeding the wallet is correct.

#### Database structure

We store information in the following tables:

- TXOs - Contains both the information needed to spent it as well as information related
  to wallet state (confirmations, spent/unspent etc)
- Addresses - must reference the account it belongs to
- Accounts

#### Mnemonic encryption

The mnemonic seed to the Bitcoin-S wallet is written to disk, encrypted. The file name is
`$HOME/.bitcoin-s/$NETWORK/encrypted_bitcoin-s_seed.json`. We store it in a JSON object
that looks like this:

```json
{
  "iv": "initializationVector",
  "cipherText": "encryptedCipherText",
  "salt": "saltUsedInEncryption"
}
```

The parts that's relevant to this part of the wallet is `WalletStorage.scala` (where we handle
the actual reading from and writing to disk), `EncryptedMnemonic.scala` (where we convert an
encrypted mnemonic to a cleartext mnemonic) and `AesCrypt.scala` (where do the actual
encryption/decryption).

We use AES encryption for this, block cipher mode and PKCS5 padding. The wallet password is fed
into the PBKDF2 key stretching function, using SHA512 as the HMAC function. This happens in
`PBKDF2.scala`.
