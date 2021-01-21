---
id: server-key-manager
title: Server Key Manager
---

The [Application Server](../applications/server.md) supports a Neutrino node and wallet. For its wallet, it uses
a `BIP39KeyManager` for handling its underlying keys and doing cryptographic functions like signing transactions.

### Backups

All seeds are stored in the `seeds` folder inside your bitcoin-s directory, the default location is in `~/.bitcoin-s`.
The easiest way to back up your seeds is to simply copy the `seeds` folder to your secure back up location.

Do note that if you have a key manager passphrase or a BIP 39 passphrase set for a given seed, you will need these to be
able to recover the seed.

### Using multiple seeds

You can change which wallet and key manager you are using by setting the `bitcoin-s.wallet.walletName` config
option. If it is not set, it will use the default wallet. Different seeds are saved in the `seeds` folder, and the
seed's file name will be prefixed with the wallet name.

For example, if you have a default wallet, and 2 wallets named `test-wallet` and `rickRollWallet` your seeds folder will look like:

```
tree ~/.bitcoin-s/seeds
/home/suredbits/.bitcoin-s/seeds
├── encrypted-bitcoin-s-seed.json
├── test-wallet-encrypted-bitcoin-s-seed.json
└── rickRollWallet-encrypted-bitcoin-s-seed.json
```

Currently, Bitcoin-S only supports using a single key manger/wallet at a single instance. When switching wallets, you
will need to shut down the server and restart it with your modified config.

### Passphrases

Upon creating a wallet it's passphrase and the BIP 39 passphrase will be set to use what is in
your [configuration file](../config/configuration.md). The key manager's passphrase is set using
the `bitcoin-s.keymanager.aespassword` option, and the BIP 39 passphrase is set using
the `bitcoin-s.keymanager.bip39password` option.

If you do not have the set the `aespassword` config option on the creation of the key manager, then the seed will be
saved unencrypted.

#### Changing Key Manager Passphrase

You can set and change your key mananger's passphrase using the `keymanagerpassphraseset`
or `keymanagerpassphrasechange` rpc commands. For more details checkout out
the [server docs](../applications/server.md#wallet).

You cannot change your BIP39 passphrase because this would change the underlying keys. To change this you will need to
create a new wallet and then send all the UTXOs from the previous wallet to the new one.

### Restoring a Seed

#### Restore from Mnemonic

To restore from a mnemonic seed you can use the `importseed` cli command.
It takes in the actual mnemonic seed, wallet name, and an optional passphrase to encrypt the seed.

**Example:**

```
bitcoin-s-cli importseed "never gonna give you up never gonna let you down never gonna run around and desert you never gonna make you cry never gonna" "rickRollWallet" "insecurePassword"
```

#### Restore from XPRV

To restore from a mnemonic seed you can use the `importxprv` cli command.
It takes in the base58 encoded xprv, wallet name, and an optional passphrase to encrypt the seed.

**Example:**

```
bitcoin-s-cli importxprv "xprv9uHRZZhk6KAJC1avXpDAp4MDc3sQKNxDiPvvkX8Br5ngLNv1TxvUxt4cV1rGL5hj6KCesnDYUhd7oWgT11eZG7XnxHrnYeSvkzY7d2bhkJ7" "xprvWallet" "insecurePassword"
```

#### Restore from backup

To restore from a backed up seed, simply put the seed file in the `seeds` folder and then load the wallet.
To do this, you will need the seed file name to be `walletName-encrypted-bitcoin-s-seed.json` or `encrypted-bitcoin-s-seed.json`
if you want it to be the default wallet.
To load it, just set the `bitcoin-s.wallet.walletName` config option to the appropriate wallet name.
It is also important to remember to set `bitcoin-s.wallet.defaultAccountType` to the type of wallet you were using on your old wallet.
As of this writing there are 3 types allowed
- legacy
- nested-segwit
- segwit

If you fail to set this setting, your wallet could be looking for the wrong script types and not correctly find your funds
on the blockchain as documented in [this issue](https://github.com/bitcoin-s/bitcoin-s/issues/2527).

When restoring from a backup you should not replace an existing seed file as this can result in a loss of funds.
If you have a seed file that would replace an existing seed its file name should be prefixed with a different wallet name.
If you do replace a seed file you will need to delete the old wallet database associated with that wallet.

You may need to run a wallet rescan to restore funds, this can be done by using the `rescan` RPC command.

For example, if you wanted to restore your `rickRollWallet`, you would place your backup at `~/.bitcoin-s/seeds/rickRollWallet-encrypted-bitcoin-s-seed.json`
and then load the wallet using a config with `bitcoin-s.wallet.walletName = rickRollWallet`
