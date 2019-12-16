---
id: version-0.2.0-key-manager
title: Key Manager
original_id: key-manager
---


#### Key Manager

The key manager module's goal is to encapusulate all private key interactions with the [wallet](wallet.md) project.

As of this writing, the wallet just delegates storage of the encrypted mnemonic seed to the key manager project. Over the log run, we want to make it so that the wallet project needs to communicate with the key-manager to access private keys.

This means that ALL SIGNING should be done inside of the key-manager, and private keys should not leave the key manager.

This makes it easier to reason about the security characteristics of our private keys, and a way to provide a uniform interface for alternative key storage systems (hsm, cloud based key storage, etc) to be plugged into the bitcoin-s library.