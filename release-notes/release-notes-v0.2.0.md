Please see bitcoin-s.org for more information on bitcoin-s.

# Warning

This release publishes an alpha version of our wallet, chain and node project. This code is well tested in unit tests, but has not been extensively tested on a live network. Use with caution.

# Highlights

- a378cd17e Add auto publish of website to Travis (#487)
- 839d52020 Node (and chain and wallet) (#490)
- 3040d03fa Create all root level accounts on wallet creation (#497)
- eec55d45 Multi module configuration (#494)
- aaccfbd93 Add sections on generating addresses and expand HD section in website
- 888e16c8a Generate bloom filters from wallet (#501)
- 90b9b6aa9 Pass bloom filter to SPV node (#514)
- 05a89e1a7 Default to file based databases in tests (#517)
- 5ed0f6d35 BIP 158 Golomb-Rice Coded Sets and block filters (#481)
- 26ad52460 Implemented BIP 157 Block Filter Headers (#532)
- 207578444 Add website version for 0.1.0 (#541)
- 91633375a Attempt to sync with sendheaders (#537)
- f50b55f7e 2019 07 01 windows secp256k1 bin (#559)
- 854242b46 [WIP] New Eclair RPC client (#535)
- d00dff564 Reorg handling in chain project (#548)
- 9bbd4c0ad New doc for Windows users (#572)
- 9101aece9 Process outgoing transactions (#555)
- 70ce5a0ba Somewhat dirty standalone server and CLI binary (#558)
- ae134f974 Node cleanup (#591)
- d8e214bbb Refactor TX processing logic and add comments re. TODOs
- 538f0e4f7 Make sure our secp256k1jni package is compiled against java 8 (#629)
- 53af971b1 Add all wallet outpoints to bloom filter
- 8125bca7c  Add functionality for broadcasting TXs to node (#577)
- 72018bf83 Fix merkle block parsing error
- 363c9fb83 Update cli.sbt for native-image building
- e960422ff implement ability to monitor a invoice that we are generated (#649)
- 2994e82d0 Implement missing branch on POW validation for testnet, implement Blo… (#652)
- 31642aff4 Add configurable logging to data directory (#640)
- b706e8ea4 Add functionality for updating SPV node bloom filter  (#585)
- c89cfea92 2019 08 09 Don't use BlockHeaderDAO in TipValidation (#688)
- 4afdccb0a 2019 08 05 broadcast tx test (#680)
- ab170d05f 2019 08 16 process header optimization (#701)
- c5fc13316 use 2.13 for website publish (#721)
- 9ce969985 Build and CI improvements  (#710)

# Thanks

Thanks to @nkohen @kaibr @torkelrogstad @rorp @cwaldron97 @piu130 and @Christewart for contributing to this release. 

