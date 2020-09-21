---
id: version-v0.4-dlc
title: Executing A DLC with Bitcoin-S
original_id: dlc
---

## Executing A Discreet Log Contract (DLC)

## Step 1: Get Bitcoin-S Setup

See the [setup document](../getting-setup).

Make sure to follow [Step 4](../getting-setup#step-4-optional-discreet-log-contract-branch) to checkout the `dlc` feature branch.

## Step 2: Agree On Contract Terms

Both parties must agree on all fields from the table below:

|   Field Name   |                          Format                          |
| :------------: | :------------------------------------------------------: |
|   oracleInfo   |            OraclePubKeyHex ++ OracleRValueHex            |
|  contractInfo  | Hash1Hex ++ 8ByteValue1Hex ++ Hash2Hex ++ 8ByteValue2Hex |
|   collateral   |                      NumInSatoshis                       |
|    locktime    |                       LockTimeNum                        |
| refundlocktime |                       LockTimeNum                        |
|    feerate     |                  NumInSatoshisPerVByte                   |

Here is an example `oracleInfo` for public key `025acb434efb32bbf7ca7fd44b22e0f3f5570c6bc564e6059b03ba18c277054ac1` and R value `03f8758d7f03a65b67b90f62301a3554849bde6d00d50e965eb123398de9fd6ea7`:

```bashrc
025acb434efb32bbf7ca7fd44b22e0f3f5570c6bc564e6059b03ba18c277054ac103f8758d7f03a65b67b90f62301a3554849bde6d00d50e965eb123398de9fd6ea7
```

Here is an example `contractInfo` for hashes `c07803e32c12e100905e8d69fe38ae72f2e7a17eb7b8dc1a9bce134b0cbe920f` and `5c58e41254e7a117ee1db59874f2334facc1576c238c16d18767b47861f93f7c` with respective Satoshi denominated outcomes of `100000 sats` and `0 sats`:

```bashrc
c07803e32c12e100905e8d69fe38ae72f2e7a17eb7b8dc1a9bce134b0cbe920fa0860100000000005c58e41254e7a117ee1db59874f2334facc1576c238c16d18767b47861f93f7c0000000000000000
```

And finally, here are the oracle signatures for each hash in order in case you want to test with this contract:

```bashrc
f8758d7f03a65b67b90f62301a3554849bde6d00d50e965eb123398de9fd6ea7fbbee821b7166028a6927282830c9452cfcf3c5716c57e43dd4069ca87625010
```

```bashrc
f8758d7f03a65b67b90f62301a3554849bde6d00d50e965eb123398de9fd6ea7af05f01f1ca852cf5454a7dc91cdad7903dc2e67ddb2b3bc9d61dabd8856aa6a
```

Note: if you wish to setup your own oracle for testing, you can do so by pasting the following into the `sbt core/console`:

```scala
import org.bitcoins.core.crypto._
import org.bitcoins.core.util.CryptoUtil
import scodec.bits.ByteVector
import org.bitcoins.core.currency._

val privKey = ECPrivateKey.freshPrivateKey
val pubKey = privKey.publicKey
val nonce = SchnorrNonce.freshNonce
val rValue = nonce.publicKey
val winHash = CryptoUtil.sha256(ByteVector("WIN".getBytes)).flip
val loseHash = CryptoUtil.sha256(ByteVector("LOSE".getBytes)).flip

(pubKey.bytes ++ rValue.bytes).toHex
(winHash.bytes ++ Satoshis(100000).bytes ++ loseHash.bytes ++ Satoshis.zero.bytes).toHex
Schnorr.signWithNonce(winHash.bytes, privKey, nonce).hex
Schnorr.signWithNonce(loseHash.bytes, privKey, nonce).hex
```

Where you can replace the messages `WIN` and `LOSE` to have the oracle sign any two messages, and replace `Satoshis(100000)` and `Satoshis.zero` to change the outcomes.

## Step 3: Setup The DLC

### Creating The Offer

Once these terms are agreed to, either party can call on `createdlcoffer` with flags for each of the fields in the table above. For example:

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli createdlcoffer --oracleInfo 025acb434efb32bbf7ca7fd44b22e0f3f5570c6bc564e6059b03ba18c277054ac103f8758d7f03a65b67b90f62301a3554849bde6d00d50e965eb123398de9fd6ea7 --contractInfo c07803e32c12e100905e8d69fe38ae72f2e7a17eb7b8dc1a9bce134b0cbe920fa0860100000000005c58e41254e7a117ee1db59874f2334facc1576c238c16d18767b47861f93f7c0000000000000000 --collateral 40000 --locktime 1666720 --refundlocktime 1666730 --feerate 3
```

This will return a nice pretty-printed JSON offer. To get an offer that can be sent to the counter-party, add the `--escaped` flag to the end of this command.

### Accepting The Offer

Upon receiving a DLC Offer from your counter-party, the following command will create the serialized accept message:

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli acceptdlcoffer --offer [offer] --escaped
```

### Signing The DLC

Upon receiving a DLC Accept message from your counter-party, the following command will generate all of your signatures for this DLC:

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli signdlc --accept [accept] --escaped
```

### Adding DLC Signatures To Your Database

Upon receiving a DLC Sign message from your counter-party, add their signatures to your database by:

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli adddlcsigs --sigs [sign]
```

You are now fully setup and can generate the fully signed funding transaction for broadcast using

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli getdlcfundingtx --eventid [eventid]
```

where the `eventid` is in all but the messages other than the DLC Offer message, and is also returned by the `adddlcsigs` command.

## Step 4: Executing the DLC

### Mutual Close

Upon receiving an oracle signature, either party can initiate a mutual close with

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli initdlcmutualclose --eventid [eventid] --oraclesig [sig] --escaped
```

And if you receive one of these CloseSig messages from your counter-party, you can generate the fully-signed mutual closing transaction with

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli acceptdlcmutualclose --closesig [closesig]
```

### Unilateral Close

If your counter-party is unresponsive upon receiving an `initdlcmutualclose` message, or is unreachable, you can execute the DLC unilaterally with

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli executedlcforceclose --eventid [eventid] --oraclesig [sig]
```

which will return two fully-signed transactions in the case that you are owed any funds, and one fully-signed transaction in the case that you aren't. The first transaction returned should be the fully signed Contract Execution Transaction, and the second transaction, if existing, should be the fully-signed sweep transaction which claims your funds on the CET.

#### Claiming Remote Funds When Counter-Party Unilaterally Closes

If your counter-party has broadcasted a CET to the network, you can claim the funds on the `ToRemoteOutput` using

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli claimdlcremotefunds --eventid [eventid] --forceclosetx [cet]
```

#### Claiming Penalty Funds

If your counter-party has broadcasted a CET to the network, and does not sweep their ToLocal funds in `5` blocks, you can claim the funds on the `ToLocalOutput` using

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli claimdlcpenaltyfunds --eventid [eventid] --forceclosetx [cet]
```

### Refund

If the `refundlocktime` for the DLC has been reached, you can get the fully-signed refund transaction with

```bashrc
./app/cli/target/graalvm-native-image/bitcoin-s-cli executedlcrefund --eventid [eventid]
```

