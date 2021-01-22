---
id: dlc
title: Executing A DLC with Bitcoin-S
---

## Executing A Discreet Log Contract (DLC)

## Step 1: Get Bitcoin-S Setup

See the [setup document](../getting-setup.md).

Make sure to follow [Step 4](../getting-setup.md#step-4-optional-discreet-log-contract-branch) to checkout the `adaptor-dlc` feature branch.

### Using the GUI

To first start up the GUI you first need to start your bitcoin-s server and gui with

```bashrc
sbt bundle/run
```

or if your bitcoin-s server is already running, you can run the standalone gui with

```bashrc
sbt gui/run
```

or by following the instructions for building and running the GUI [here](../getting-setup.md#step-5-setting-up-a-bitcoin-s-server)

## Step 2: Agree On Contract Terms

Both parties must agree on all fields from the table below:

|   Field Name   |                       Description                        |
| :------------: | :------------------------------------------------------: |
|  contractInfo  |    Information about payouts and which oracles to use    |
|   collateral   |        Number of sats the initiator is putting up        |
|    locktime    |                  Locktime of the CETs                    |
| refundlocktime |            Locktime of the Refund Transaction            |
|    feerate     |                 Fee rate in sats/vbyte                   |

> Note: if you wish to set up your own oracle for testing, you can do so by checking out our [oracle rpc server](../oracle/oracle-server.md) or [Krystal Bull](https://github.com/benthecarman/krystal-bull)

## Step 3: Set up The DLC

### Using the GUI 

If you're a visual learner there is a [video demo](https://www.youtube.com/watch?v=zy1sL2ndcDg) that explains this process in detail. 
But do note that this demonstrates the old non-adaptor version of DLCs so that the Offer, Accept, Sign protocol is the same, but the contents will be different.

If using a numeric contract and/or multiple oracles, messages can get very large and sometimes even too large to for the application.
To solve this there is an `Export to file` button located under the text box for the messages your wallet will construct.
This can be used to export a DLC message to a file and then the file can be sent to your counter party.
If you receive a file from a counter-party, there is an `Import file` button on every dialog you input a DLC message.
This can be used to import the file of the DLC message from your counter-party.

#### Creating The Offer

Once the terms are agreed to, either party can use the `Offer` button and enter each of the fields from the table above.

#### Accepting The Offer

Upon receiving a DLC Offer from your counter-party, you can use the `Accept` button and paste in the DLC Offer.

#### Signing The DLC

Upon receiving a DLC Accept message from your counter-party, you can use the `Sign` button and paste in the DLC Accept.

#### Adding DLC Signatures To Your Database

Upon receiving a DLC Sign message from your counter-party, add their signatures to your database using the `Add Sigs` button and paste in the message.
After doing so you can get the fully signed funding transaction using the `Get Funding Tx` button. This will return the fully signed serialized transaction.

### Using the CLI

If using a numeric contract and/or multiple oracles, messages can get very large and sometimes even too large to for the application.
To solve this there are RPC calls where you can give a file instead of the entire DLC message.
To output a file you simply just need to pipe the output of a command into a file.

For example:
```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcoffer [offer] > myDLCAccept.txt
```

#### Creating The Offer

Once these terms are agreed to, either party can call on `createdlcoffer` with flags for each of the fields in the table above. For example:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli createdlcoffer [contractInfo] [collateral] [feerate] [locktime] [refundlocktime]
```

#### Accepting The Offer

Upon receiving a DLC Offer from your counter-party, the following command will create the serialized accept message:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcoffer [offer]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli acceptdlcofferfromfile [filepath]
```

#### Signing The DLC

Upon receiving a DLC Accept message from your counter-party, the following command will generate all of your signatures for this DLC:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli signdlc [accept]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli signdlcfromfile [filepath]
```


#### Adding DLC Signatures To Your Database

Upon receiving a DLC Sign message from your counter-party, add their signatures to your database by:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli adddlcsigs [sign]
```

or from file:

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli adddlcsigsfromfile [filepath]
```

#### Getting Funding Transaction

You are now fully setup and can generate the fully signed funding transaction for broadcast using

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli getdlcfundingtx [contractId]
```

where the `contractId` is in all but the messages other than the DLC Offer message, and is also returned by the `adddlcsigs` command.

Alternatively, you can use the `getdlcs` command to list all of your current DLCs saved in your wallet.

## Step 4: Executing the DLC

### Using the GUI

#### Execute

You can execute the DLC unilaterally with the `Execute` button which will require the oracle signature.
This will return a fully signed Contract Execution Transaction for the event signed by the oracle.

#### Refund

If the `refundlocktime` for the DLC has been reached, you can get the fully-signed refund transaction with the `Refund` button and entering the `contractId`.

### Using the CLI

#### Execute

Upon receiving an oracle signature, you can execute the DLC unilaterally with

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli executedlc [contractId] [signatureTLVs]
```

which will return fully signed Contract Execution Transaction for the event signed by the oracle.

#### Refund

If the `refundlocktime` for the DLC has been reached, you can get the fully-signed refund transaction with

```bashrc
./app/cli/target/universal/stage/bitcoin-s-cli executedlcrefund [contractId]
```

