---
id: dlc
title: Discreet Log Contract Data Structures
---

Bitcoin-S now has support for the basic data structures and algorithms involved in the setup and execution of Discreet Log Contracts (DLCs) in the package `org.bitcoins.core.protocol.dlc` as well as basic TLV format and LN Message format support at `org.bitcoins.core.protocol.tlv` which can be useful for many off-chain protocols including DLCs and Lightning.

**Please note that this code is experimental as the [DLC specification](https://github.com/discreetlogcontracts/dlcspecs) is a work-in-progress and so this code is not stable and is subject to change. This means that the DLC code in the official release may be out-of-date and may conflict with other implementations' results.**

> If you require the most up-to-date DLC data structures and algorithms, you should use the `master` branch.
> If you are interested in actually setting up and executing DLCs, you should checkout the [`adaptor-dlc branch`](https://github.com/bitcoin-s/bitcoin-s/tree/adaptor-dlc).

Let's now cover the main data structures and interfaces supported.

```scala mdoc:invisible
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.dlc.RoundingIntervals.IntervalStart
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.Indexed
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
```

## DLCPayoutCurve

[DLCPayoutCurve.scala](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/DLCPayoutCurve.scala) provides an interface for serializing and evaluating payout curves for DLCs as specified in the [Payout Curve Specification](https://github.com/discreetlogcontracts/dlcspecs/blob/c4fb12d95a4255eabb873611437d05b740bbeccc/PayoutCurve.md). This file supports arbitrary polynomial interpolation.

To approximate a payout curve that is not a piecewise polynomial function, one may either propose a new kind of curve to the specification, or use approximation. For example by feeding `DLCPayoutCurve` a list of `OutcomePayoutEndpoint`s, one receives a linear approximation of their payout curve which takes the sampled points and "connects the dots" with straight lines. Alternatively one can use spline interpolation and sample two midpoints of every spline to get a piecewise cubic interpolation.

```scala mdoc:to-string
// Constructing a forward contract's payout curve (going long) that looks like this:
//         ________________
//        /
//       /
// _____/

// Assume a 15 binary digit oracle
val maxVal = (1L << 15) - 1
val pts = Vector(
    OutcomePayoutEndpoint(0, 0),
    OutcomePayoutEndpoint(1000, 0),
    OutcomePayoutEndpoint(2000, 1000),
    OutcomePayoutEndpoint(maxVal, 1000)
)
val curve = DLCPayoutCurve(pts)

// Let's evalute the curve's values at varios points
curve(500)
curve(1500)
curve(1667)
curve(10000)

// Now with rounding to the nearest 100
val roundTo100 = RoundingIntervals(Vector(IntervalStart(0, 100)))
curve(1667, roundTo100)

// Let's take a look at the pieces in this piece-wise polynomial
curve.functionComponents

// And we can even see which component is used on a given outcome
val Indexed(line1, _) = curve.componentFor(500)
line1(500)

val Indexed(line2, _) = curve.componentFor(1667)
line2(1667)

val Indexed(line3, _) = curve.componentFor(10000)
line3(10000)
```

While many approaches result in higher fidelity to the original curve than linear approximation, usually it is the case that numeric contracts use [Rounding](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/RoundingIntervals.scala) which dominates the error so that linear approximation is adequate.

## CETCalculator

[CETCalculator.scala](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/CETCalculator.scala) provides an interface to all Contract Execution Transaction (CET) set computations as specified in the [CET Compression Specification](https://github.com/discreetlogcontracts/dlcspecs/blob/c4fb12d95a4255eabb873611437d05b740bbeccc/CETCompression.md) and the [Multi-Oracle Specification](https://github.com/discreetlogcontracts/dlcspecs/blob/4fb01bc4e15865fa8323caf7e9cebb403b8116a5/MultiOracle.md).

Of particular note, are the functions `computeCETs` and `computeMultiOracleCETsBinary` which compute the entire set of outcomes (corresponding to CETs) for a single oracle and multiple numeric oracles with a bounded difference allowed respectively.

Additionally the `combinations` function allows for the easy and correctly-ordered reduction of any `t-of-n` scheme to many `t-of-t` schemes.

```scala mdoc:to-string
val totalCollateral = Satoshis(1000)
val cetsNoRounding = CETCalculator.computeCETs(base = 2, numDigits = 15, curve, totalCollateral, RoundingIntervals.noRounding)
cetsNoRounding.length
val cetsWithRounding = CETCalculator.computeCETs(base = 2, numDigits = 15, curve, totalCollateral, roundTo100)
cetsWithRounding.length
```

```scala mdoc:to-string
val oraclesStr = Vector("Alice", "Bob", "Carol", "Dave", "Eve")
val combinations = CETCalculator.combinations(oraclesStr, threshold = 3)
```

```scala mdoc:to-string
val multiOracleCETsNoRounding = CETCalculator.computeMultiOracleCETsBinary(
    numDigits = 15,
    curve,
    totalCollateral,
    RoundingIntervals.noRounding,
    maxErrorExp = 5,
    minFailExp = 3,
    maximizeCoverage = false,
    numOracles = 3
)
multiOracleCETsNoRounding.length
val multiOracleCETsWithRounding = CETCalculator.computeMultiOracleCETsBinary(
    numDigits = 15,
    curve,
    totalCollateral,
    roundTo100,
    maxErrorExp = 5,
    minFailExp = 3,
    maximizeCoverage = false,
    numOracles = 3
)
multiOracleCETsWithRounding.length
```

## OracleOutcome

[OracleOutcome](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/OracleOutcome.scala)s correspond one-to-one with DLC execution paths (and with CETs). An `OracleOutcome` contains information about which oracles signed what, but storing only the information actually used for DLC execution and not extra unneeded information such as what additional oracles signed.

This trait also exposes an endpoint to the aggregate signature point (aka adaptor point) corresponding to this outcome which is used during CET signing, as well as an endpoint to compute the aggregate nonce.

## DLCStatus

[DLCStatus.scala](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/DLCStatus.scala) contains the DLC state machine comprised of the states `[Offered, Accepted, Signed, Broadcasted, Confirmed, Claimed, RemoteClaimed, Refunded]` each of which contain all relevant P2P messages and on-chain transactions. 

The `DLCStatus` object also contains many useful utility functions to extract and compute various things from a `DLCStatus` such as transaction ids and even computing the [OracleOutcome](#OracleOutcome) and aggregate oracle signature from on-chain information in the case that one's remote counter-party initiates execution.

## ContractInfo

[ContractInfo](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/ContractInfo.scala) wraps a [ContractDescriptor](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/ContractDescriptor.scala) and an [OracleInfo](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/dlc/OracleInfo.scala) and provides the complete external-facing interface needed during DLC setup and execution. A `ContractInfo` fully determines a DLC up to the choice of public keys and funding UTXOs to be used. One of its most important functions is `allOutcomesAndPayouts` which computes the entire set of [OracleOutcome](#OracleOutcome)s and their corresponding payouts. Most of its other functions utilize that set to do all sorts of things such as retrieving payouts or finding an outcome given `OracleSignatures`.

```scala mdoc:to-string
val descriptor = NumericContractDescriptor(curve, numDigits = 15, roundTo100)

val announcements = 0.until(5).toVector.map { _ =>
    val oraclePrivKey = ECPrivateKey.freshPrivateKey
    val nonces = 0.until(15).toVector.map(_ => ECPrivateKey.freshPrivateKey.schnorrNonce)
  	OracleAnnouncementV0TLV.dummyForKeys(oraclePrivKey, nonces)
}
val oracleInfo = NumericMultiOracleInfo(
    threshold = 3,
    announcements,
    maxErrorExp = 5,
    minFailExp = 3,
    maximizeCoverage = false
)

val contractInfo = ContractInfo(totalCollateral, ContractOraclePair.NumericPair(descriptor, oracleInfo))
contractInfo.max
contractInfo.allOutcomes.length

val signingOracles = oracleInfo.singleOracleInfos.take(3)
val outcome = NumericOracleOutcome(signingOracles.map((_, UnsignedNumericOutcome(Vector(0, 0, 0, 0, 0)))))
contractInfo.getPayouts(outcome)
```

## TLV

[TLV.scala](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/tlv/TLV.scala) provides protocol agnostic infrastructure for the Type-Length-Value (TLV) serialization format which is used by both the Lightning Network and DLCs. The file contains a sealed trait of all possible TLVs as well as utilities for defining new types.

This includes `NormalizedString` a standardized string serialization using varint-prefixed UTF-8 with NFC normalization which has implicit conversions with `String` so that they can be used interchangeably. This also includes a `TLVUtil` trait for serialization and a `ValueIterator` class for deserialization.

Currently, only the most basic Lightning messages are defined (`Ping`, `Pong`, `Error`) but all DLC messages are supported.

## LnMessage

[LnMessage.scala](https://github.com/bitcoin-s/bitcoin-s/blob/master/core/src/main/scala/org/bitcoins/core/protocol/tlv/LnMessage.scala) provides support for the Lightning Network Message serialization format, which is very similar to the [TLV](#tlv) format with some minor differences so that an `LnMessage` simply wraps a `TLV` and provides the altered serialization format. Likewise one can parse a `LnMessage` using a `TLVFactory` and subsequently unwrap to get the nested `TLV`.

```scala mdoc:to-string
val offerTLV = DLCOfferTLV(
    contractFlags = 0.toByte,
    chainHash = DoubleSha256Digest.empty,
    contractInfo = contractInfo.toTLV,
    fundingPubKey = ECPublicKey.freshPublicKey,
    payoutSPK = EmptyScriptPubKey,
    totalCollateralSatoshis = Satoshis(500),
    fundingInputs = Vector.empty,
    changeSPK = EmptyScriptPubKey,
    feeRate = SatoshisPerVirtualByte(Satoshis(1)),
    contractMaturityBound = BlockStamp.BlockHeight(0),
    contractTimeout = BlockStamp.BlockHeight(0)
)

val lnMsgHex = LnMessage(offerTLV).hex
val lnMsg = LnMessageFactory(DLCOfferTLV).fromHex(lnMsgHex)

lnMsg.tlv == offerTLV
```

