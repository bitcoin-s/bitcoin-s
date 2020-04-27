---
id: adding-spks
title: Adding New Script Types
---

```scala mdoc:invisible
/* In order to allow the code in this document to be compiled, we must add these
 * imports here in this invisible, executed code block. We must also not import any
 * sealed traits that get extended as this will cause errors, and so instead we define
 * new ones in this invisible code block of the same names and add implicit conversions
 * where needed so that our fake type can be returned anywhere the real one is expected
 * and vice-versa. We also add defs to traits where there are overrides to avoid errors,
 * as well as defs for all vals that are out of scope in code executed below.
 *
 * Note that as this code is never used outside of simply defining things below (only
 * compiled), we can use ??? everywhere where implementations are expected.
 *
 * Also note that when defining our "new" traits in the actual doc, they must be put in
 * silent mode rather than compile-only mode to make them accessible to the rest of the doc.
 */

import org.bitcoins.crypto._
import org.bitcoins.core.crypto._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.util._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.script.{Script, ScriptPubKey, ScriptWitness, ScriptFactory, CSVScriptPubKey, MultiSignatureScriptPubKey, LockTimeScriptPubKey, EmptyScriptPubKey, ConditionalScriptSignature, P2PKScriptSignature, MultiSignatureScriptSignature}
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.utxo.{ConditionalPath, MultiSignatureSpendingInfoSingle, MultiSignatureSpendingInfoFull, ConditionalSpendingInfoSingle, ConditionalSpendingInfoFull}
import org.bitcoins.testkit.core.gen._
import org.bitcoins.testkit.core.gen.ScriptGenerators._

import org.scalacheck.Gen

import scodec.bits.ByteVector

import scala.concurrent.{Future, ExecutionContext, Await}
import scala.util._

import scala.language.implicitConversions

sealed trait RawScriptPubKey extends Script
implicit def realToFakeRawSPK(real: org.bitcoins.core.protocol.script.RawScriptPubKey): RawScriptPubKey = ???
implicit def fakeToRealRawSPK(fake: RawScriptPubKey): org.bitcoins.core.protocol.script.RawScriptPubKey = ???

sealed trait RawScriptUTXOSpendingInfo {
  def signer: Sign
  def signers: Vector[Sign]
  def requiredSigs: Int
  def conditionalPath: ConditionalPath
  def scriptPubKey: Any
}
sealed trait RawScriptUTXOSpendingInfoFull extends RawScriptUTXOSpendingInfo {
  def signer: Sign = signers.head
  def toSingle(signerIndex: Int): Any = ???
  def toSingles: Vector[Any] = ???
}
implicit def realToFakeRawSpendingInfoFull(real: org.bitcoins.core.wallet.utxo.RawScriptUTXOSpendingInfoFull): RawScriptUTXOSpendingInfoFull = ???
implicit def fakeToRealRawSpendingInfoFull(fake: RawScriptUTXOSpendingInfoFull): org.bitcoins.core.wallet.utxo.RawScriptUTXOSpendingInfoFull = ???

sealed trait UTXOSpendingInfoFull
implicit def realToFakeSpendingInfoFull(real: org.bitcoins.core.wallet.utxo.UTXOSpendingInfoFull): UTXOSpendingInfoFull = ???
implicit def fakeToRealSpendingInfoFull(fake: UTXOSpendingInfoFull): org.bitcoins.core.wallet.utxo.UTXOSpendingInfoFull = ???

sealed trait UTXOSpendingInfoSingle
implicit def realToFakeSpendingInfoSingle(real: org.bitcoins.core.wallet.utxo.UTXOSpendingInfoSingle): UTXOSpendingInfoSingle = ???
implicit def fakeToRealSpendingInfoSingle(fake: UTXOSpendingInfoSingle): org.bitcoins.core.wallet.utxo.UTXOSpendingInfoSingle = ???

sealed trait ScriptSignature extends Script
implicit def realToFakeRawScriptSig(real: org.bitcoins.core.protocol.script.ScriptSignature): ScriptSignature = ???
implicit def fakeToRealRawScriptSig(fake: ScriptSignature): org.bitcoins.core.protocol.script.ScriptSignature = ???
implicit def realMultiToFakeRawScriptSigInFuture(real: Future[MultiSignatureScriptSignature]): Future[ScriptSignature] = ???
implicit def realConditionalToFakeRawScriptSigInFuture(real: Future[ConditionalScriptSignature]): Future[ScriptSignature] = ???

sealed trait BitcoinUTXOSpendingInfoFull
sealed trait RawScriptUTXOSpendingInfoSingle extends RawScriptUTXOSpendingInfo {
  override def signers: Vector[Sign] = Vector(signer)
  override def requiredSigs: Int = 1
}
sealed trait BitcoinSigner[-SpendingInfo] {
  def sign(
        spendingInfo: UTXOSpendingInfoFull,
        unsignedTx: Transaction,
        isDummySignature: Boolean,
        spendingInfoToSatisfy: SpendingInfo)(
        implicit ec: ExecutionContext): Future[TxSigComponent]

  def signSingle(
        spendingInfo: UTXOSpendingInfoSingle,
        unsignedTx: Transaction,
        isDummySignature: Boolean)(
        implicit ec: ExecutionContext): Future[PartialSignature] = ???
}
sealed trait RawSingleKeyBitcoinSigner[-SpendingInfo <: RawScriptUTXOSpendingInfoFull with RawScriptUTXOSpendingInfoSingle] {
  def keyAndSigToScriptSig(
        key: ECPublicKey,
        sig: ECDigitalSignature,
        spendingInfo: SpendingInfo): ScriptSignature

  def sign(
        spendingInfo: SpendingInfo,
        unsignedTx: Transaction,
        isDummySignature: Boolean)(
        implicit ec: ExecutionContext): Future[TxSigComponent] = ???

  def sign(
      spendingInfo: UTXOSpendingInfoFull,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: SpendingInfo)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = ???
}

object RawScriptUTXOSpendingInfoFull {
  def apply(
        outPoint: TransactionOutPoint,
        amount: CurrencyUnit,
        scriptPubKey: RawScriptPubKey,
        signers: Seq[Sign],
        hashType: HashType,
        conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoFull = ???
}

object RawScriptUTXOSpendingInfoSingle {
  def apply(
        outPoint: TransactionOutPoint,
        amount: CurrencyUnit,
        scriptPubKey: RawScriptPubKey,
        signer: Sign,
        hashType: HashType,
        conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoSingle = ???
}

object BitcoinSigner {
  def sign(
        spendingInfo: UTXOSpendingInfoFull,
        unsignedTx: Transaction,
        isDummySignature: Boolean,
        spendingInfoToSatisfy: UTXOSpendingInfoFull)(
        implicit ec: ExecutionContext): Future[TxSigComponent] = ???

  def signSingle(
      spendingInfo: UTXOSpendingInfoSingle,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[PartialSignature] = ???
}

def asm: Seq[ScriptToken] = ???
def tokens: Seq[ScriptToken] = ???
def scriptPubKey: RawScriptPubKey = ???
def spendingInfoToSatisfy: UTXOSpendingInfoFull = ???
def conditionalPath: ConditionalPath = ???
def outPoint: TransactionOutPoint = ???
def amount: CurrencyUnit = ???
def signer: Sign = ???
def hashType: HashType = ???
def beforeTimeout: Boolean = ???
def spendingInfo: UTXOSpendingInfoFull = ???
def unsignedTx: Transaction = ???
def isDummySignature: Boolean = ???
def min: Int = ???
def max: Int = ???
implicit def ec: ExecutionContext = ???
def relevantInfo(spendingInfo: UTXOSpendingInfoFull, unsignedTx: Transaction): (Seq[Sign], TransactionOutput, UInt32, HashType) = ???
def updateScriptSigInSigComponent(
      unsignedTx: Transaction,
      inputIndex: Int,
      output: TransactionOutput,
      scriptSignatureF: Future[ScriptSignature])(
      implicit ec: ExecutionContext): Future[BaseTxSigComponent] = ???
def build(
      spk: ScriptPubKey,
      signers: Seq[Sign],
      redeemScript: Option[ScriptPubKey],
      scriptWitness: Option[ScriptWitness]): Gen[BitcoinUTXOSpendingInfoFull] = ???
```

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
<!-- END doctoc -->

- [Adding a New ScriptPubKey Type](#adding-a-new-scriptpubkey-type)
  - [Step 0: Design Philosophy](#step-0-design-philosophy)
  - [Step 1: Create a New ScriptPubKey Trait](#step-1-create-a-new-scriptpubkey-trait)
  - [Step 2: Create Companion Object](#step-2-create-companion-object)
  - [Step 3: Add to Relevant fromAsm Methods](#step-3-add-to-relevant-fromasm-methods)
  - [Step 4: Create a ScriptSignature If Necessary](#step-4-create-a-scriptsignature-if-necessary)
  - [Step 5: Add to ScriptSignature.fromAsm If Applicable](#step-5-add-to-scriptsignaturefromasm-if-applicable)
  - [Step 6: Create Relevant BitcoinUTXOSpendingInfo](#step-6-create-relevant-bitcoinutxospendinginfo)
    - [Non-Nested Single-Key Spending Info](#non-nested-single-key-spending-info)
    - [Non-Nested Multi-Key Spending Info](#non-nested-multi-key-spending-info)
    - [Nested Spending Info](#nested-spending-info)
  - [Step 7: Add to Relevant Apply Methods](#step-7-add-to-relevant-apply-methods)
  - [Step 8: Create a Signer](#step-8-create-a-signer)
    - [Non-Nested Single-Key Spending Info](#non-nested-single-key-spending-info-1)
    - [Non-Nested Multi-Key Spending Info](#non-nested-multi-key-spending-info-1)
    - [Nested Spending Info](#nested-spending-info-1)
  - [Step 9: Add to BitcoinSigner.sign](#step-9-add-to-bitcoinsignersign)
  - [Step 10: Add to ScriptGenerators](#step-10-add-to-scriptgenerators)
    - [ScriptPubKey Generator](#scriptpubkey-generator)
    - [ScriptSignature Generator](#scriptsignature-generator)
    - [ScriptPubKey with Paired ScriptSignature Generator](#scriptpubkey-with-paired-scriptsignature-generator)
  - [Step 11: Add to CreditingTxGen](#step-11-add-to-creditingtxgen)
  - [Step 12: Fix all Non-Exhaustive Matches](#step-12-fix-all-non-exhaustive-matches)
  - [Step 13: Run tests and debug](#step-13-run-tests-and-debug)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Adding a New ScriptPubKey Type

In this document, we will describe how to add new script implementations and types in Bitcoin-S. We will use the following script template example which we have called P2PK with Timeout to illustrate the process:

```
OP_IF
    <Public Key>
OP_ELSE
    <Timeout> OP_CHECKSEQUENCEVERIFY OP_DROP
    <Timeout Public Key>
OP_ENDIF
OP_CHECKSIG
```

Here is [the actual pull request](https://github.com/bitcoin-s/bitcoin-s/pull/967) in which a very similar `ScriptPubKey` is implemented in Bitcoin-S.

Please note that this document only explains how to add new `RawScriptPubKey`s which are the subset of `ScriptPubKey`s which are fully described by their raw scripts. This is to say that this guide will not help in implementing a new segwit version, but should be helpful for most anything else.

It is also important to note that all new scripts should be implemented as if they are to appear on-chain without any P2SH or P2WSH. Bitcoin-S already supports conversions from raw on-chain scripts to these formats in the constructors for the script hash schemes which does not require extra support for new script types.

## Step 0: Design Philosophy

Bitcoin-S strives to have script types defined in such a way that they can be easily composed and reused. Before going through this guide and implementing a really large script template type, try to decompose your script into smaller re-usable pieces.

Also remember to consider what existing pieces you can use. For example, `LockTimeScriptPubKey`s are implemented in such a way that any other `RawScriptPubKey` can be given a time lock by nesting it within a `LockTimeScriptPubKey` subtype. Likewise, `ConditionalScriptPubKey`s are built to allow any other `RawScriptPubKey` type to populate both the `OP_IF/OP_NOTIF` case and the `OP_ELSE` case.

## Step 1: Create a New ScriptPubKey Trait

Go to `ScriptPubKey.scala` and add a new trait:

```scala mdoc:compile-only
sealed trait P2PKWithTimeoutScriptPubKey extends RawScriptPubKey
```

You will then want to add all of the relevant accessor methods. For our case of P2PKWithTimeout, this will mean giving access to the public key, timeout, and timeout public key. Lastly, you will want to add a scaladoc. In total, we get the following result:

```scala mdoc:silent
/** The type for ScriptPubKeys of the form:
  * OP_IF
  *   <Public Key>
  * OP_ELSE
  *   <Timeout> OP_CHECKSEQUENCEVERIFY OP_DROP
  *   <Timeout Public Key>
  * OP_ENDIF
  * OP_CHECKSIG
  */
sealed trait P2PKWithTimeoutScriptPubKey extends RawScriptPubKey {

  lazy val pubKey: ECPublicKey =
    ECPublicKey.fromBytes(asm(2).bytes)

  lazy val lockTime: ScriptNumber = ScriptNumber.fromBytes(asm(5).bytes)

  lazy val timeoutPubKey: ECPublicKey =
    ECPublicKey.fromBytes(asm(9).bytes)
}
```
```scala mdoc:invisible
implicit def realToFakeP2PKWithTimeoutSPK(real: org.bitcoins.core.protocol.script.P2PKWithTimeoutScriptPubKey): P2PKWithTimeoutScriptPubKey = ???
implicit def fakeToRealRawP2PKWithTimeoutSPK(fake: P2PKWithTimeoutScriptPubKey): org.bitcoins.core.protocol.script.P2PKWithTimeoutScriptPubKey = ???
```

## Step 2: Create Companion Object

We now need a companion object which will fulfill four functionalities for us:

1. Contain a concrete `Impl` class for our SPK type
   - This simply means creating a `private case class` wrapping `asm: Vector[ScriptToken]`
2. Create a `fromAsm` constructor
   - This should be a simple call to `buildScript` which is inherited from `ScriptFactory`
3. Create a logical constructor from Bitcoin-S types
   - This means creating an `apply` method that takes in logical BItcoin-S types and constructs asm
   - Note that this may require the use of `BitcoinScriptUtil.calculatePushOp`
4. Create an ASM filter which will detect if a given `Vector[ScriptToken]` corresponds to our type

This looks like the following:

```scala mdoc:silent
object P2PKWithTimeoutScriptPubKey
    extends ScriptFactory[P2PKWithTimeoutScriptPubKey] {
  private case class P2PKWithTimeoutScriptPubKeyImpl(asm: Vector[ScriptToken])
      extends P2PKWithTimeoutScriptPubKey

  override def fromAsm(asm: Seq[ScriptToken]): P2PKWithTimeoutScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = P2PKWithTimeoutScriptPubKeyImpl.apply,
      invariant = isP2PKWithTimeoutScriptPubKey,
      errorMsg = s"Given asm was not a P2PKWithTimeoutScriptPubKey, got $asm"
    )
  }

  def apply(
      pubKey: ECPublicKey,
      lockTime: ScriptNumber,
      timeoutPubKey: ECPublicKey): P2PKWithTimeoutScriptPubKey = {
    val timeoutAsm = CSVScriptPubKey(lockTime, EmptyScriptPubKey).asm.toVector
    val pubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(pubKey.bytes)
      .toVector ++ Vector(ScriptConstant(pubKey.bytes))
    val timeoutPubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(timeoutPubKey.bytes)
      .toVector ++ Vector(ScriptConstant(timeoutPubKey.bytes))

    P2PKWithTimeoutScriptPubKeyImpl(
      Vector(Vector(OP_IF),
             pubKeyAsm,
             Vector(OP_ELSE),
             timeoutAsm,
             timeoutPubKeyAsm,
             Vector(OP_ENDIF, OP_CHECKSIG)).flatten
    )
  }

  def isP2PKWithTimeoutScriptPubKey(asm: Seq[ScriptToken]): Boolean = {
    if (asm.length == 12) {
      val pubKey = ECPublicKey.fromBytes(asm(2).bytes)
      val lockTimeTry = Try(ScriptNumber.fromBytes(asm(5).bytes))
      val timeoutPubKey = ECPublicKey.fromBytes(asm(9).bytes)

      lockTimeTry match {
        case Success(lockTime) =>
          asm == P2PKWithTimeoutScriptPubKey(pubKey, lockTime, timeoutPubKey).asm
        case Failure(_) => false
      }
    } else {
      false
    }
  }
}
```

## Step 3: Add to Relevant fromAsm Methods

We now need to ensure that `ScriptPubKey.fromAsm(p2pkWithTimeoutSPK.asm)` returns our type. Since `P2PKWithTimeoutScriptPubKey extends RawScriptPubKey`, this means we must add to `RawScriptPubKey.fromAsm`. Note that order in this function's `match` can matter. Since our type is more specific than any other currently existing type, we put our new `case` at the top:

```scala mdoc:compile-only
asm match {
    case Nil => EmptyScriptPubKey
    case _ if P2PKWithTimeoutScriptPubKey.isP2PKWithTimeoutScriptPubKey(asm) =>
      P2PKWithTimeoutScriptPubKey.fromAsm(asm)
    //...
}
```

## Step 4: Create a ScriptSignature If Necessary

Often times a new `ScriptSignature` type will be necessary when introducing a new `ScriptPubKey` type. When this is the case, the procedure for adding a new `ScriptSignature` is more or less identical to steps 1 and 2 above. Here is what this looks like for `P2PKScriptPubKey` (note, this is not `P2PKWithTimeoutScriptPubKey`):

```scala mdoc:compile-only
/**
  * Represents a pay to public key script signature
  * https://bitcoin.org/en/developer-guide#pubkey
  * Signature script: <sig>
  */
sealed trait P2PKScriptSignature extends ScriptSignature {

  /** PubKey scriptSignatures only have one signature */
  def signature: ECDigitalSignature = signatures.head

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(asm).head.hex))
  }

  override def toString = s"P2PKScriptSignature($signature)"
}

object P2PKScriptSignature extends ScriptFactory[P2PKScriptSignature] {
  private case class P2PKScriptSignatureImpl(
      override val asm: Vector[ScriptToken])
      extends P2PKScriptSignature

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptSignature = {
    buildScript(asm.toVector,
                P2PKScriptSignatureImpl(_),
                isP2PKScriptSignature(_),
                "The given asm tokens were not a p2pk script sig: " + asm)
  }

  def apply(signature: ECDigitalSignature): P2PKScriptSignature = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val signatureConstant = ScriptConstant(signature.bytes)
    val asm = pushOps ++ Seq(signatureConstant)
    P2PKScriptSignature.fromAsm(asm)
  }

  /** P2PK scriptSigs always have the pattern [pushop, digitalSignature] */
  def isP2PKScriptSignature(asm: Seq[ScriptToken]): Boolean = asm match {
    case Seq(_: BytesToPushOntoStack, _: ScriptConstant) => true
    case _                                               => false
  }
}
```

However, it is sometimes not necessary to create a new `ScriptSignature` type for every new `ScriptPubKey`. This is because we want to maintain unique representations for every `ScriptSignature`, and it turns out that in our case of `P2PKWithTimeoutScriptPubKey`, script signatures are of the form

```
<boolean> <signautre>
```

which is already represented by `ConditionalScriptSignature`. When this happens, you only need to create an `object` for your new type, and then follow step 2 above, skipping the first part (adding an Impl `case class`):

```scala mdoc:silent
object P2PKWithTimeoutScriptSignature
    extends ScriptFactory[ConditionalScriptSignature] {
  override def fromAsm(asm: Seq[ScriptToken]): ConditionalScriptSignature = {
    buildScript(
      asm.toVector,
      ConditionalScriptSignature.fromAsm,
      isP2PKWithTimeoutScriptSignature,
      s"The given asm tokens were not a P2PKWithTimeoutScriptSignature, got $asm"
    )
  }

  def apply(
      beforeTimeout: Boolean,
      signature: ECDigitalSignature): ConditionalScriptSignature = {
    ConditionalScriptSignature(P2PKScriptSignature(signature), beforeTimeout)
  }

  def isP2PKWithTimeoutScriptSignature(asm: Seq[ScriptToken]): Boolean = {
    P2PKScriptSignature.isP2PKScriptSignature(asm.dropRight(1)) && ConditionalScriptSignature
      .isValidConditionalScriptSig(asm)
  }
}
```

Remember that in all of them above, `ScriptSignature`s are written as if they are to appear on-chain in transaction inputs rather than transaction witnesses, since Bitcoin-S supports turning any raw `ScriptSignature` into a `P2WSHWitness` without requiring explicit support for new script types.

## Step 5: Add to ScriptSignature.fromAsm If Applicable

If you added a new `ScriptSignature` type in the previous step, you must add a `case` to the `match` statement in `ScriptSignature.fromAsm` at the bottom of `ScriptSignature.scala`. For `P2PKScriptSignature` (note that this does not apply to `P2PKWithTimeoutScriptSignature` since there is no new unique type for this `ScriptSignature`), this looks like:

```scala mdoc:compile-only
tokens match {
  //...
  case _ if P2PKScriptSignature.isP2PKScriptSignature(tokens) =>
      P2PKScriptSignature.fromAsm(tokens)
  //...
}
```

## Step 6: Create Relevant BitcoinUTXOSpendingInfo

`BitcoinUTXOSpendingInfo` is the Bitcoin-S data structure for the information required to spend from a given `ScriptPubKey`. Hence, when defining new script types, it is important to define how they are spent as well so that they can be useful.

There are three distinct kinds of scripts when it comes to signing in Bitcoin-S: scripts that have nesting (such as `ConditionalScriptPubKey`, `LockTimeScriptPubKey`), scripts without nesting but which involve multiple signing keys (such as `MultiSignatureScriptPubKey`), and scripts without nesting and which require only one key for signing (such as `P2PKWithTimeoutScriptPubKey`, `P2PKHScriptPubKey`). We will cover each of these cases in turn, starting with the last case as it applies to our example of `P2PKWithTimeout`.

### Non-Nested Single-Key Spending Info

This is the easiest case and only requires creating a new `case class` in `UTXOSpendingInfo.scala` which extends `RawScriptUTXOSpendingInfoFull with RawScriptUTXOSpendingInfoSingle` and which contains in its parameters, all of the info required for spending. Make sure to also validate any data in these parameters using `require` statements. Make sure to `override` both `requiredSigs: Int = 1` and `conditionalPath: ConditionalPath` to be either whatever is needed based on your parameters, or `ConditionalPath.NoConditionsLeft` if your script does not use any conditionals. Here is what this looks like for `P2PKWithTimeout`:

```scala mdoc:silent
case class P2PKWithTimeoutSpendingInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    override val signer: Sign,
    hashType: HashType,
    isBeforeTimeout: Boolean)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(
    scriptPubKey.pubKey == signer.publicKey || scriptPubKey.timeoutPubKey == signer.publicKey,
    "Signer pubkey must match ScriptPubKey")

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
}
```

### Non-Nested Multi-Key Spending Info

For new script types which require multiple keys to spend, we must make two separate `case class`es, one for normal spending (which extends `RawScriptUTXOSpendingInfoFull`) and one for signing a transaction with a single key (which extends `RawScriptUTXOSpendingInfoSingle`). We then make a `sealed trait` for the both of them to extend. In this `trait`, you want to make sure to `override def scriptPubKey` to have your new `ScriptPubKey` type as well as overriding any other members of `RawScriptUTXOSpendingInfo` which are common to both classes. In total, this all works out to the following for `MutliSignatureScriptPubKey`:

```scala mdoc:compile-only
sealed trait MultiSignatureSpendingInfo extends RawScriptUTXOSpendingInfo {
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
  override def scriptPubKey: MultiSignatureScriptPubKey
}

case class MultiSignatureSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    signer: Sign,
    hashType: HashType
) extends MultiSignatureSpendingInfo
    with RawScriptUTXOSpendingInfoSingle

case class MultiSignatureSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfoFull
    with MultiSignatureSpendingInfo {
  require(signersWithPossibleExtra.length >= scriptPubKey.requiredSigs,
          s"Not enough signers!: $this")

  override val requiredSigs: Int = scriptPubKey.requiredSigs

  override val signers: Vector[Sign] =
    signersWithPossibleExtra.take(requiredSigs)

  override def toSingle(signerIndex: Int): MultiSignatureSpendingInfoSingle = {
    MultiSignatureSpendingInfoSingle(outPoint,
                                     amount,
                                     scriptPubKey,
                                     signers(signerIndex),
                                     hashType)
  }

  /** @inheritdoc */
  override def toSingles: Vector[MultiSignatureSpendingInfoSingle] = {
    signers.map { signer =>
      MultiSignatureSpendingInfoSingle(outPoint,
                                       amount,
                                       scriptPubKey,
                                       signer,
                                       hashType)
    }
  }
}
```

### Nested Spending Info

This case is very similar to the above where we need to create two case classes one for normal spending (which extends `RawScriptUTXOSpendingInfoFull`) and one for signing a transaction with a single key (which extends `RawScriptUTXOSpendingInfoSingle`). As well as needing to create a `sealed trait` for them both to extend which overrides `scriptPubKey` as well as any other commonalities from the sub-classes. The one new thing in the nested case is that we must create a `def nestedSpendingInfo: RawScriptUTXOSpendingInfo` in our `trait` and make sure to override `signers` and `requiredSigs` from this `nestedSpendingInfo` in the subclass extending `RawScriptUTXOSpendingInfoFull`. For the case of spending `LockTimeScriptPubKey`s, this looks like the following:

```scala mdoc:compile-only
sealed trait LockTimeSpendingInfo extends RawScriptUTXOSpendingInfo {
  override def scriptPubKey: LockTimeScriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class LockTimeSpendingInfoSingle(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    signer: Sign,
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends LockTimeSpendingInfo
    with RawScriptUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptPubKey.nestedScriptPubKey,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

case class LockTimeSpendingInfoFull(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType,
    conditionalPath: ConditionalPath
) extends RawScriptUTXOSpendingInfoFull
    with LockTimeSpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  scriptPubKey.nestedScriptPubKey,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}
```

## Step 7: Add to Relevant Apply Methods

Now that we have created our new `RawScriptUTXOSpendingInfoFull` and `RawScriptUTXOSpendingInfoSingle`, possibly in the same class, we need to add them to the general-purpose spending info constructors. This means adding a `case` to both `RawScriptUTXOSpendingInfoSingle.apply` and `RawScriptUTXOSpendingInfoFull.apply` for your new `ScriptPubKey` type which constructs your relevant `RawScriptUTXOSpendingInfoSingle` and `RawScriptUTXOSpendingInfoFull` from generic types (given as parameters in the `apply` methods). For `P2PKWithTimeout`, both of these cases look like the following:

```scala mdoc:compile-only
    scriptPubKey match {
      //...
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(outPoint,
                                        amount,
                                        p2pkWithTimeout,
                                        signer,
                                        hashType,
                                        beforeTimeout)
        }
      //...
    }
```

## Step 8: Create a Signer

We must now add signing functionality for our new script type within `Signer.scala`. Once again, we have three different cases depending on your new script type.

### Non-Nested Single-Key Spending Info

For this case, all we must do is create a new class which extends `RawSingleKeyBitcoinSigner` and implements `keyAndSigToScriptSig`. For `P2PKWithTimeout` this looks like the following:

```scala mdoc:silent
sealed abstract class P2PKWithTimeoutSigner
    extends RawSingleKeyBitcoinSigner[P2PKWithTimeoutSpendingInfo] {

  override def keyAndSigToScriptSig(
      key: ECPublicKey,
      sig: ECDigitalSignature,
      spendingInfo: P2PKWithTimeoutSpendingInfo): ScriptSignature = {
    P2PKWithTimeoutScriptSignature(spendingInfo.isBeforeTimeout, sig)
  }
}

object P2PKWithTimeoutSigner extends P2PKWithTimeoutSigner
```

### Non-Nested Multi-Key Spending Info

In this case we must create a new `BitcoinSignerFull`, which requires implementing the `sign` function. For `MultiSignature` this looks like the following:

```scala mdoc:compile-only
sealed abstract class MultiSigSigner
    extends BitcoinSigner[MultiSignatureSpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfoFull,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: MultiSignatureSpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) =
      relevantInfo(spendingInfo, unsignedTx)

    val keysAndSigsF = spendingInfo.toSingles.map { spendingInfoSingle =>
      signSingle(spendingInfoSingle, unsignedTx, isDummySignature)
    }

    val signaturesF = Future.sequence(keysAndSigsF).map(_.map(_.signature))

    val scriptSigF = signaturesF.map { sigs =>
      MultiSignatureScriptSignature(sigs)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}

object MultiSigSigner extends MultiSigSigner
```

### Nested Spending Info

When signing for a nested script structure, we must create a new `BitcoinSignerFull`. You will need to make a delegating call with the `nestedSpendingInfo` to `BitcoinSigner.sign`, but you may also need to do whatever else is needed with the nested result to construct a correct `ScriptSignature`. For `ConditionalScriptSignature`, this all looks like:

```scala mdoc:compile-only
/** Delegates to get a ScriptSignature for the case being
  * spent and then adds an OP_TRUE or OP_FALSE
  */
sealed abstract class ConditionalSigner
    extends BitcoinSigner[ConditionalSpendingInfoFull] {

  override def sign(
      spendingInfo: UTXOSpendingInfoFull,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      spendingInfoToSatisfy: ConditionalSpendingInfoFull)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)

    val missingOpSigComponentF = BitcoinSigner.sign(
      spendingInfo,
      unsignedTx,
      isDummySignature,
      spendingInfoToSatisfy.nestedSpendingInfo)

    val scriptSigF = missingOpSigComponentF.map { sigComponent =>
      ConditionalScriptSignature(sigComponent.scriptSignature,
                                 spendingInfoToSatisfy.condition)
    }

    updateScriptSigInSigComponent(unsignedTx,
                                  inputIndex.toInt,
                                  output,
                                  scriptSigF)
  }
}
object ConditionalSigner extends ConditionalSigner
```

## Step 9: Add to BitcoinSigner.sign

We must now add the new signing functionality from the previous step to the general-purpose signing functions by adding a new `case` for your new `ScriptPubKey` type in the `match` within `BitcoinSigner.sign`. In the case of `P2PKWithTimeout`, this looks like:

```scala mdoc:compile-only
    spendingInfoToSatisfy match {
      //...
      case p2pKWithTimeout: P2PKWithTimeoutSpendingInfo =>
        P2PKWithTimeoutSigner.sign(spendingInfo,
                                   unsignedTx,
                                   isDummySignature,
                                   p2pKWithTimeout)
      //...
    }
```

We have now fully implemented the new script type! But have we done it correctly? We must now add the new script type to the Bitcoin-S test framework so that our scripts get added to existing Bitcoin-S property-based tests.

## Step 10: Add to ScriptGenerators

The first step to adding our new script type to Bitcoin-S property-based tests is creating generators for our new `ScriptPubKey` and `ScriptSignature` types in `ScriptGenerators.scala`.

It is important to note that in the current Bitcoin-S generator framework for `ScriptPubKey`s, all conditionals always spend only their `OP_TRUE` cases.

### ScriptPubKey Generator

Let's start by creating a generator for our `ScriptPubKey`, this generator should also return the private keys that were used to create the `ScriptPubKey`. To construct this `Gen`, you will likely need to use other generators for the internal structures in your script such as keys and lock times. For `P2PKWithTimeout` this looks like:

```scala mdoc:compile-only
  def p2pkWithTimeoutScriptPubKey: Gen[
    (P2PKWithTimeoutScriptPubKey, Seq[ECPrivateKey])] =
    for {
      privKey <- CryptoGenerators.privateKey
      timeoutPrivKey <- CryptoGenerators.privateKey
      lockTime <- NumberGenerator.timeLockScriptNumbers
    } yield {
      (P2PKWithTimeoutScriptPubKey(privKey.publicKey,
                                   lockTime,
                                   timeoutPrivKey.publicKey),
       Vector(privKey, timeoutPrivKey))
    }
```

Note that the private key used in the `OP_TRUE` case is the `head` of the `Seq[ECPrivateKey]` returned. This makes it possible for tests that only spend the `OP_TRUE` case to find the correct key, as it is expected to be the first one.

We must now add this `Gen` to all of the following `def`s in `ScriptGenerators.scala`: `randomNonP2SHScriptPubKey, scriptPubKey, nonWitnessScriptPubKey, nonConditionalRawScriptPubKey, rawScriptPubKey`, and if your `ScriptPubKey` has no lock times, you must also add the above `Gen` to `nonConditionalNonLocktimeRawScriptPubKey, nonLocktimeRawScriptPubKey` as well.

### ScriptSignature Generator

We must also create a generator for our `ScriptSignature` type, even if we did not introduce a new `ScriptSignature` type (in our example of `P2PKWithTimeout` we use a specific form of `ConditionalScriptSignature`). Once again you will likely need to use other existing generators. For `P2PKWithTimeoutScriptSignature`, this looks like:

```scala mdoc:compile-only
  def p2pkWithTimeoutScriptSignature: Gen[ConditionalScriptSignature] =
    for {
      privKey <- CryptoGenerators.privateKey
      hash <- CryptoGenerators.doubleSha256Digest
      hashType <- CryptoGenerators.hashType
      signature = ECDigitalSignature.fromBytes(
        privKey.sign(hash).bytes ++ ByteVector.fromByte(hashType.byte))
      beforeTimeout <- NumberGenerator.bool
    } yield P2PKWithTimeoutScriptSignature(beforeTimeout, signature)
```

We now add this `Gen` to `scriptSignature: Gen[ScriptSignature]` as well as adding a case for our new `ScriptPubKey` type in `pickCorrespondingScriptSignature` which should return our new `ScriptSignature` generator.  If our `ScriptPubKey` does not have any lock times, you should also add this script signature `Gen` to `nonLockTimeConditionalScriptSignature` and `randomNonLockTimeScriptSig`.

### ScriptPubKey with Paired ScriptSignature Generator

Lastly, we need to construct a generator that returns both a `ScriptPubKey` and a `ScriptSignature` signing that that `ScriptPubKey`. All keys used in signing should also be returned. This all should be done by using the above `ScriptPubKey` generator, then constructing an `ScriptSignature` for your type where all actual signatures are `EmptyDigitalSignature`s. A `SpendingInfoFull` should then be constructed for the generated `ScriptPubKey` (using the private keys generated in the same line). Finally, a `TxSignatureComponent` should be created by using the new `Signer` for our script type. From this `TxSignatureComponent`, a `ScriptSignature` is readily available. For `P2PKWithTimeout`, this generator looks like:

```scala mdoc:compile-only
  def signedP2PKWithTimeoutScriptSignature: Gen[
    (ConditionalScriptSignature, P2PKWithTimeoutScriptPubKey, ECPrivateKey)] =
    for {
      (spk, privKeys) <- p2pkWithTimeoutScriptPubKey
      hashType <- CryptoGenerators.hashType
    } yield {
      val privKey = privKeys.head
      val emptyScriptSig = P2PKWithTimeoutScriptSignature(beforeTimeout = true,
                                                          EmptyDigitalSignature)
      val (creditingTx, outputIndex) =
        TransactionGenerators.buildCreditingTransaction(spk)
      val (spendingTx, inputIndex) = TransactionGenerators
        .buildSpendingTransaction(creditingTx, emptyScriptSig, outputIndex)
      val spendingInfo = P2PKWithTimeoutSpendingInfo(
        TransactionOutPoint(creditingTx.txIdBE, inputIndex),
        creditingTx.outputs(outputIndex.toInt).value,
        spk,
        privKey,
        hashType,
        isBeforeTimeout = true)
      val txSigComponentF = P2PKWithTimeoutSigner.sign(spendingInfo,
                                                       spendingTx,
                                                       isDummySignature = false)
      val txSigComponent = Await.result(txSigComponentF, timeout)
      val signedScriptSig =
        txSigComponent.scriptSignature.asInstanceOf[ConditionalScriptSignature]

      (signedScriptSig, spk, privKey)
    }
```

I strongly advise you also look at at least one other `Gen` of this kind before writing your own.

## Step 11: Add to CreditingTxGen

Now that we have generators constructed for `ScriptPubKey`s, `ScriptSignature`s and their pairings completed, we will create a generator for our type's `SpendingInfoFull`. This should usually be as simple as mapping on the `ScriptPubKey` generator in `ScriptGenerators` and calling `build` (within `CreditinTxGen.scala`). We then also create another generator which returns lists of `SpendingInfo`s generated by the previous `Gen`. For `P2PKWithTimeout`, this looks like:

```scala mdoc:compile-only
  def p2pkWithTimeoutOutput: Gen[BitcoinUTXOSpendingInfoFull] = {
    ScriptGenerators.p2pkWithTimeoutScriptPubKey.flatMap { p2pkWithTimeout =>
      build(p2pkWithTimeout._1, Seq(p2pkWithTimeout._2.head), None, None)
    }
  }

  def p2pkWithTimeoutOutputs: Gen[Seq[BitcoinUTXOSpendingInfoFull]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkWithTimeoutOutput))
  }
```

We must then add our output `Gen` to one of `cltvOutputGens` or `nonCLTVOutputGens` depending on whether the `ScriptPubKey` type has CLTVs (absolute lock times) or not. We must also add our output `Gen` to `nonP2SHOutput`, and also to `nonSHOutput` and `nonP2WSHOutput` in the case that your `ScriptPubKey` type has no CLTVs.

## Step 12: Fix all Non-Exhaustive Matches

All we have left is to clean up our code and make sure that nothing has been missed. Within an `sbt` terminal, you should run the following sequence of commands:

```bashrc
clean
project coreTest
test:compile
```

This should have quite a lengthy output but we are only interested in any compiler errors there may be, as well as non-exhaustive match compiler warnings. You should first fix any compiler errors you encounter, and then you can get the warnings again by running `clean` and then running `test:compile` again.

The warnings we're interested in should look something like this:

```
[warn] /home/nkohen/Desktop/SuredBits/bitcoin-s-core/testkit/src/main/scala/org/bitcoins/testkit/core/gen/ScriptGenerators.scala:524:59: match may not be exhaustive.
[warn] It would fail on the following input: P2PKWithTimeoutScriptPubKeyImpl(_)
[warn]       scriptPubKey: ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
[warn]                                                           ^
[warn] one warning found
```

You may get these warnings for your new `ScriptSignature` type as well. These are places where the compiler expects there to be defined functionality in a pattern match where one of our new types is a possibility, but for which no functionality is defined. You must go to each of these warnings and add a `case` for the relevant new type, or add this new type to an existing case when applicable.

## Step 13: Run tests and debug

Lastly, once everything is compiling nicely, all that is left is to run tests and debug. While within an `sbt` terminal session, run the following two commands to run the relevant tests:

```
project coreTest
test
```

If all tests pass we are all done! If you encounter any test failures, you can re-run individual tests using the `testOnly` command which must be given the full name of the test you wish to run (these names should be at the bottom of the testing output and look something like `org.bitcoins.core.script.interpreter.ScriptInterpreterTest`).