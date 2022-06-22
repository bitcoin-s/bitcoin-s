package org.bitcoins.core.protocol.script

/** Represents the transaction digest algorithm for signature verification in Bitcoin Core
  * With the implementation of segwit, we have added different algorithm, the first alternative being BIP143
  * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]]
  * [[https://github.com/bitcoin/bitcoin/blob/53133c1c041d113c2a480a18e6ff38681d135dca/src/script/interpreter.h#L120-L124]]
  */
sealed trait SignatureVersion

/** The original digest algorithm created by Satoshi */
case object SigVersionBase extends SignatureVersion

/** The digest algorithm implemented by BIP143 [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]] */
case object SigVersionWitnessV0 extends SignatureVersion

sealed trait SigVersionTaproot extends SignatureVersion

/** For keypath spends
  * @see https://github.com/bitcoin/bitcoin/blob/e826b22da252e0599c61d21c98ff89f366b3120f/src/script/interpreter.h#L191
  */
case object SigVersionTaprootKeySpend extends SigVersionTaproot

/** For script path spends
  * @see https://github.com/bitcoin/bitcoin/blob/e826b22da252e0599c61d21c98ff89f366b3120f/src/script/interpreter.h#L192
  */
case object SigVersionTapscript extends SigVersionTaproot
