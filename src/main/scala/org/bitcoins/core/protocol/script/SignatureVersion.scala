package org.bitcoins.core.protocol.script

/**
 * Represents the transaction digest algorithm for signature verification in Bitcoin Core
 * With the implementation of segwit, we have added different algorithm, the first alternative being BIP143
 * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]]
 * [[https://github.com/bitcoin/bitcoin/blob/53133c1c041d113c2a480a18e6ff38681d135dca/src/script/interpreter.h#L120-L124]]
 */
sealed trait SignatureVersion

/** The original digest algorithm created by Satoshi */
case object SigVersionBase extends SignatureVersion

/** The digest algorithm implemented by BIP143 [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki]] */
case object SigVersionWitnessV0 extends SignatureVersion

