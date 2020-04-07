package org.bitcoins.core.crypto

import scodec.bits.ByteVector

case class ECAdaptorSignature(sigBytes: ByteVector, proofBytes: ByteVector)
