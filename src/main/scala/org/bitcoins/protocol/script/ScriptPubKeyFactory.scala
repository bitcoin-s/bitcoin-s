package org.bitcoins.protocol.script

import org.bitcoins.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.bitcoins.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.bitcoins.script.stack.OP_DUP
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSUtil, Factory}

/**
 * Created by chris on 1/19/16.
 */

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator

