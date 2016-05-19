package org.bitcoins.core.protocol.script

import org.bitcoins.core.serializers.script.{RawScriptPubKeyParser, ScriptParser}
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util.{BitcoinScriptUtil, BitcoinSUtil, Factory}

/**
 * Created by chris on 1/19/16.
 */

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator

