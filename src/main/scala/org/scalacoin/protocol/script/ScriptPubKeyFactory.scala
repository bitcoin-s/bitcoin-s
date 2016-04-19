package org.scalacoin.protocol.script

import org.scalacoin.marshallers.script.{RawScriptPubKeyParser, ScriptParser}
import org.scalacoin.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto.{OP_CHECKMULTISIGVERIFY, OP_CHECKMULTISIG, OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util.{BitcoinScriptUtil, BitcoinSUtil, Factory}

/**
 * Created by chris on 1/19/16.
 */

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm : Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes : Seq[Byte]) extends ScriptPubKeyUpdateIndicator

