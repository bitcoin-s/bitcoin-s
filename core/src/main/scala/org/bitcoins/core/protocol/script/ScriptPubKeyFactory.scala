package org.bitcoins.core.protocol.script

import org.bitcoins.core.script.constant._

/**
 * Created by chris on 1/19/16.
 */

sealed trait ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyAsm(asm: Seq[ScriptToken]) extends ScriptPubKeyUpdateIndicator
case class UpdateScriptPubKeyBytes(bytes: scodec.bits.ByteVector) extends ScriptPubKeyUpdateIndicator

