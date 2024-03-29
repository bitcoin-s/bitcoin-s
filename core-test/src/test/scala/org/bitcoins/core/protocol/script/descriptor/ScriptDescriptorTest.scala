package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.RawScriptPubKey
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ScriptDescriptorTest extends BitcoinSUnitTest {

  behavior of "ScriptDescriptor"

  it must "parse a raw script descriptor" in {
    val str = "raw(deadbeef)#89f8spxm"
    val desc = ScriptDescriptor.fromString(str)
    assert(desc.scriptPubKey == RawScriptPubKey.fromAsmHex("deadbeef"))
    assert(desc.checksum.get == "89f8spxm")
    assert(desc.toString == str)

  }
}
