package org.bitcoins.core.script

import org.bitcoins.testkit.util.BitcoinSUnitTest

class ScriptTypeTest extends BitcoinSUnitTest {
  behavior of "ScriptType"

  it must "have serialization symmetry" in {
    ScriptType.all.foreach { scriptType =>
      val newScriptType = ScriptType.fromString(scriptType.toString)

      assert(newScriptType.contains(scriptType))
    }
  }

  it must "fail when nonsense ScriptType is used" in {
    val lyrics = "Never gonna give you up, never gonna let you down"

    assert(ScriptType.fromString(lyrics).isEmpty)
    assertThrows[IllegalArgumentException](ScriptType.fromStringExn(lyrics))
  }
}
