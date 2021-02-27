package org.bitcoins.core.crypto.words

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import scala.io.Source

class EnglishWordsBip39Test extends BitcoinSUnitTest {
  behavior of "EnglishWords"

  private val ENGLISH_WORDS_FILE = "/bip39-wordlists/english.txt"

  it must "have the same in memory and file contents" in {
    val resourceStream = getClass.getResourceAsStream(ENGLISH_WORDS_FILE)
    val source = Source.fromInputStream(resourceStream)

    val lines = source.getLines()
    val linesVec = lines.toVector
    source.close()
    assert(EnglishWordsBip39.getWords == linesVec)
  }
}
