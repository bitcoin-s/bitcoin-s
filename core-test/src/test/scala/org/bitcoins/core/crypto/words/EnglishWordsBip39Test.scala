package org.bitcoins.core.crypto.words

import org.bitcoins.testkit.util.BitcoinSUnitTest

import scala.io.Source

class EnglishWordsBip39Test extends BitcoinSUnitTest {
  behavior of "EnglishWords"

  private val ENGLISH_WORDS_FILE = "/bip39-wordlists/english.txt"

  it must "have the same in memory and file contents" in {
    val source = Source.fromURL(getClass.getResource(ENGLISH_WORDS_FILE))

    val lines = source.getLines()
    val linesVec = lines.toVector
    source.close()
    assert(EnglishWordsBip39.getWords == linesVec)
  }
}
