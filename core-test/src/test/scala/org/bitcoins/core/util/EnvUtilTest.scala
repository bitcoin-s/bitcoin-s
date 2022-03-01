package org.bitcoins.core.util

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class EnvUtilTest extends BitcoinSUnitTest {

  behavior of "EnvUtil"

  it must "calculate the number of commits since last tag" in {
    val versionString = "1.9.0-10-eddcc94b-SNAPSHOT"
    val numCommitsOpt = EnvUtil.parseCommitsSinceLastTag(versionString)
    assert(numCommitsOpt.get == 10)
  }

  it must "calculate no commits for an official release" in {
    val versionString = "1.9.0"
    val numCommitsOpt = EnvUtil.parseCommitsSinceLastTag(versionString)
    assert(numCommitsOpt.isEmpty)
  }
}
