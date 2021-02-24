package org.bitcoins.core.number

/** Helper trait to cache data types that represent numbers
  * Examples are [[org.bitcoins.core.script.constant.ScriptNumber]]
  * [[UInt32]] [[UInt64]] etc
  */
trait NumberCache[T] {
  def fromNativeNumber(long: Long): T

  /** The minimum number cached (inclusive) */
  def minCached: Long = 0

  /** The max number cached (inclusive) */
  def maxCached: Long = 255

  private lazy val cache: Vector[T] = {
    minCached.to(maxCached).map(fromNativeNumber).toVector
  }

  /** Checks if the given number is cached
    * if not, allocates a new object to represent the number
    */
  def checkCached(long: Long): T = {
    if (long <= maxCached && long >= minCached) cache(long.toInt)
    else {
      fromNativeNumber(long)
    }
  }
}

/** Number cache, except for scala [[BigInt]] */
trait NumberCacheBigInt[T] extends NumberCache[T] {

  private val bigIntCache: Vector[T] = {
    minCachedBigInt
      .to(maxCachedBigInt)
      .map(fromBigInt)
      .toVector
  }

  def fromBigInt(bigInt: BigInt): T

  /** The minimum number cached (inclusive) */
  def minCachedBigInt: BigInt = BigInt(minCached)

  /** The max number cached (inclusive) */
  def maxCachedBigInt: BigInt = BigInt(maxCached)

  /** [[org.bitcoins.core.protocol.CompactSizeUInt]] uses a UInt64
    * which means we have larger uint64s used on a regular basis
    */
  override def maxCached: Long = 2048

  /** Checks if the given number is cached
    * if not, allocates a new object to represent the number
    */
  def checkCachedBigInt(bigInt: BigInt): T = {
    if (bigInt <= maxCachedBigInt && bigInt >= minCachedBigInt)
      bigIntCache(bigInt.toInt)
    else {
      fromBigInt(bigInt)
    }
  }
}
