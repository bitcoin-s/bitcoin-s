package org.bitcoins.crypto.facade

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSBracketAccess, JSGlobal, JSImport}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.|

//Copied from
//https://github.com/scalajs-io/nodejs/blob/master/app/common/src/main/scala/io/scalajs/nodejs/buffer/Buffer.scala
/** Prior to the introduction of TypedArray in ECMAScript 2015 (ES6), the JavaScript language had no mechanism for
  * reading or manipulating streams of binary data. The Buffer class was introduced as part of the Node.js API to
  * make it possible to interact with octet streams in the context of things like TCP streams and file system operations.
  *
  * Now that TypedArray has been added in ES6, the Buffer class implements the Uint8Array API in a manner that is more
  * optimized and suitable for Node.js' use cases.
  *
  * Instances of the Buffer class are similar to arrays of integers but correspond to fixed-sized, raw memory
  * allocations outside the V8 heap. The size of the Buffer is established when it is created and cannot be resized.
  *
  * The Buffer class is a global within Node.js, making it unlikely that one would need to ever use require('buffer').
  * @see https://nodejs.org/api/buffer.html
  * @author lawrence.daniels@gmail.com
  */
@js.native
@JSImport("buffer", "Buffer")
class Buffer() extends js.Object {

  /////////////////////////////////////////////////////////////////////////////////
  //      Constructors
  /////////////////////////////////////////////////////////////////////////////////

  /** @example new Buffer(size)
    */
  @inline
  def this(size: Int) = this()

  /** @example new Buffer(str, [encoding])
    */
  @inline
  @deprecated("Use Buffer.from(str[, encoding]) instead.", since = "6.0.0")
  def this(str: String, encoding: String = js.native) = this()

  /** @example new Buffer(array)
    */
  @inline
  @deprecated("Use Buffer.from(array) instead.", since = "6.0.0")
  def this(array: js.Array[Int]) = this()

  /** @example new Buffer(buffer)
    */
  @inline
  @deprecated("Use Buffer.from(buffer) instead.", since = "6.0.0")
  def this(buffer: Buffer) = this()

  /** @example {{{ new Buffer(arrayBuffer[, byteOffset[, length]]) }}}
    */
  @inline
  @deprecated("Use Buffer.from(arrayBuffer[, byteOffset [, length]]) instead.",
              since = "6.0.0")
  def this(arrayBuffer: ArrayBuffer, byteOffset: Int, length: Int) = this()

  /** @example {{{ new Buffer(arrayBuffer[, byteOffset[, length]]) }}}
    */
  @inline
  @deprecated("Use Buffer.from(arrayBuffer[, byteOffset [, length]]) instead.",
              since = "6.0.0")
  def this(arrayBuffer: ArrayBuffer, byteOffset: Int) = this()

  /** @example {{{ new Buffer(arrayBuffer[, byteOffset[, length]]) }}}
    */
  @inline
  @deprecated("Use Buffer.from(arrayBuffer[, byteOffset [, length]]) instead.",
              since = "6.0.0")
  def this(arrayBuffer: ArrayBuffer) = this()

  /////////////////////////////////////////////////////////////////////////////////
  //      Accessors and Mutators
  /////////////////////////////////////////////////////////////////////////////////

  /** The index operator [index] can be used to get and set the octet at position index in buf. The values refer
    * to individual bytes, so the legal value range is between 0x00 and 0xFF (hex) or 0 and 255 (decimal).
    * @param index the given index
    * @return the value at the given index
    */
  @JSBracketAccess
  def apply(index: Int): Int = js.native

  /** The index operator [index] can be used to get and set the octet at position index in buf. The values refer
    * to individual bytes, so the legal value range is between 0x00 and 0xFF (hex) or 0 and 255 (decimal).
    * @param index the given index
    */
  @JSBracketAccess
  def update(index: Int, value: Int): Unit = js.native

  /////////////////////////////////////////////////////////////////////////////////
  //      Methods
  /////////////////////////////////////////////////////////////////////////////////

  /** Compares buf with target and returns a number indicating whether buf comes before, after, or is the same
    * as target in sort order. Comparison is based on the actual sequence of bytes in each Buffer.
    * @param target      A Buffer to compare to
    * @param targetStart The offset within target at which to begin comparison. Default: 0
    * @param targetEnd   The offset with target at which to end comparison (not inclusive). Ignored when targetStart
    *                    is undefined. Default: target.length
    * @param sourceStart The offset within buf at which to begin comparison. Ignored when targetStart is undefined. Default: 0
    * @param sourceEnd   The offset within buf at which to end comparison (not inclusive).
    *                    Ignored when targetStart is undefined. Default: buf.length
    * @return 0 is returned if target is the same as buf
    *         1 is returned if target should come before buf when sorted.
    *         -1 is returned if target should come after buf when sorted.
    * @example {{{ buf.compare(target[, targetStart[, targetEnd[, sourceStart[, sourceEnd]]]]) }}}
    */
  def compare(
      target: Buffer,
      targetStart: Int = js.native,
      targetEnd: Int = js.native,
      sourceStart: Int = js.native,
      sourceEnd: Int = js.native): Int = js.native

  /** Copies data from a region of buf to a region in target even if the target memory region overlaps with buf.
    * @param target      A Buffer to copy into.
    * @param targetStart The offset within target at which to begin copying to. Default: 0
    * @param sourceStart The offset within buf at which to begin copying from.
    *                    Ignored when targetStart is undefined. Default: 0
    * @param sourceEnd   The offset within buf at which to stop copying (not inclusive).
    *                    Ignored when sourceStart is undefined. Default: buf.length
    * @return The number of bytes copied.
    * @example {{{ buf.copy(target[, targetStart[, sourceStart[, sourceEnd]]]) }}}
    */
  def copy(
      target: Buffer,
      targetStart: Int = js.native,
      sourceStart: Int = js.native,
      sourceEnd: Int = js.native): Int = js.native

  /** Creates and returns an iterator of [index, byte] pairs from the Buffer contents.
    * @return an [[Iterator]]
    */
  def entries(): js.Iterator[js.Array[Int]] = js.native

  /** Returns true if both buf and otherBuffer have exactly the same bytes, false otherwise.
    * @param otherBuffer A Buffer to compare to
    * @return true if both buf and otherBuffer have exactly the same bytes, false otherwise.
    * @example buf.equals(otherBuffer)
    */
  def equals(otherBuffer: Buffer): Boolean = js.native

  /** Fills buf with the specified value. If the offset and end are not given, the entire buf will be filled.
    * This is meant to be a small simplification to allow the creation and filling of a Buffer to be done on a single line.
    * @param value    The value to fill buf with
    * @param offset   Where to start filling buf. Default: 0
    * @param end      Where to stop filling buf (not inclusive). Default: buf.length
    * @param encoding If value is a string, this is its encoding. Default: 'utf8'
    * @return A reference to buf
    * @example {{{ buf.fill(value[, offset[, end]][, encoding]) }}}
    */
  def fill(
      value: Buffer | Int | String,
      offset: Int = js.native,
      end: Int = js.native,
      encoding: String = js.native): this.type = js.native

  /** Returns the index of the first occurrence of value in buf or -1 if buf does not contain value
    * @param value      What to search for
    * @param byteOffset Where to begin searching in buf. Default: 0
    * @param encoding   If value is a string, this is its encoding. Default: 'utf8'
    * @return The index of the first occurrence of value in buf or -1 if buf does not contain value
    * @example {{{ buf.indexOf(value[, byteOffset][, encoding]) }}}
    */
  def indexOf(
      value: Buffer | Int | String,
      byteOffset: Int = js.native,
      encoding: String = js.native): Int = js.native

  /** Equivalent to buf.indexOf() !== -1.
    * @param value      What to search for
    * @param byteOffset Where to begin searching in buf. Default: 0
    * @param encoding   If value is a string, this is its encoding. Default: 'utf8'
    * @return true if value was found in buf, false otherwise
    * @example {{{ buf.includes(value[, byteOffset][, encoding]) }}}
    */
  def includes(
      value: Buffer | Int | String,
      byteOffset: Int = js.native,
      encoding: String = js.native): Boolean =
    js.native

  /** Creates and returns an iterator of buf keys (indices).
    * @return an [[Iterator]]
    * @example buf.keys()
    */
  def keys(): js.Iterator[Int] = js.native

  /** The largest size allowed for a single Buffer instance.
    * On 32-bit architectures, this value is (2^30)-1 (~1GB). On 64-bit architectures, this value is (2^31)-1 (~2GB).
    * Note that this is a property on the buffer module returned by require('buffer'), not on the Buffer global or
    * a Buffer instance.
    * @return the largest size allowed
    */
  def kMaxLength: Int = js.native

  /** Identical to buf.indexOf(), except buf is searched from back to front instead of front to back.
    * @param value      What to search for
    * @param byteOffset Where to begin searching in buf. Default: 0
    * @param encoding   If value is a string, this is its encoding. Default: 'utf8'
    * @return The index of the last occurrence of value in buf or -1 if buf does not contain value
    * @example {{{ buf.lastIndexOf(value[, byteOffset][, encoding]) }}}
    */
  def lastIndexOf(
      value: Buffer | Int | String,
      byteOffset: Int = js.native,
      encoding: String = js.native): Int =
    js.native

  /** Returns the amount of memory allocated for buf in bytes.
    * Note that this does not necessarily reflect the amount of "usable" data within buf.
    * @return the amount of memory allocated for buf in bytes.
    * @example buf.length
    */
  def length: Int = js.native

  /** Reads a 64-bit double from buf at the specified offset with specified endian format (readDoubleBE()
    * returns big endian, readDoubleLE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 8
    * @param noAssert Skip offset validation? Default: false
    * @return a [[Double]] value
    * @example {{{ buf.readDoubleBE(offset[, noAssert]) }}}
    */
  def readDoubleBE(offset: Int, noAssert: Boolean = js.native): Double =
    js.native

  /** Reads a 64-bit double from buf at the specified offset with specified endian format (readDoubleBE()
    * returns big endian, readDoubleLE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 8
    * @param noAssert Skip offset validation? Default: false
    * @return a [[Double]] value
    * @example {{{ buf.readDoubleBE(offset[, noAssert]) }}}
    */
  def readDoubleLE(offset: Int, noAssert: Boolean = js.native): Double =
    js.native

  /** Reads a 32-bit float from buf at the specified offset with specified endian format (readFloatBE()
    * returns big endian, readFloatLE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return a [[Double]] value
    * @example {{{ buf.readFloatBE(offset[, noAssert]) }}}
    */
  def readFloatBE(offset: Int, noAssert: Boolean = js.native): Double =
    js.native

  /** Reads a 32-bit float from buf at the specified offset with specified endian format (readFloatBE()
    * returns big endian, readFloatLE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return a [[Double]] value
    * @example {{{ buf.readFloatLE(offset[, noAssert]) }}}
    */
  def readFloatLE(offset: Int, noAssert: Boolean = js.native): Double =
    js.native

  /** Reads a signed 8-bit integer from buf at the specified offset.
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be considered undefined behavior.
    *
    * Integers read from a Buffer are interpreted as two's complement signed values.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 1
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readInt8(offset[, noAssert]) }}}
    */
  def readInt8(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads a signed 16-bit integer from buf at the specified offset with the specified endian format
    * (readInt16BE() returns big endian, readInt16LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    *
    * Integers read from a Buffer are interpreted as two's complement signed values.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readInt16BE(offset[, noAssert]) }}}
    */
  def readInt16BE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads a signed 16-bit integer from buf at the specified offset with the specified endian format
    * (readInt16BE() returns big endian, readInt16LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    *
    * Integers read from a Buffer are interpreted as two's complement signed values.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readInt16LE(offset[, noAssert]) }}}
    */
  def readInt16LE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads a signed 32-bit integer from buf at the specified offset with the specified endian format
    * (readInt32BE() returns big endian, readInt32LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    *
    * Integers read from a Buffer are interpreted as two's complement signed values.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readInt32BE(offset[, noAssert]) }}}
    */
  def readInt32BE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads a signed 32-bit integer from buf at the specified offset with the specified endian format
    * (readInt32BE() returns big endian, readInt32LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    *
    * Integers read from a Buffer are interpreted as two's complement signed values.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readInt32LE(offset[, noAssert]) }}}
    */
  def readInt32LE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads byteLength number of bytes from buf at the specified offset and interprets the result as a
    * two's complement signed value. Supports up to 48 bits of accuracy.
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset     Where to start reading. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to read. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip offset and byteLength validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readIntBE(offset, byteLength[, noAssert]) }}}
    */
  def readIntBE(
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Reads byteLength number of bytes from buf at the specified offset and interprets the result as a
    * two's complement signed value. Supports up to 48 bits of accuracy.
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset     Where to start reading. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to read. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip offset and byteLength validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readIntLE(offset, byteLength[, noAssert]) }}}
    */
  def readIntLE(
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Reads an unsigned 8-bit integer from buf at the specified offset.
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 1
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUInt8(offset[, noAssert]) }}}
    */
  def readUInt8(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads an unsigned 16-bit integer from buf at the specified offset with specified endian format
    * (readUInt16BE() returns big endian, readUInt16LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUInt16BE(offset[, noAssert]) }}}
    */
  def readUInt16BE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads an unsigned 16-bit integer from buf at the specified offset with specified endian format
    * (readUInt16BE() returns big endian, readUInt16LE() returns little endian).
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUInt16LE(offset[, noAssert]) }}}
    */
  def readUInt16LE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads an unsigned 32-bit integer from buf at the specified offset with specified endian format
    * (readUInt32BE() returns big endian, readUInt32LE() returns little endian).
    * *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUInt32BE(offset[, noAssert]) }}}
    */
  def readUInt32BE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads an unsigned 32-bit integer from buf at the specified offset with specified endian format
    * (readUInt32BE() returns big endian, readUInt32LE() returns little endian).
    * *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset   Where to start reading. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip offset validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUInt32LE(offset[, noAssert]) }}}
    */
  def readUInt32LE(offset: Int, noAssert: Boolean = js.native): Int = js.native

  /** Reads byteLength number of bytes from buf at the specified offset and interprets the result
    * as an unsigned integer. Supports up to 48 bits of accuracy.
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset     Where to start reading. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to read. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip offset and byteLength validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUIntBE(offset, byteLength[, noAssert]) }}}
    */
  def readUIntBE(
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Reads byteLength number of bytes from buf at the specified offset and interprets the result
    * as an unsigned integer. Supports up to 48 bits of accuracy.
    *
    * Setting noAssert to true allows offset to be beyond the end of buf, but the result should be
    * considered undefined behavior.
    * @param offset     Where to start reading. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to read. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip offset and byteLength validation? Default: false
    * @return the [[Int integer]] value
    * @example {{{ buf.readUIntLE(offset, byteLength[, noAssert]) }}}
    */
  def readUIntLE(
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Returns a new Buffer that references the same memory as the original, but offset and cropped by
    * the start and end indices.
    *
    * Note that modifying the new Buffer slice will modify the memory in the original Buffer because the
    * allocated memory of the two objects overlap.
    * @param start Where the new Buffer will start. Default: 0
    * @param end   Where the new Buffer will end (not inclusive). Default: buf.length
    * @return a [[Buffer]]
    * @example {{{ buf.slice([start[, end]]) }}}
    */
  def slice(start: Int = js.native, end: Int = js.native): this.type = js.native

  /** Interprets buf as an array of unsigned 16-bit integers and swaps the byte-order in-place.
    * Throws a RangeError if buf.length is not a multiple of 2.
    * @return A reference to buf
    * @example buf.swap16()
    */
  def swap16(): this.type = js.native

  /** Interprets buf as an array of unsigned 32-bit integers and swaps the byte-order in-place.
    * Throws a RangeError if buf.length is not a multiple of 4.
    * @return A reference to buf
    * @example buf.swap32()
    */
  def swap32(): this.type = js.native

  /** Interprets buf as an array of unsigned 64-bit numbers and swaps the byte-order in-place.
    * Throws a RangeError if buf.length is not a multiple of 8.
    * @return A reference to buf
    * @example buf.swap64()
    */
  def swap64(): this.type = js.native

  /** Returns a JSON representation of buf.
    * JSON.stringify() implicitly calls this function when stringifying a Buffer instance.
    * @return a JSON representation of buf.
    * @example buf.toJSON()
    */
  def toJSON(): String = js.native

  /** Decodes buf to a string according to the specified character encoding in encoding. start and end may be passed
    * to decode only a subset of buf.
    * @param encoding The character encoding to decode to. Default: 'utf8'
    * @param start    The byte offset to start decoding at. Default: 0
    * @param end      The byte offset to stop decoding at (not inclusive). Default: buf.length
    * @return a string according to the specified character encoding in encoding.
    * @example {{{ buf.toString([encoding[, start[, end]]]) }}}
    */
  def toString(
      encoding: String = js.native,
      start: Int = js.native,
      end: Int = js.native): String = js.native

  /** Re-encodes the given Buffer instance from one character encoding to another. Returns a new Buffer instance.
    * Throws if the fromEnc or toEnc specify invalid character encodings or if conversion from fromEnc to toEnc
    * is not permitted.
    * @param source  A Buffer instance
    * @param fromEnc The current encoding
    * @param toEnc   To target encoding
    * @return a new [[Buffer]]
    */
  def transcode(source: Buffer, fromEnc: String, toEnc: String): Buffer =
    js.native

  /** Creates and returns an iterator for buf values (bytes). This function is called automatically when a [[Buffer]]
    * is used in a for..of statement.
    * @return an iterator for buf values (bytes)
    * @example buf.values()
    */
  def values(): js.Iterator[Int] = js.native

  /** Writes string to buf at offset according to the character encoding in encoding. The length parameter is
    * the number of bytes to write. If buf did not contain enough space to fit the entire string, only a partial
    * amount of string will be written. However, partially encoded characters will not be written.
    * @param string   String to be written to buf
    * @param offset   Where to start writing string. Default: 0
    * @param length   How many bytes to write. Default: buf.length - offset
    * @param encoding The character encoding of string. Default: 'utf8'
    * @return the Number of bytes written
    * @example {{{ buf.write(string[, offset[, length]][, encoding]) }}}
    */
  def write(
      string: String,
      offset: Int = js.native,
      length: Int = js.native,
      encoding: String = js.native): Int =
    js.native

  /** Writes value to buf at the specified offset with specified endian format (writeDoubleBE() writes big endian,
    * writeDoubleLE() writes little endian). value should be a valid 64-bit double. Behavior is undefined when value
    * is anything other than a 64-bit double.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result should
    * be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 8
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeDoubleBE(value, offset[, noAssert]) }}}
    */
  def writeDoubleBE(
      value: Double,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeDoubleBE() writes big endian,
    * writeDoubleLE() writes little endian). value should be a valid 64-bit double. Behavior is undefined when value
    * is anything other than a 64-bit double.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 8
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeDoubleLE(value, offset[, noAssert]) }}}
    */
  def writeDoubleLE(
      value: Double,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeFloatBE() writes big endian,
    * writeFloatLE() writes little endian). value should be a valid 32-bit float. Behavior is undefined when value
    * is anything other than a 32-bit float.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeFloatBE(value, offset[, noAssert]) }}}
    */
  def writeFloatBE(
      value: Float,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeFloatBE() writes big endian,
    * writeFloatLE() writes little endian). value should be a valid 32-bit float. Behavior is undefined when value
    * is anything other than a 32-bit float.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeFloatLE(value, offset[, noAssert]) }}}
    */
  def writeFloatLE(
      value: Float,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset. value should be a valid signed 8-bit integer. Behavior is
    * undefined when value is anything other than a signed 8-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    *
    * value is interpreted and written as a two's complement signed integer.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 1
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeInt8(value, offset[, noAssert]) }}}
    */
  def writeInt8(value: Int, offset: Int, noAssert: Boolean = js.native): Int =
    js.native

  /** Writes value to buf at the specified offset with specified endian format (writeInt16BE() writes big endian,
    * writeInt16LE() writes little endian). value should be a valid signed 16-bit integer. Behavior is undefined
    * when value is anything other than a signed 16-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    *
    * value is interpreted and written as a two's complement signed integer.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeInt16BE(value, offset[, noAssert]) }}}
    */
  def writeInt16BE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeInt16BE() writes big endian,
    * writeInt16LE() writes little endian). value should be a valid signed 16-bit integer. Behavior is undefined
    * when value is anything other than a signed 16-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    *
    * value is interpreted and written as a two's complement signed integer.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 2
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeInt16BE(value, offset[, noAssert]) }}}
    */
  def writeInt16LE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeInt32BE() writes big endian,
    * writeInt32LE() writes little endian). value should be a valid signed 32-bit integer. Behavior is undefined
    * when value is anything other than a signed 32-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    *
    * value is interpreted and written as a two's complement signed integer.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeInt32BE(value, offset[, noAssert]) }}}
    */
  def writeInt32BE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeInt32BE() writes big endian,
    * writeInt32LE() writes little endian). value should be a valid signed 32-bit integer. Behavior is undefined
    * when value is anything other than a signed 32-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    *
    * value is interpreted and written as a two's complement signed integer.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeInt32BE(value, offset[, noAssert]) }}}
    */
  def writeInt32LE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes byteLength bytes of value to buf at the specified offset. Supports up to 48 bits of accuracy.
    * Behavior is undefined when value is anything other than a signed integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the
    * result should be considered undefined behavior.
    * @param value      Number to be written to buf
    * @param offset     Where to start writing. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to write. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip value, offset, and byteLength validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeIntBE(value, offset, byteLength[, noAssert]) }}}
    */
  def writeIntBE(
      value: Int,
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes byteLength bytes of value to buf at the specified offset. Supports up to 48 bits of accuracy.
    * Behavior is undefined when value is anything other than a signed integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the
    * result should be considered undefined behavior.
    * @param value      Number to be written to buf
    * @param offset     Where to start writing. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to write. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip value, offset, and byteLength validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeIntLE(value, offset, byteLength[, noAssert]) }}}
    */
  def writeIntLE(
      value: Int,
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset. value should be a valid unsigned 8-bit integer. Behavior
    * is undefined when value is anything other than an unsigned 8-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 1
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUInt8(value, offset[, noAssert]) }}}
    */
  def writeUInt8(value: Int, offset: Int, noAssert: Boolean = js.native): Int =
    js.native

  /** Writes value to buf at the specified offset with specified endian format (writeUInt16BE() writes big endian,
    * writeUInt16LE() writes little endian). value should be a valid unsigned 16-bit integer. Behavior is undefined
    * when value is anything other than an unsigned 16-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Number to be written to buf
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUInt16BE(value, offset[, noAssert]) }}}
    */
  def writeUInt16BE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeUInt16BE() writes big endian,
    * writeUInt16LE() writes little endian). value should be a valid unsigned 16-bit integer. Behavior is undefined
    * when value is anything other than an unsigned 16-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Number to be written to buf
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUInt16BE(value, offset[, noAssert]) }}}
    */
  def writeUInt16LE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeUInt32BE() writes big endian,
    * writeUInt32LE() writes little endian). value should be a valid unsigned 32-bit integer. Behavior is undefined
    * when value is anything other than an unsigned 32-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUInt32BE(value, offset[, noAssert]) }}}
    */
  def writeUInt32BE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes value to buf at the specified offset with specified endian format (writeUInt32BE() writes big endian,
    * writeUInt32LE() writes little endian). value should be a valid unsigned 32-bit integer. Behavior is undefined
    * when value is anything other than an unsigned 32-bit integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the result
    * should be considered undefined behavior.
    * @param value    Number to be written to buf
    * @param offset   Where to start writing. Must satisfy: 0 <= offset <= buf.length - 4
    * @param noAssert Skip value and offset validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUInt32LE(value, offset[, noAssert]) }}}
    */
  def writeUInt32LE(
      value: Int,
      offset: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes byteLength bytes of value to buf at the specified offset. Supports up to 48 bits of accuracy.
    * Behavior is undefined when value is anything other than an unsigned integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the
    * result should be considered undefined behavior.
    * @param value      Number to be written to buf
    * @param offset     Where to start writing. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to write. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip value, offset, and byteLength validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUIntBE(value, offset, byteLength[, noAssert]) }}}
    */
  def writeUIntBE(
      value: Int,
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

  /** Writes byteLength bytes of value to buf at the specified offset. Supports up to 48 bits of accuracy.
    * Behavior is undefined when value is anything other than an unsigned integer.
    *
    * Setting noAssert to true allows the encoded form of value to extend beyond the end of buf, but the
    * result should be considered undefined behavior.
    * @param value      Number to be written to buf
    * @param offset     Where to start writing. Must satisfy: 0 <= offset <= buf.length - byteLength
    * @param byteLength How many bytes to write. Must satisfy: 0 < byteLength <= 6
    * @param noAssert   Skip value, offset, and byteLength validation? Default: false
    * @return the offset plus the number of bytes written
    * @example {{{ buf.writeUIntLE(value, offset, byteLength[, noAssert]) }}}
    */
  def writeUIntLE(
      value: Int,
      offset: Int,
      byteLength: Int,
      noAssert: Boolean = js.native): Int = js.native

}

@js.native
@JSGlobal
object Buffer extends js.Object {

  /////////////////////////////////////////////////////////////////////////////////
  //      Properties
  /////////////////////////////////////////////////////////////////////////////////

  /** Returns the maximum number of bytes that will be returned when buf.inspect() is called.
    * This can be overridden by user modules. See util.inspect() for more details on buf.inspect() behavior.
    *
    * Note that this is a property on the buffer module returned by require('buffer'), not on the
    * Buffer global or a Buffer instance.
    */
  val INSPECT_MAX_BYTES: Int = js.native

  /** On 32-bit architectures, this value is (2^30)-1 (~1GB). On 64-bit architectures, this value is (2^31)-1 (~2GB).F
    * Note that this is a property on the buffer module returned by require('buffer'), not on the Buffer global or a Buffer instance.
    */
  val kMaxLength: Int = js.native

  /** This is the number of bytes used to determine the size of pre-allocated, internal Buffer instances used for pooling.
    * This value may be modified.
    */
  var poolSize: Int = js.native

  /////////////////////////////////////////////////////////////////////////////////
  //      Methods
  /////////////////////////////////////////////////////////////////////////////////

  /** Allocates a new Buffer of size bytes. If fill is undefined, the Buffer will be zero-filled.
    * @param size     The desired length of the new Buffer
    * @param fill     A value to pre-fill the new Buffer with. Default: 0
    * @param encoding If fill is a string, this is its encoding. Default: 'utf8'
    * @return a new [[Buffer]]
    * @example {{{ Buffer.alloc(size[, fill[, encoding]]) }}}
    */
  def alloc(
      size: Int,
      fill: Buffer | Int | String = js.native,
      encoding: String = js.native): Buffer = js.native

  /** Calling Buffer.alloc(size) can be significantly slower than the alternative Buffer.allocUnsafe(size) but ensures
    * that the newly created Buffer instance contents will never contain sensitive data.
    * @param size the allocated size.
    * @example Buffer.allocUnsafe(size)
    */
  def allocUnsafe(size: Int): Buffer = js.native

  /** Allocates a new non-zero-filled and non-pooled Buffer of size bytes. The size must be less than or equal to the
    * value of require('buffer').kMaxLength (on 64-bit architectures, kMaxLength is {{{ (2^31)-1). }}} Otherwise, a RangeError
    * is thrown. A zero-length Buffer will be created if a size less than or equal to 0 is specified.
    *
    * The underlying memory for Buffer instances created in this way is not initialized. The contents of the newly
    * created Buffer are unknown and may contain sensitive data. Use buf.fill(0) to initialize such Buffer instances to zeroes.
    *
    * When using Buffer.allocUnsafe() to allocate new Buffer instances, allocations under 4KB are, by default, sliced
    * from a single pre-allocated Buffer. This allows applications to avoid the garbage collection overhead of creating
    * many individually allocated Buffers. This approach improves both performance and memory usage by eliminating the
    * need to track and cleanup as many Persistent objects.
    *
    * However, in the case where a developer may need to retain a small chunk of memory from a pool for an indeterminate
    * amount of time, it may be appropriate to create an un-pooled Buffer instance using Buffer.allocUnsafeSlow() then
    * copy out the relevant bits.
    * @param size the allocated size.
    * @example Buffer.allocUnsafeSlow(size)
    */
  def allocUnsafeSlow(size: Int): Buffer = js.native

  /** Returns the actual byte length of a string. This is not the same as String.prototype.length since that returns
    * the number of characters in a string.
    * @param string   <String> | <Buffer> | <TypedArray> | <DataView> | <ArrayBuffer>
    * @param encoding the optional encoding (default "utf8")
    * @example Buffer.byteLength(string[, encoding])
    */
  def byteLength(string: js.Any, encoding: String = "utf8"): Int = js.native

  /** Compares buf1 to buf2 typically for the purpose of sorting arrays of Buffers.
    * This is equivalent is calling buf1.compare(buf2).
    */
  def compare(buf1: Buffer, buf2: Buffer): Int = js.native

  /** Returns a new Buffer which is the result of concatenating all the Buffers in the list together.
    * If the list has no items, or if the totalLength is 0, then a new zero-length Buffer is returned.
    * If totalLength is not provided, it is calculated from the Buffers in the list. This, however, adds an additional
    * loop to the function, so it is faster to provide the length explicitly.
    * @param list        the list of Buffer objects to concat
    * @param totalLength the optional total length
    * @example Buffer.concat(list[, totalLength])
    */
  def concat(list: js.Array[Buffer], totalLength: Int): Buffer = js.native

  /** Returns a new Buffer which is the result of concatenating all the Buffers in the list together.
    * If the list has no items, or if the totalLength is 0, then a new zero-length Buffer is returned.
    * If totalLength is not provided, it is calculated from the Buffers in the list. This, however, adds an additional
    * loop to the function, so it is faster to provide the length explicitly.
    * @param list the list of Buffer objects to concat
    * @example Buffer.concat(list[, totalLength])
    */
  def concat(list: js.Array[Buffer]): Buffer = js.native

  /** When passed a reference to the .buffer property of a TypedArray instance, the newly created Buffer
    * will share the same allocated memory as the TypedArray.
    * @example {{{ Buffer.from(arrayBuffer[, byteOffset[, length]]) }}}
    */
  def from(arrayBuffer: ArrayBuffer, byteOffset: Int, length: Int): Buffer =
    js.native

  /** When passed a reference to the .buffer property of a TypedArray instance, the newly created Buffer
    * will share the same allocated memory as the TypedArray.
    * @example {{{ Buffer.from(arrayBuffer[, byteOffset[, length]]) }}}
    */
  def from(arrayBuffer: ArrayBuffer, byteOffset: Int): Buffer = js.native

  /** When passed a reference to the .buffer property of a TypedArray instance, the newly created Buffer
    * will share the same allocated memory as the TypedArray.
    * @example {{{ Buffer.from(arrayBuffer[, byteOffset[, length]]) }}}
    */
  def from(arrayBuffer: ArrayBuffer): Buffer = js.native

  /** Allocates a new Buffer using an array of octets.
    * @example Buffer.from(array)
    */
  def from(array: js.Array[Int]): Buffer = js.native

  /** Creates a new Buffer containing the given JavaScript string str. If provided, the encoding parameter identifies
    * the strings character encoding.
    * @param str      the source string
    * @param encoding the given encoding
    * @return a new Buffer
    */
  def from(str: String, encoding: String = js.native): Buffer = js.native

  /** Returns true if obj is a Buffer, false otherwise.
    * @param obj the given object
    * @return true if obj is a Buffer, false otherwise.
    */
  def isBuffer(obj: js.Any): Boolean = js.native

  /** Returns true if encoding contains a supported character encoding, or false otherwise.
    * @param encoding A character encoding name to check
    * @return true if encoding contains a supported character encoding, or false otherwise.
    */
  def isEncoding(encoding: String): Boolean = js.native

  /** Re-encodes the given Buffer instance from one character encoding to another. Returns a new Buffer instance.
    *
    * Throws if the fromEnc or toEnc specify invalid character encodings or if conversion from fromEnc to toEnc
    * is not permitted.
    *
    * The transcoding process will use substitution characters if a given byte sequence cannot be adequately
    * represented in the target encoding.
    * @param source  A Buffer instance
    * @param fromEnc The current encoding
    * @param toEnc   To target encoding
    * @return a new Buffer instance.
    */
  def transcode(source: Buffer, fromEnc: String, toEnc: String): Buffer =
    js.native

}
