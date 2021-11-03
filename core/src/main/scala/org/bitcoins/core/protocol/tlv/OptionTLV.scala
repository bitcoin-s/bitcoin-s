package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

sealed abstract class OptionTLV[+A <: NetworkElement]
    extends Product
    with Serializable
    with NetworkElement {
  self =>

  /** Returns true if the option is $none, false otherwise.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(_) => false
    *   case None    => true
    * }
    * }}}
    */
  final def isEmpty: Boolean = this eq None

  /** Returns true if the option is an instance of $some, false otherwise.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(_) => true
    *   case None    => false
    * }
    * }}}
    */
  final def isDefined: Boolean = !isEmpty

  final def knownSize: Int = if (isEmpty) 0 else 1

  /** Returns the option's value.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => x
    *   case None    => throw new Exception
    * }
    * }}}
    *  @note The option must be nonempty.
    *  @throws NoSuchElementException if the option is empty.
    */
  def get: A

  /** Returns the option's value if the option is nonempty, otherwise
    * return the result of evaluating `default`.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => x
    *   case None    => default
    * }
    * }}}
    *
    *  @param default  the default expression.
    */
  @inline final def getOrElse[B >: A <: NetworkElement](default: => B): B =
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
    * or `null` if it is empty.
    *
    * Although the use of null is discouraged, code written to use
    * $option must often interface with code that expects and returns nulls.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => x
    *   case None    => null
    * }
    * }}}
    * @example {{{
    * val initialText: Option[String] = getInitialText
    * val textField = new JComponent(initialText.orNull,20)
    * }}}
    */
  @inline final def orNull[A1 >: A <: NetworkElement](implicit
      ev: Null <:< A1): A1 =
    this getOrElse ev(null)

  /** Returns a $some containing the result of applying $f to this $option's
    * value if this $option is nonempty.
    * Otherwise return $none.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => Some(f(x))
    *   case None    => None
    * }
    * }}}
    *  @note This is similar to `flatMap` except here,
    *  $f does not need to wrap its result in an $option.
    *
    *  @param  f   the function to apply
    *  @see flatMap
    *  @see foreach
    */
  @inline final def map[B <: NetworkElement](f: A => B): OptionTLV[B] =
    if (isEmpty) NoneTLV else SomeTLV(f(this.get))

  /** Returns the result of applying $f to this $option's
    *  value if the $option is nonempty.  Otherwise, evaluates
    *  expression `ifEmpty`.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => f(x)
    *   case None    => ifEmpty
    * }
    * }}}
    * This is also equivalent to:
    * {{{
    * option map f getOrElse ifEmpty
    * }}}
    *  @param  ifEmpty the expression to evaluate if empty.
    *  @param  f       the function to apply if nonempty.
    */
  @inline final def fold[B <: NetworkElement](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.get)

  /** Returns the result of applying $f to this $option's value if
    * this $option is nonempty.
    * Returns $none if this $option is empty.
    * Slightly different from `map` in that $f is expected to
    * return an $option (which could be $none).
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => f(x)
    *   case None    => None
    * }
    * }}}
    *  @param  f   the function to apply
    *  @see map
    *  @see foreach
    */
  @inline final def flatMap[B <: NetworkElement](
      f: A => OptionTLV[B]): OptionTLV[B] =
    if (isEmpty) NoneTLV else f(this.get)

  /** Returns the nested $option value if it is nonempty.  Otherwise,
    * return $none.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(Some(b)) => Some(b)
    *   case _             => None
    * }
    * }}}
    * @example {{{
    * Some(Some("something")).flatten
    * }}}
    *
    * @param ev an implicit conversion that asserts that the value is
    *           also an $option.
    * @see flatMap
    */
  def flatten[B <: NetworkElement](implicit
      ev: A <:< OptionTLV[B]): OptionTLV[B] =
    if (isEmpty) NoneTLV else ev(this.get)

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
    * this $option's value returns true. Otherwise, return $none.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) if p(x) => Some(x)
    *   case _               => None
    * }
    * }}}
    *  @param  p   the predicate used for testing.
    */
  @inline final def filter(p: A => Boolean): OptionTLV[A] =
    if (isEmpty || p(this.get)) this else NoneTLV

  /** Returns this $option if it is nonempty '''and''' applying the predicate $p to
    * this $option's value returns false. Otherwise, return $none.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) if !p(x) => Some(x)
    *   case _                => None
    * }
    * }}}
    *  @param  p   the predicate used for testing.
    */
  @inline final def filterNot(p: A => Boolean): OptionTLV[A] =
    if (isEmpty || !p(this.get)) this else NoneTLV

  /** Returns false if the option is $none, true otherwise.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(_) => true
    *   case None    => false
    * }
    * }}}
    *  @note   Implemented here to avoid the implicit conversion to Iterable.
    */
  final def nonEmpty: Boolean = isDefined

  /** Necessary to keep $option from being implicitly converted to
    *  [[scala.collection.Iterable]] in `for` comprehensions.
    */
  @inline final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
    *  collection" contract even though it seems unlikely to matter much in a
    *  collection with max size 1.
    */
  class WithFilter(p: A => Boolean) {
    def map[B <: NetworkElement](f: A => B): OptionTLV[B] = self filter p map f

    def flatMap[B <: NetworkElement](f: A => OptionTLV[B]): OptionTLV[B] =
      self filter p flatMap f
    def foreach[U <: NetworkElement](f: A => U): Unit = self filter p foreach f

    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x =>
      p(x) && q(x))
  }

  /** Tests whether the option contains a given value as an element.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => x == elem
    *   case None    => false
    * }
    * }}}
    *  @example {{{
    *  // Returns true because Some instance contains string "something" which equals "something".
    *  Some("something") contains "something"
    *
    *  // Returns false because "something" != "anything".
    *  Some("something") contains "anything"
    *
    *  // Returns false when method called on None.
    *  None contains "anything"
    *  }}}
    *
    *  @param elem the element to test.
    *  @return `true` if the option has an element that is equal (as
    *  determined by `==`) to `elem`, `false` otherwise.
    */
  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && this.get == elem

  /** Returns true if this option is nonempty '''and''' the predicate
    * $p returns true when applied to this $option's value.
    * Otherwise, returns false.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => p(x)
    *   case None    => false
    * }
    * }}}
    *  @param  p   the predicate to test
    */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Returns true if this option is empty '''or''' the predicate
    * $p returns true when applied to this $option's value.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => p(x)
    *   case None    => true
    * }
    * }}}
    *  @param  p   the predicate to test
    */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Apply the given procedure $f to the option's value,
    *  if it is nonempty. Otherwise, do nothing.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => f(x)
    *   case None    => ()
    * }
    * }}}
    *  @param  f   the procedure to apply.
    *  @see map
    *  @see flatMap
    */
  @inline final def foreach[U <: NetworkElement](f: A => U): Unit = {
    if (!isEmpty) {
      f(this.get)
      ()
    } else ()
  }

  /** Returns a $some containing the result of
    * applying `pf` to this $option's contained
    * value, '''if''' this option is
    * nonempty '''and''' `pf` is defined for that value.
    * Returns $none otherwise.
    *
    *  @example {{{
    *  // Returns Some(HTTP) because the partial function covers the case.
    *  Some("http") collect {case "http" => "HTTP"}
    *
    *  // Returns None because the partial function doesn't cover the case.
    *  Some("ftp") collect {case "http" => "HTTP"}
    *
    *  // Returns None because the option is empty. There is no value to pass to the partial function.
    *  None collect {case value => value}
    *  }}}
    *
    *  @param  pf   the partial function.
    *  @return the result of applying `pf` to this $option's
    *  value (if possible), or $none.
    */
  @inline final def collect[B <: NetworkElement](
      pf: PartialFunction[A, B]): Option[B] =
    if (!isEmpty) pf.lift(this.get) else None

  /** Returns this $option if it is nonempty,
    *  otherwise return the result of evaluating `alternative`.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => Some(x)
    *   case None    => alternative
    * }
    * }}}
    *  @param alternative the alternative expression.
    */
  @inline final def orElse[B >: A <: NetworkElement](
      alternative: => OptionTLV[B]): OptionTLV[B] =
    if (isEmpty) alternative else this

  /** Converts an Option of a pair into an Option of the first element and an Option of the second element.
    *
    *  This is equivalent to:
    *  {{{
    *  option match {
    *    case Some((x, y)) => (Some(x), Some(y))
    *    case _            => (None,    None)
    *  }
    *  }}}
    *  @tparam A1    the type of the first half of the element pair
    *  @tparam A2    the type of the second half of the element pair
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this Option is a pair.
    *  @return       a pair of Options, containing, respectively, the first and second half
    *                of the element pair of this Option.
    */
  final def unzip[A1 <: NetworkElement, A2 <: NetworkElement](implicit
      asPair: A <:< (A1, A2)): (OptionTLV[A1], OptionTLV[A2]) = {
    if (isEmpty)
      (NoneTLV, NoneTLV)
    else {
      val e = asPair(this.get)
      (SomeTLV(e._1), SomeTLV(e._2))
    }
  }

  /** Converts an Option of a triple into three Options, one containing the element from each position of the triple.
    *
    *  This is equivalent to:
    *  {{{
    *  option match {
    *    case Some((x, y, z)) => (Some(x), Some(y), Some(z))
    *    case _               => (None,    None,    None)
    *  }
    *  }}}
    *  @tparam A1      the type of the first of three elements in the triple
    *  @tparam A2      the type of the second of three elements in the triple
    *  @tparam A3      the type of the third of three elements in the triple
    *  @param asTriple an implicit conversion which asserts that the element type
    *                  of this Option is a triple.
    *  @return         a triple of Options, containing, respectively, the first, second, and third
    *                  elements from the element triple of this Option.
    */
  final def unzip3[
      A1 <: NetworkElement,
      A2 <: NetworkElement,
      A3 <: NetworkElement](implicit asTriple: A <:< (A1, A2, A3)): (
      OptionTLV[A1],
      OptionTLV[A2],
      OptionTLV[A3]) = {
    if (isEmpty)
      (NoneTLV, NoneTLV, NoneTLV)
    else {
      val e = asTriple(this.get)
      (SomeTLV(e._1), SomeTLV(e._2), SomeTLV(e._3))
    }
  }

  /** Returns a singleton iterator returning the $option's value
    * if it is nonempty, or an empty iterator if the option is empty.
    */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty
    else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
    * if it is nonempty, or the empty list if the $option is empty.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => List(x)
    *   case None    => Nil
    * }
    * }}}
    */
  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  /** Returns a [[scala.util.Left]] containing the given
    * argument `left` if this $option is empty, or
    * a [[scala.util.Right]] containing this $option's value if
    * this is nonempty.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => Right(x)
    *   case None    => Left(left)
    * }
    * }}}
    * @param left the expression to evaluate and return if this is empty
    * @see toLeft
    */
  @inline final def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
    * argument `right` if this is empty, or
    * a [[scala.util.Left]] containing this $option's value
    * if this $option is nonempty.
    *
    * This is equivalent to:
    * {{{
    * option match {
    *   case Some(x) => Left(x)
    *   case None    => Right(right)
    * }
    * }}}
    * @param right the expression to evaluate and return if this is empty
    * @see toRight
    */
  @inline final def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(this.get)

  def toOption: Option[A] = this match {
    case SomeTLV(t) => Some(t)
    case NoneTLV    => None
  }
}

final case class SomeTLV[+A <: NetworkElement](value: A) extends OptionTLV[A] {
  def get: A = value

  override val bytes: ByteVector = ByteVector.fromByte(1) ++ value.bytes
}

case object NoneTLV extends OptionTLV[Nothing] {
  def get: Nothing = throw new NoSuchElementException("None.get")

  override val bytes: ByteVector = ByteVector.fromByte(0)
}

object OptionTLV {

  def apply[T <: NetworkElement](opt: Option[T]): OptionTLV[T] = {
    opt match {
      case Some(t) => SomeTLV(t)
      case None    => NoneTLV
    }
  }
}
