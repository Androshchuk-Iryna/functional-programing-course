package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = this > that || this == that

    @targetName("less than")
    infix def <(that: Numeral): Boolean = that > this

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = that >= this

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = throw new IllegalStateException("Zero has no predecessor")

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      if that.isZero then Zero
      else throw new IllegalArgumentException("Cannot subtract a larger number from Zero")

    def toInt: Int = 0

    override def toString: String = toInt.toString

  override def equals(obj: Any): Boolean = obj match
    case Zero => true
    case _    => false

  override def hashCode(): Int = 0

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    infix def >(that: Numeral): Boolean =
      if that.isZero then true
      else predecessor >= that.predecessor

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that match
      case Zero         => this
      case s: Successor => (predecessor + s).successor

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      if that.isZero then this
      else predecessor - that.predecessor

    def toInt: Int = 1 + n.toInt

    override def toString: String = toInt.toString

    override def equals(obj: Any): Boolean = obj match
      case s: Successor => this.predecessor == s.predecessor
      case _            => false

    override def hashCode(): Int = 31 * predecessor.hashCode() + 1
