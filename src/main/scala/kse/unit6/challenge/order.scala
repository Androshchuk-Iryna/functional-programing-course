package kse.unit6.challenge

import kse.unit4.challenge.numerals.{Numeral, Successor, Zero}
import scala.annotation.targetName

object order:

  trait Order[T]:
    def compare(left: T, right: T): Int

  given Order[Int] with

    def compare(left: Int, right: Int): Int =
      if left < right then -1
      else if left > right then 1
      else 0

  given Order[Numeral] with

    private def toInt(numeral: Numeral): Int = numeral match
      case Zero         => 0
      case Successor(n) => 1 + toInt(n)

    def compare(left: Numeral, right: Numeral): Int =
      val leftInt  = toInt(left)
      val rightInt = toInt(right)
      summon[Order[Int]].compare(leftInt, rightInt)

  object Order:
    def apply[T](using ord: Order[T]): Order[T] = ord

  extension [V: Order as ord](elem: V)

    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

    infix def <(that: V): Boolean =
      ord.compare(elem, that) < 0

    infix def >=(that: V): Boolean =
      ord.compare(elem, that) >= 0

    infix def <=(that: V): Boolean =
      ord.compare(elem, that) <= 0

end order
