package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero is zero") = Zero.isZero

  property("successor of any number is not zero") = forAll: (n: Numeral) =>
    !n.successor.isZero

  property("predecessor of successor returns the original number") = forAll: (n: Numeral) =>
    n.successor.predecessor == n

  property("zero is less than any non-zero natural number") = forAll: (n: Numeral) =>
    if !n.isZero then Zero < n else true

  property("less than and greater than are inverse relations") = forAll: (a: Numeral, b: Numeral) =>
    (a < b) == (b > a)

  property("less-equal and greater-equal are inverse relations") = forAll: (a: Numeral, b: Numeral) =>
    (a <= b) == (b >= a)

  property("zero and addition") = forAll: (n: Numeral) =>
    (Zero + n == n) && (n + Zero == n)

  property("addition is commutative") = forAll: (a: Numeral, b: Numeral) =>
    a + b == b + a

  property("addition is associative") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    (a + b) + c == a + (b + c)

  property("subtracting zero leaves a number unchanged") = forAll: (n: Numeral) =>
    n - Zero == n

  property("subtracting a number from itself equals zero") = forAll: (n: Numeral) =>
    n - n == Zero

  property("toInt of successor equals original toInt plus one") = forAll: (n: Numeral) =>
    n.successor.toInt == n.toInt + 1

  property("toInt preserves addition (toInt(a+b) = toInt(a) + toInt(b))") = forAll: (a: Numeral, b: Numeral) =>
    (a + b).toInt == a.toInt + b.toInt

  property("every number equals itself") = forAll: (n: Numeral) =>
    n == n

  property("symmetric equals") = forAll: (a: Numeral, b: Numeral) =>
    (a == b) == (b == a)

  property("transitive equals") = forAll: (a: Numeral, b: Numeral, c: Numeral) =>
    (!((a == b) && (b == c))) || (a == c)

  property("subtraction with addition") = forAll: (a: Numeral, b: Numeral) =>
    (b > a) || ((a + b) - a == b)

  property("addition with successor") = forAll: (a: Numeral, b: Numeral) =>
    a + b.successor == (a + b).successor

  property("addition increases value") = forAll: (a: Numeral, b: Numeral) =>
    b.isZero || a < (a + b)

  property("toInt.non.negative") = forAll: (n: Numeral) =>
    n.toInt >= 0

end NumeralsSpecification
