package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =
    if isZero(right) then left
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =

    @tailrec
    def multiplicationReq(left: Number, right: Number, acc: Number): Number =
      if isZero(right) then acc
      else if isNonNegative(right) then multiplicationReq(left, decrement(right), addition(acc, left))
      else multiplicationReq(left, increment(right), addition(acc, negative(left)))

    def negative(value: Number): Number =
      @tailrec
      def negativeReq(value: Number, acc: Number): Number =
        if isZero(value) then acc
        else if isNonNegative(value) then negativeReq(decrement(value), decrement(acc))
        else negativeReq(increment(value), increment(acc))
      negativeReq(value, acc = 0)

    multiplicationReq(left, right, acc = 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerReq(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else powerReq(base, decrement(p), multiplication(acc, base))

    powerReq(base, p, acc = 1)
