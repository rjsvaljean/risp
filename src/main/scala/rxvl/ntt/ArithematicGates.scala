package rxvl.ntt

import LogicalGates._
import shapeless.nat._
import scalaz.syntax.zip._
import scalaz.syntax.foldable._

import BinHelpers.toBin

object ArithematicGates {

  type Sum = Boolean
  type Carry = Boolean

  def HalfAdder(
    a: Boolean,
    b: Boolean
  ): (Carry, Sum) = (And(a, b), XOr(a, b))

  def FullAdder(
    a: Boolean,
    b: Boolean,
    c: Boolean
  ): (Carry, Sum) = {
    val (carry1, sum1) = HalfAdder(a, b)
    val (carry2, sum2) = HalfAdder(c, sum1)
    (Or(carry2, carry1), sum2)
  }

  def Add16(
    a: Word[_16, Boolean],
    b: Word[_16, Boolean]
  ): Word[_16, Boolean] = {
    Word[_16, Boolean](a.fzip(b).foldRight((Vector[Boolean](), false)) {
      case ((_a, _b), (acc, carry)) =>
        val (newCarry, newSum) = FullAdder(_a, _b, carry)
        (newSum +: acc, newCarry)
    }._1)
  }

  def Inc16(in: Word[_16, Boolean]): Word[_16, Boolean] =
    Add16(in, toBin(1))

}
