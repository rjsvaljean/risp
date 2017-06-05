package rxvl.ntt

import shapeless.nat.{ _16, _2, _3, _8 }
import scalaz.syntax.functor._
import scalaz.syntax.zip._

object LogicalGates {

  def NAnd(in1: Boolean, in2: Boolean): Boolean = !(in1 && in2)

  def Not(in: Boolean): Boolean = NAnd(in, in)

  def And(in1: Boolean, in2: Boolean): Boolean = !NAnd(in1, in2)

  def Or(in1: Boolean, in2: Boolean): Boolean = NAnd(Not(in1), Not(in2))

  def NOr(in1: Boolean, in2: Boolean): Boolean = Not(Or(in1, in2))

  def XOr(in1: Boolean, in2: Boolean): Boolean =
    Or(And(in1, Not(in2)), And(Not(in1), in2))

  def Mux(sel: Boolean, a: Boolean, b: Boolean): Boolean =
    Or(And(sel, b), And(Not(sel), a))

  def DMux(sel: Boolean, in: Boolean): (Boolean, Boolean) =
    (And(Not(sel), in), And(sel, in))

  def Not16(in: Word[_16, Boolean]): Word[_16, Boolean] = in.map(Not)

  def And16(a: Word[_16, Boolean], b: Word[_16, Boolean]): Word[_16, Boolean] =
    a.fzip(b).map((And _).tupled)

  def Or16(a: Word[_16, Boolean], b: Word[_16, Boolean]): Word[_16, Boolean] =
    a.fzip(b).map((Or _).tupled)

  def Mux16(sel: Boolean, a: Word[_16, Boolean], b: Word[_16, Boolean]): Word[_16, Boolean] =
    a.fzip(b).map { case (_a, _b) => Mux(sel, _a, _b) }

  def Or8Way(in: Word[_8, Boolean]): Boolean = in.bits.reduce(Or)

  def Mux4Way16(
    sel: Word[_2, Boolean],
    in1: Word[_16, Boolean],
    in2: Word[_16, Boolean],
    in3: Word[_16, Boolean],
    in4: Word[_16, Boolean]
  ): Word[_16, Boolean] = {
    val Vector(sel1, sel2) = sel.bits
    Word[_16](b = And(sel1, sel2))
    Mux16(sel1, Mux16(sel2, in1, in2), Mux16(sel2, in3, in4))
  }

  def Mux8Way16(
    sel: Word[_3, Boolean],
    in1: Word[_16, Boolean],
    in2: Word[_16, Boolean],
    in3: Word[_16, Boolean],
    in4: Word[_16, Boolean],
    in5: Word[_16, Boolean],
    in6: Word[_16, Boolean],
    in7: Word[_16, Boolean],
    in8: Word[_16, Boolean]
  ): Word[_16, Boolean] = {
    val Vector(sel1, sel2, sel3) = sel.bits
    val sel23 = Word[_2, Boolean](Vector(sel2, sel3))
    Mux16(
      sel1,
      Mux4Way16(sel23, in1, in2, in3, in4),
      Mux4Way16(sel23, in5, in6, in7, in8)
    )
  }

  def DMux4Way(
    sel: Word[_2, Boolean],
    in: Boolean
  ): (Boolean, Boolean, Boolean, Boolean) = {
    val Vector(sel1, sel2) = sel.bits
    (
      And(And(Not(sel1), Not(sel2)), in),
      And(And(sel1, Not(sel2)), in),
      And(And(Not(sel1), Not(sel2)), in),
      And(And(sel1, Not(sel2)), in)
    )
  }

  def DMux8Way(
    sel: Word[_3, Boolean],
    in: Boolean
  ): (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) = {
    val Vector(sel1, sel2, sel3) = sel.bits
    (
      And(And(Not(sel1), And(Not(sel2), Not(sel3))), in),
      And(And(Not(sel1), And(Not(sel2), sel3)), in),
      And(And(Not(sel1), And(sel2, Not(sel3))), in),
      And(And(Not(sel1), And(sel2, sel3)), in),
      And(And(sel1, And(Not(sel2), Not(sel3))), in),
      And(And(sel1, And(Not(sel2), sel3)), in),
      And(And(sel1, And(sel2, Not(sel3))), in),
      And(And(sel1, And(sel2, sel3)), in)
    )
  }

}

