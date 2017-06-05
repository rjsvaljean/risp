package rxvl.ntt

import shapeless.Nat
import shapeless.nat.{ _16, _2, _3, _8 }
import shapeless.ops.nat.ToInt

import scalaz._
import Scalaz._
import scala.collection.immutable.Seq

case class Word[Size <: Nat: ToInt, A](bits: Vector[A]) {
  def apply(i: Int): A = bits(i)
  val size: Int = ToInt[Size].apply()
  require(size == bits.size)

  override def toString: String = bits
    .map { case true => "1"; case _ => "0" }
    .mkString
}

object Word {
  def apply[Size <: Nat: ToInt](b: Boolean): Word[Size, Boolean] =
    Word[Size, Boolean](Vector.fill(ToInt[Size].apply())(b))

  implicit def functor[Size <: Nat: ToInt]: Functor[({ type l[a] = Word[Size, a] })#l] =
    new Functor[({ type l[a] = Word[Size, a] })#l] {
      def map[A, B](
        fa: Word[Size, A]
      )(
        f: (A) => B
      ): Word[Size, B] = Word[Size, B](fa.bits.map(f))
    }

  implicit def zip[Size <: Nat: ToInt]: Zip[({ type l[a] = Word[Size, a] })#l] =
    new Zip[({ type l[a] = Word[Size, a] })#l] {
      def zip[A, B](
        a: => Word[Size, A],
        b: => Word[Size, B]
      ): Word[Size, (A, B)] = Word[Size, (A, B)](a.bits.zip(b.bits))
    }

  implicit def unzip[Size <: Nat: ToInt]: Unzip[({ type l[a] = Word[Size, a] })#l] =
    new Unzip[({ type l[a] = Word[Size, a] })#l] {
      def unzip[A, B](a: Word[Size, (A, B)]) = {
        val (as, bs) = a.bits.unzip
        (Word[Size, A](as), Word[Size, B](bs))
      }
    }

}

object Gates {

  implicit def foldable[Size <: Nat: ToInt]: Foldable[({ type l[a] = Word[Size, a] })#l] =
    new Foldable[({ type l[a] = Word[Size, a] })#l] {
      def foldMap[A, B](
        fa: Word[Size, A]
      )(
        f: (A) => B
      )(
        implicit
        F: Monoid[B]
      ): B = fa.bits.foldMap(f)

      def foldRight[A, B](
        fa: Word[Size, A],
        z: => B
      )(
        f: (A, => B) => B
      ): B = fa.bits.foldRight(z)((a, b) => f(a, b))
    }

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

object RunGates {

  import shapeless.ops.product.ToHList
  import shapeless.{ ::, HNil }
  import Gates._
  import GateHelpers._

  def show(dat: List[(List[Boolean], List[Boolean])]): Unit = {
    dat.map {
      case (ins, outs) =>
        val toBin: Boolean => Int = { case true => 1; case false => 0 }
        s"${ins.map(toBin).mkString(" | ")} -> ${outs.map(toBin).mkString(" | ")}"
    }.foreach(println)
  }

  def run = {
    implicit object toHListB extends ToHList[Boolean] {
      type Out = Boolean :: HNil
      def apply(t: Boolean): Boolean :: HNil = t :: HNil
    }
    println("Not")
    show(truthTable(Not _))
    println("Nand")
    show(truthTable(NAnd _))
    println("And")
    show(truthTable(And _))
    println("Or")
    show(truthTable(Or _))
    println("NOr")
    show(truthTable(NOr _))
    println("XOr")
    show(truthTable(XOr _))
    println("Mux")
    show(truthTable(Mux _))
    println("DMux")
    show(truthTable(DMux _))
  }
}

object GateHelpers {
  import shapeless._
  import shapeless.ops.hlist._
  import shapeless.ops.function._
  import shapeless.ops.nat._
  import shapeless.ops.product._

  def truthTable[In <: HList, N <: Nat, Fn, Out, OutL <: HList](f: Fn)(
    implicit
    ev1: FnToProduct.Aux[Fn, In => Out],
    ev2: ToList[In, Boolean],
    ev3: Length.Aux[In, N],
    ev4: ToInt[N],
    ev5: ToHList.Aux[Out, OutL],
    ev6: ToList[OutL, Boolean]
  ): List[(List[Boolean], List[Boolean])] = {
    gateInputs(ev4())
      .map(_.foldRight(HNil: HList)(_ :: _).asInstanceOf[In])
      .map(ins => ev2(ins) -> ev5(ev1(f)(ins)).toList)
  }

  def gateInputs(n: Int): List[List[Boolean]] = {
    val ins = List(true, false)
    List.fill(n - 1)(ins)
      .foldLeft(ins.map(List(_))) { (acc: List[List[Boolean]], in: List[Boolean]) =>
        in.flatMap(i => acc.map(i :: _))
      }
  }

}

