package rxvl.ntt

import shapeless.Nat
import shapeless.ops.nat.ToInt

import scalaz.Functor
import scalaz.Zip
import scalaz.Unzip
import scalaz.Foldable
import scalaz.Monoid
import scalaz.syntax.foldable._
import scalaz.std.vector._

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

}

