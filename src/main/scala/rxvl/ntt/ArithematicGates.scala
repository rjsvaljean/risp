package rxvl.ntt

import Gates._
import shapeless.nat._
import scalaz._
import scalaz.Scalaz._

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
    val (carry1, sum1) = HalfAdder( a, b )
    val (carry2, sum2) = HalfAdder( c, sum1)
    (Or( carry2, carry1), sum2)
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


  def showAdderTruthTables = {
    RunGates.show(
      GateHelpers.truthTable(HalfAdder _)
    )
    RunGates.show(
      GateHelpers.truthTable(FullAdder _)
    )
  }

  def testAdd(i: Int, j: Int) = fromBin(Add16(toBin(i), toBin(j)))
  def testInc(i: Int) = fromBin(Inc16(toBin(i)))

  def toBin(i: Int): Word[_16, Boolean] = {
    def bin(j: Int): Stream[Boolean] = {
      val x = j / 2
      val remainder = j - ( x * 2 )
      if (j == j/2) Stream.empty
      else (if (remainder == 1) true else false) #:: bin(x)
    }
    val n = if (i < 0) Math.pow(2, 16).toInt - Math.abs(i) else i
    Word[_16, Boolean](bin(n).take(16).toVector.padTo(16, false).reverse)
  }
  def toBin(i: String): Word[_16, Boolean] = {
    Word[_16, Boolean](
      i.toVector.reverse.take(16).padTo(16, '0').reverse.map {
        case '0' => false
        case '1' => true
      }
    )
  }
  def fromBin(in: Word[_16, Boolean]): Int = {
    val unsigned = in.foldRight( (0, 0) ) {
      case (true, (pos, acc)) => (pos + 1, acc + Math.pow( 2, pos ).toInt)
      case (false, (pos, acc)) => (pos + 1, acc)
    }._2
    if (unsigned > Math.pow(2, 15))
      unsigned - Math.pow(2, 16).toInt
    else unsigned
  }
  def fromBin(in: Vector[Boolean]): Int = {
    in.foldRight( (0, 0) ) {
      case (true, (pos, acc)) => (pos + 1, acc + Math.pow( 2, pos ).toInt)
      case (false, (pos, acc)) => (pos + 1, acc)
    }._2
  }

  def testBin(n: Int) = fromBin(toBin(n))

}

