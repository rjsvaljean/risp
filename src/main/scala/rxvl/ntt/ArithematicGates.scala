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
    Word[_16, Boolean](bin(i).take(16).toVector.padTo(16, false).reverse)
  }
  def fromBin(in: Word[_16, Boolean]): Int = {
    in.foldRight((0, 0)) {
      case (true, (pos, acc)) => (pos + 1, acc + Math.pow(2, pos).toInt)
      case (false, (pos, acc)) => (pos + 1, acc)
    }._2
  }

}

object ALU {
  // This file is part of www.nand2tetris.org
  // and the book "The Elements of Computing Systems"
  // by Nisan and Schocken, MIT Press.
  // File name: projects/02/ALU.hdl

  /**
    * The ALU. Computes one of the following functions:
    * x+y, x-y, y-x, 0, 1, -1, x, y, -x, -y, !x, !y,
    * x+1, y+1, x-1, y-1, x&y, x|y on two 16-bit inputs,
    * according to 6 input bits denoted zx,nx,zy,ny,f,no.
    * The bit-combinations that yield each function are
    * documented in the book. In addition, the ALU
    * computes two 1-bit outputs: if the ALU output
    * is 0, zr is set to 1; otherwise zr is set to 0;
    * If out<0, ng is set to 1; otherwise ng is set to 0.
    */

  // Implementation: the ALU manipulates the x and y
  // inputs and then operates on the resulting values,
  // as follows:
  // if (zx==1) set x = 0        // 16-bit constant
  // if (nx==1) set x = ~x       // bitwise "not"
  // if (zy==1) set y = 0        // 16-bit constant
  // if (ny==1) set y = ~y       // bitwise "not"
  // if (f==1)  set out = x + y  // integer 2's complement addition
  // if (f==0)  set out = x & y  // bitwise "and"
  // if (no==1) set out = ~out   // bitwise "not"
  // if (out==0) set zr = 1
  // if (out<0) set ng = 1


//  CHIP ALU {
//    IN
//    x[16], y[16],  // 16-bit inputs
//    zx, // zero the x input?
//    nx, // negate the x input?
//    zy, // zero the y input?
//    ny, // negate the y input?
//    f,  // compute  out = x + y (if 1) or out = x & y (if 0)
//    no; // negate the out output?
//
//    OUT
//    out[16], // 16-bit output
//    zr, // 1 if (out==0), 0 otherwise
//    ng; // 1 if (out<0),  0 otherwise
//

  def apply(
    x: Word[_16, Boolean],
    y: Word[_16, Boolean],
    zx: Boolean,
    nx: Boolean,
    zy: Boolean,
    ny: Boolean,
    f: Boolean,
    no: Boolean
  ): (Word[_16, Boolean], Boolean, Boolean) = {

    ???
  }

}