package rxvl.ntt

import Gates._
import shapeless.nat._
import scalaz._
import scalaz.Scalaz._

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
  // if (f==1)  set out = x + y  if (f==0)  set out = x & y
  // if (no==1) set out = ~out   // bitwise "not"



  import ArithematicGates._

  type Zero = Boolean
  type Negative = Boolean
  type Out = Word[_16, Boolean]

  def apply(
    x: Word[_16, Boolean],
    y: Word[_16, Boolean],
    zx: Boolean, // if (zx==1) set x = 0
    nx: Boolean, // if (nx==1) set x = ~x
    zy: Boolean, // if (zy==1) set y = 0
    ny: Boolean, // if (ny==1) set y = ~y
    f: Boolean, // if (f==1)  x + y else x & y
    no: Boolean // if (no==1) set out = ~out
  ): (Out, Zero, Negative) = {
    // if (out==0) zr = 1 if (out<0) set ng = 1
    val x1 = Mux16(Word[_16](zx), x, Word[_16](false))
    val y1 = Mux16(Word[_16](zy), y, Word[_16](false))

    val notx1 = Not16(x1)
    val x2 = Mux16(Word[_16](nx), x1, notx1)
    val noty1 = Not16(y1)
    val y2 = Mux16(Word[_16](ny), y1, noty1)

    val xandy = And16(x2, y2)
    val xplusy = Add16(x2, y2)
    val xy = Mux16(Word[_16](f), xandy, xplusy)

    val notxy = Not16(xy)
    val out = Mux16(Word[_16](no), xy, notxy)

    val (first, second) = out.bits.splitAt(out.length/2)
    val zr = Or(
      Or8Way(Word[_8, Boolean](first)),
      Or8Way(Word[_8, Boolean](second))
    )

    val ng = out.bits.last
    (out, zr, ng)
  }

  sealed trait Op
  case object Plus extends Op
  case object XMinusY extends Op
  case object YMinusX extends Op
  case object Zero extends Op
  case object One extends Op
  case object MinusOne extends Op
  case object JustX extends Op
  case object JustY extends Op
  case object JustNegX extends Op
  case object JustNegY extends Op
  case object JustNotX extends Op
  case object JustNotY extends Op
  case object IncX extends Op
  case object IncY extends Op
  case object DecX extends Op
  case object DecY extends Op
  case object AndOp extends Op
  case object OrOp extends Op


  private def test( op: Op, x: Word[ shapeless.nat._16, Zero ], y: Word[ shapeless.nat._16, Zero ] ) = {
    println(x)
    println(y)
    val (out, _, _) = op match {
      case Zero =>     apply( x, y, zx = true, nx = false, zy = true, ny = false, f = true, no = false )
      case One =>      apply( x, y, zx = true, nx = true, zy = true, ny = true, f = true, no = true )
      case MinusOne => apply( x, y, zx = true, nx = true, zy = true, ny = false, f = true, no = false )
      case JustX =>    apply( x, y, zx = false, nx = false, zy = true, ny = true, f = false, no = false )
      case JustY =>    apply( x, y, zx = true, nx = true, zy = false, ny = false, f = false, no = false )
      case JustNotX => apply( x, y, zx = false, nx = true, zy = true, ny = true, f = false, no = false )
      case JustNotY => apply( x, y, zx = false, nx = false, zy = true, ny = true, f = false, no = true )
      case JustNegX => apply( x, y, zx = true, nx = true, zy = true, ny = true, f = false, no = true )
      case JustNegY => apply( x, y, zx = true, nx = true, zy = false, ny = false, f = false, no = true )
      case IncX =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
      case IncY =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
      case DecX =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
      case DecY =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
      case Plus =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = true, no = false )
      case XMinusY =>  apply( x, y, zx = false, nx = true, zy = false, ny = false, f = true, no = true )
      case YMinusX =>  apply( x, y, zx = false, nx = false, zy = false, ny = true, f = true, no = true )
      case AndOp =>    apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
      case OrOp =>     apply( x, y, zx = false, nx = false, zy = false, ny = false, f = false, no = false )
    }
    println(Vector.fill(16)('-').mkString)
    println(out)
    out
  }

  def testB(op: Op)( xStr: String, yStr: String ): String =
    test( op, toBin( xStr ), toBin( yStr ) ).toString

  def testD(op: Op)(xInt: Int, yInt: Int): Int =
    fromBin( test( op, toBin( xInt ), toBin( yInt ) ) )


}
