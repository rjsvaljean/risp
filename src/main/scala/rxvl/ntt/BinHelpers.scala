package rxvl.ntt

import rxvl.ntt.Memory.{ Byte16, Address }
import shapeless.nat._

import scalaz.syntax.foldable._

object BinHelpers {

  def toBin(i: Int): Byte16 = {
    def bin(j: Int): Stream[Boolean] = {
      val x = j / 2
      val remainder = j - (x * 2)
      if (j == j / 2) Stream.empty
      else (if (remainder == 1) true else false) #:: bin(x)
    }
    val n = if (i < 0) Math.pow(2, 16).toInt - Math.abs(i) else i
    Word[_16, Boolean](bin(n).take(16).toVector.padTo(16, false).reverse)
  }

  def toAddr(i: Int): Address = {
    def bin(j: Int): Stream[Boolean] = {
      val x = j / 2
      val remainder = j - (x * 2)
      if (j == j / 2) Stream.empty
      else (if (remainder == 1) true else false) #:: bin(x)
    }
    val n = if (i < 0) Math.pow(2, 15).toInt - Math.abs(i) else i
    Word[_15, Boolean](bin(n).take(15).toVector.padTo(15, false).reverse)
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
    val unsigned = in.foldRight((0, 0)) {
      case (true, (pos, acc)) => (pos + 1, acc + Math.pow(2, pos).toInt)
      case (false, (pos, acc)) => (pos + 1, acc)
    }._2
    if (unsigned > Math.pow(2, 15))
      unsigned - Math.pow(2, 16).toInt
    else unsigned
  }
  def fromBin(in: Vector[Boolean]): Int = {
    in.foldRight((0, 0)) {
      case (true, (pos, acc)) => (pos + 1, acc + Math.pow(2, pos).toInt)
      case (false, (pos, acc)) => (pos + 1, acc)
    }._2
  }

}
